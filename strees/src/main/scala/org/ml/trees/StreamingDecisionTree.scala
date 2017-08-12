package org.ml.trees

import java.util.concurrent.TimeUnit

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.mutable.ListBuffer
import scala.collection.{Map, mutable, _}
import scala.concurrent.{Await, Future}

import scala.concurrent.ExecutionContext.Implicits.global

case class Split(start: Int, end: Int)

case class Dataset(features: DenseMatrix[Double], labels: DenseVector[Int], nfeatures: Int, nlabels: Int, size: Int)

/**
  * TODO: unlabeledLeafs -- make safe
  *
  * @param dt
  * @param impurity
  */
class SPDT(dt: DecisionTree, impurity: Impurity) {

  def trainParallel(dataset: Dataset, ngroups: Int, maxIter: Int = 200): Unit = {
    val groups = split(ngroups, dataset.size)
    var iter = 0
    while (!dt.converged && iter < maxIter) {
      iter += 1
      Timer.start("compress.group")
      val lst = groups.par.map(g => compressGroup(dataset, g)).toList
      Timer.stop("compress.group")
      Timer.start("compress.merge")
      val mergedLeafs = mergeLeafs(dataset, lst)
      Timer.stop("compress.merge")
      println("Will process unlabeled leafs: ", mergedLeafs.size)
      println(mergedLeafs.size, dt.cnt)
      if (mergedLeafs.isEmpty) return
      (for ((leaf, distr) <- mergedLeafs)
        yield Future[(Option[UnlabeledLeaf], Int, Double)] {
          computeSplit(leaf, distr)
        })
        .map(ft => Await.result(ft, scala.concurrent.duration.Duration(500, TimeUnit.MILLISECONDS)))
        .map({
          case (Some(leaf), sa, sv) => dt.grow(leaf, sa, sv); true
          case (None, _, _) => false
        })
    }

  }

  def train(dataset: Dataset, ngroups: Int, maxIters: Int = 100): Unit = {
    val groups = split(ngroups, dataset.size)
    var iter = 0
    while (iter < maxIters) {
      iter += 1
      Timer.start("compress.group")
      val lst = groups.par.map(g => compressGroup(dataset, g)).toList
      Timer.stop("compress.group")
      Timer.start("compress.merge")
      val mergedLeafs = mergeLeafs(dataset, lst)
      Timer.stop("compress.merge")
      println("Will process unlabeled leafs: ", mergedLeafs.size)
      if (mergedLeafs.isEmpty) return
      for ((leaf, distr) <- mergedLeafs) {
        val (optLeaf, sa, sv) = computeSplit(leaf, distr)
        optLeaf match {
          case Some(leaf) => dt.grow(leaf, sa, sv); true
          case None => false
        }
      }
    }

  }

  def computeSplit(leaf: UnlabeledLeaf, distr: Distribution): (Option[UnlabeledLeaf], Int, Double) = {
    val leafImpurity = impurity.compute(computeNodeProbs(distr))
    if (!dt.tryLabel(leaf, leafImpurity, distr.labelDistribution())) {
      val (maxInfoGain, splitAttr, splitValue) = findMaxInfoGain(distr)
      (Some(leaf), splitAttr, splitValue)
    } else {
      (None, 0, 0)
    }
  }

  def findMaxInfoGain(distr: Distribution): (Double, Int, Double) = {
    val attrGrouped = distr.mergeByFeature()
    var maxInfoGain = Double.MinValue
    var mAttrInd = -1
    var splitValue = Double.MinValue
    val leafImpurity = impurity.compute(distr.pointsFractionByLabels)
    for ((attrInd, hist) <- attrGrouped) {
      val (currInfoGain, cSplitPoint) = findInfoGainForAttribute(hist, leafImpurity, distr, attrInd)
      if (currInfoGain > maxInfoGain) {
        maxInfoGain = currInfoGain
        mAttrInd = attrInd
        splitValue = cSplitPoint
      }
    }
    //    println(s"Max info gain: $maxInfoGain, attrIndSplit: $mAttrInd, split value: $splitValue")
    (maxInfoGain, mAttrInd, splitValue)
  }

  def findInfoGainForAttribute(hist: BoundHistogram, leafImpurity: Double,
                               distr: Distribution, featureId: Int): (Double, Double) = {
    Timer.start("hist.uniform")
    val points = hist.uniform(hist.bins.size)
    Timer.stop("hist.uniform")
    var maxInfoGain = Double.MinValue
    var mPoint = 0.0
    val npoints = hist.npoints
    for (point <- points) {
      val (lfract, rfract) = distr.findFractionsAfterSplit(featureId, point)
      val (lprob, rprob) = (impurity.compute(lfract), impurity.compute(rfract))
      val currInfoGain = leafImpurity - hist.leftSum(point) / npoints * lprob - (1 - hist.leftSum(point) / npoints) * rprob
      if (currInfoGain > maxInfoGain) {
        maxInfoGain = currInfoGain
        mPoint = point
      }
    }
    (maxInfoGain, mPoint)
  }

  def computeNodeProbs(distr: Distribution): List[Double] = distr.pointsFractionByLabels

  def split(ngroups: Int, nlen: Int): List[Split] = {
    val splits = ListBuffer[Split]()
    val elsPerGroup = nlen / ngroups
    for (group <- 0 until ngroups - 1) {
      val split = Split(group * elsPerGroup, (group + 1) * elsPerGroup - 1)
      splits += split
    }
    splits += Split((ngroups - 1) * elsPerGroup, nlen - 1)
    splits.toList
  }

  def mergeLeafs(dataset: Dataset, lst: List[Map[UnlabeledLeaf, Distribution]]): Map[UnlabeledLeaf, Distribution] = {
    val leafs = lst.map(mp => mp.map {
      case (leaf, _) => leaf
    }).toSet.flatten
    val grouped = mutable.HashMap[UnlabeledLeaf, Distribution]()
    for (leaf <- leafs) {
      grouped.put(leaf, mergeLeaf(dataset, lst, leaf))
    }
    grouped.toMap
  }

  def mergeLeaf(dataset: Dataset, lst: List[Map[UnlabeledLeaf, Distribution]], leaf: UnlabeledLeaf): Distribution = {
    var distr = Distribution.empty(dataset.nlabels, dataset.nfeatures)
    for (el <- lst) {
      el.get(leaf) match {
        case Some(d) =>
          Timer.start("compress.merge.distr")
          distr = Distribution.merge(distr, d)
          Timer.stop("compress.merge.distr")
        case None =>
      }
    }
    distr
  }


  /**
    * Instead of working directly with the whole data set, we use the concept of histograms:
    *
    * @param dataset
    * @param split
    * @return
    */
  def compressGroup(dataset: Dataset, split: Split): Map[UnlabeledLeaf, Distribution] = {
    val leafInds = mutable.HashMap[UnlabeledLeaf, ListBuffer[Int]]()
    for (ind <- split.start to split.end) {
      dt.fit(dataset.features(ind, ::).inner) match {
        case labeledLeaf: LabeledLeaf =>
        case leaf: UnlabeledLeaf => leafInds.get(leaf) match {
          case Some(bf) => bf += ind
          case None => leafInds.put(leaf, ListBuffer[Int](ind))
        }
      }
    }
    val groupedMp = mutable.HashMap[UnlabeledLeaf, Distribution]()
    for ((leaf, group) <- leafInds) {
      val distr = compressLeaf(dataset, group)
      groupedMp.put(leaf, distr)
    }
    groupedMp.toMap
  }

  /** *
    *
    * @param group - indices of the dataset that belong to the group
    * @return (Int1,Int2):Hist => Int1 - label, Int2 - attrId
    */
  def compressLeaf(dataset: Dataset, group: Seq[Int]): Distribution = {
    val distr = Distribution.empty(dataset.nlabels, dataset.nfeatures)
    for (ind <- group) {
      val featureVector = dataset.features(ind, ::).inner
      for (attrInd <- 0 until featureVector.length) {
        val feature = featureVector(attrInd)
        distr.get((dataset.labels(ind), attrInd)) match {
          case Some(hist) => hist.update(feature)
          case None => distr.add((dataset.labels(ind), attrInd), feature)
        }
      }
    }
    distr
  }

}

trait Impurity {
  def compute(probs: List[Double]): Double
}

class GiniImpurity extends Impurity {
  override def compute(probs: List[Double]): Double = 1 - probs.map(p => p * p).sum
}

class EntropyImpurity extends Impurity {
  val lnOf2 = scala.math.log(2)

  // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

  override def compute(probs: List[Double]): Double = -probs.map(p => if (p == 0.0) 0.0 else p * log2(p)).sum
}


object StreamingDecisionTree {

  def main(args: Array[String]): Unit = {
    val path = "/Users/aliaksandrivanou/data/census_income/processed.csv"
    val dataset = Reader.fromCsv(path, true)
    Timer.start("split")
    val (trainSet, testSet) = Reader.split(dataset, 0.3)
    Timer.stop("split")
    println(Timer.provide("split"))
    val dt = new DecisionTree(10, 0.12)
    val spdt = new SPDT(dt, new GiniImpurity())
    println("read data, start processing")
    Timer.start("train")
    spdt.trainParallel(trainSet, 10)
    Timer.stop("train")
    println("train time: ", Timer.provide("train"))
    val e = dt.fitSet(testSet)
    println(testSet.size, e)
    println(trainSet.size, dt.fitSet(trainSet))
    println(Timer.provide("hist.uniform"))
    println(Timer.provide("compress.group"))
    println(Timer.provide("compress.merge"))
    println(Timer.provide("compress.merge.distr"))
    println(Timer.provide("hist.merge"))
  }


}
