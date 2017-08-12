package org.ml.trees

import scala.collection.{Map, mutable}

/**
  *
  *
  * The class contains functionality for working with triple: (Label,Feature,Histogram), where
  * Label - is a label, Feature - the index of the feature, and Histogram - the distribution of
  * the dataset for corresponding (Feature,Label).
  *
  * @param nlabels   the amount of points on X axis
  * @param nfeatures the amount of points on Y axis
  */
class Distribution(nlabels: Int, nfeatures: Int) {

  /**
    * Map represents the  (Label,FeatureId -> Histogram)
    */
  private val distr = new mutable.HashMap[(Int, Int), BoundHistogram]()

  /**
    * @param pair  the pair of points(Label,FeatureId)
    * @param point The value of the FeatureId for a particular sample
    */
  def add(pair: (Int, Int), point: Double): Unit = {
    distr.get(pair) match {
      case Some(hist) => hist.update(point)
      case None => distr.put(pair, BoundHistogram.fromPoint(point))
    }
  }

  /**
    *
    * @param pair    the pair of points(Label,FeatureId)
    * @param newHist the distribution on (Label,FeatureId)
    */
  def add(pair: (Int, Int), newHist: BoundHistogram): Unit = {
    distr.get(pair) match {
      case Some(hist) =>
        hist.merge(newHist)
      //distr.put(pair, BoundHistogram.merge(hist, newHist))
      case None => distr.put(pair, newHist)
    }
  }

  def get(p: (Int, Int)): Option[BoundHistogram] = distr.get(p)

  def totalPoints: Int = distr.values.map(_.npoints).sum

  def totalSamples: Int = {
    val featureId = distr.head._1._2
    (for {label <- 0 until nlabels}
      yield distr.get((label, featureId)) match {
        case Some(hist) => hist.npoints
        case None => 0
      }).sum
  }

  def npointsBySplit(splitPoint: Double): (Double, Double) = {
    val lpoints = distr.values.map(_.leftSum(splitPoint)).sum
    (lpoints, totalPoints - lpoints)
  }

  def findFractionsAfterSplit(featureInd: Int, splitPoint: Double): (List[Double], List[Double]) = {
    val featureDistr = filterForFeature(featureInd)
    val (lFracts, rFracts) = (mutable.ListBuffer[Double](), mutable.ListBuffer[Double]())
    val (lTotalPoints, rTotalPoints) = featureDistr.npointsBySplit(splitPoint)
    for (label <- 0 until nlabels) {
      val lprob = featureDistr.get((label, featureInd)) match {
        case Some(hist) => hist.leftSum(splitPoint) / lTotalPoints
        case None => 0.0
      }
      val rprob = featureDistr.get((label, featureInd)) match {
        case Some(hist) => hist.rightSum(splitPoint) / rTotalPoints
        case None => 0.0
      }
      lFracts += lprob
      rFracts += rprob
    }
    (lFracts.toList, rFracts.toList)
  }

  def filterForFeature(featureId: Int): Distribution = {
    val d = Distribution.empty(nlabels, nfeatures)
    for (label <- 0 until nlabels) {
      distr.get((label, featureId)) match {
        case Some(h) => d.add((label, featureId), h)
        case None =>
      }
    }
    d
  }

  def mergeByFeature(): Map[Int, BoundHistogram] = groupBy(p => p._2)

  def mergeByLabel(): Map[Int, BoundHistogram] = groupBy(p => p._1)

  def pointsFractionByLabels: List[Double] = {
    val ldist = labelDistribution()
    val totalPoints = ldist.values.sum
    ((0 until nlabels) map (ind => ldist.get(ind) match {
      case Some(npoints) => npoints.toDouble / totalPoints
      case None => 0.0
    })).toList
  }

  def labelDistribution(): Map[Int, Int] = {
    val ldistr = mutable.HashMap[Int, Int]()
    val featureId = distr.keys.head._2
    for (label <- 0 until nlabels) {
      distr.get((label, featureId)) match {
        case Some(hist) => ldistr.put(label, hist.npoints)
        case None =>
      }
    }
    ldistr.toMap
  }

  def merge(d: Distribution): Unit = {
    for {
      label <- 0 until nlabels
      attrId <- 0 until nfeatures
    } d.get((label, attrId)) match {
      case Some(hist) => merge((label, attrId), hist)
      case None =>
    }
  }

  def merge(p: (Int, Int), hist: BoundHistogram): Unit = distr.get(p) match {
    case Some(myHist) =>
      Timer.start("hist.merge")
      //      distr.put(p, BoundHistogram.merge(myHist, hist))
      myHist.merge(hist)
      Timer.stop("hist.merge")
    case None => distr.put(p, hist)
  }

  private def groupBy(group: ((Int, Int)) => Int): Map[Int, BoundHistogram] = {
    val mp = mutable.HashMap[Int, BoundHistogram]()
    for ((key, attrHist) <- distr) {
      mp.get(group(key)) match {
        case Some(hist) => mp.put(group(key), BoundHistogram.merge(hist, attrHist, (hist.getMaxBins + attrHist.getMaxBins) / 2))
        case None => mp.put(group(key), attrHist)
      }
    }
    mp.toMap
  }
}

object Distribution {

  def empty(nlabels: Int, nfeatures: Int): Distribution = new Distribution(nlabels, nfeatures)

  def merge(d1: Distribution, d2: Distribution): Distribution = {
    d1.merge(d2)
    d1
  }
}
