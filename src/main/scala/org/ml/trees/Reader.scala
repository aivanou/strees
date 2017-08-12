package org.ml.trees

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec
import scala.collection.mutable

object Reader {

  def fromCsv(path: String, hasHeaders: Boolean = false): Dataset = {
    val (_, lines) = readCsv(path, true)
    val m = toMatirx(lines)
    val (features, labels) = (m(::, 1 to 13), m(::, 14).map(_.toInt))
    Dataset(features, labels, 13, 2, labels.length)
  }

  def toClass(vector: DenseVector[Double]): (DenseVector[Double], Map[Double, Double]) = {
    val classMap = mutable.HashMap[Double, Double]()
    val newVector = DenseVector.zeros[Double](vector.length)
    var classInd = -1F
    for (i <- 0 until vector.length) {
      classMap.get(vector(i)) match {
        case Some(value) => newVector(i) = value
        case None => classInd += 1; classMap.put(vector(i), classInd); newVector(i) = classInd
      }
    }
    (newVector, classMap.toMap)
  }

  def toMatirx(lines: List[List[String]]): DenseMatrix[Double] = {
    val matr = DenseMatrix.zeros[Double](lines.length, lines.head.length)
    @tailrec
    def processRow(lines: List[List[String]], ind: Int): Unit = lines match {
      case h :: t => matr(ind, ::) := toFloatVector(h).t; processRow(t, ind + 1)
      case Nil =>
    }
    processRow(lines, 0)
    matr
  }

  def toFloatVector(vals: List[String]): DenseVector[Double] = {
    val vector = DenseVector.zeros[Double](vals.length)
    for (i <- vals.indices) {
      vector(i) = vals(i).toDouble
    }
    vector
  }


  def readCsv(path: String, hasHeaders: Boolean = false): (List[String], List[List[String]]) = {
    val lines = scala.io.Source.fromFile(path).getLines()
    val headers = if (hasHeaders) parseLine(lines.next()) else List[String]()
    (headers, lines.toList.map(parseLine))
  }

  def parseLine(line: String): List[String] = line.split(",").map(value => value.trim.toLowerCase).toList

  def split(d: Dataset, f: Double): (Dataset, Dataset) = {
    val rng = new java.util.Random(System.nanoTime())
    val size = d.labels.length
    val (trainSize, testSize) = (size - (size * f).toInt, (size * f).toInt)
    val (trainSet, testSet) = (emptyDataset(trainSize, d.nfeatures, d.nlabels), emptyDataset(testSize, d.nfeatures, d.nlabels))
    val testGroup = sample(testSize, d.size)
    var trainInd = 0
    var testInd = 0
    for (i <- 0 until d.size) {
      if (testGroup.contains(i)) {
        testSet.features(testInd, ::) := d.features(i, ::)
        testSet.labels(testInd) = d.labels(i)
        testInd += 1
      } else {
        trainSet.features(trainInd, ::) := d.features(i, ::)
        trainSet.labels.update(trainInd, d.labels(i))
        trainInd += 1
      }
    }
    (trainSet, testSet)
  }

  def sample(nsamples: Int, size: Int): Set[Int] = {
    val samples = new Array[Int](nsamples)
    (0 until nsamples).foreach(ind => samples(ind) = ind)
    val rng = new java.util.Random(System.nanoTime())
    for (ind <- nsamples until size) {
      val j = rng.nextInt(ind)
      if (j < nsamples) {
        samples(j) = ind
      }
    }
    samples.toSet
  }

  def emptyDataset(size: Int, nfeatures: Int, nlabels: Int): Dataset =
    Dataset(DenseMatrix.create(size, nfeatures, new Array[Double](size * nfeatures)),
      DenseVector.create(new Array[Int](size), 0, 1, size), nfeatures, nlabels, size)

}
