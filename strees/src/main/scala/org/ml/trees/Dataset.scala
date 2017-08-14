package org.ml.trees

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.storage.Zero

import scala.reflect.ClassTag


class Dataset[F, L](val features: DenseMatrix[F], val labels: DenseVector[L],
                    val nfeatures: Int, val nlabels: Int, val size: Int)(implicit zero: Zero[F], t: ClassTag[F], t1: ClassTag[L]) {
  def featureVector(i: Int): DenseVector[F] = features(i, ::).inner

  def split(f: Double): (Dataset[F, L], Dataset[F, L]) = {
    val rng = new java.util.Random(System.nanoTime())
    val size = labels.length
    val (trainSize, testSize) = (size - (size * f).toInt, (size * f).toInt)
    val (trainSet, testSet) = (
      Dataset.emptyDataset[F, L](trainSize, nfeatures, nlabels),
      Dataset.emptyDataset[F, L](testSize, nfeatures, nlabels))
    val testGroup = sample(testSize, size)
    var trainInd = 0
    var testInd = 0
    for (i <- 0 until size) {
      if (testGroup.contains(i)) {
        testSet.features(testInd, ::) := features(i, ::)
        testSet.labels(testInd) = labels(i)
        testInd += 1
      } else {
        trainSet.features(trainInd, ::) := features(i, ::)
        trainSet.labels.update(trainInd, labels(i))
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

}

object Dataset {

  def apply(features: Array[Array[Double]], labels: Array[Int]): Dataset[Double, Int] = {
    val nfeatures = features.head.length
    val nlabels = labels.toSet.size
    val size = labels.length
    new Dataset[Double, Int](toDenseMatrix(features), toDenseVector(labels), nfeatures, nlabels, size)
  }

  def emptyDataset[F, L](size: Int, nfeatures: Int, nlabels: Int)
                        (implicit zr: Zero[F], t: ClassTag[F], t1: ClassTag[L]): Dataset[F, L] =
    new Dataset[F, L](DenseMatrix.create(size, nfeatures, new Array[F](size * nfeatures)),
      DenseVector.create(new Array[L](size), 0, 1, size), nfeatures, nlabels, size)

  def toDenseMatrix(arr: Array[Array[Double]]): DenseMatrix[Double] = {
    val rows = arr.length
    val cols = arr.head.length
    DenseMatrix.create[Double](rows, cols, arr.flatten)
  }

  def toDenseVector(arr: Array[Int]): DenseVector[Int] = {
    DenseVector.create[Int](arr, 0, 1, arr.length)
  }
}
