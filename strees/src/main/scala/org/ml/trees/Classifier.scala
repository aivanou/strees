package org.ml.trees

case class ClReport()

trait Classifier {

  def train(ds: Dataset[Double, Int], maxIter: Int): ClReport

  def trainParallel(ds: Dataset[Double, Int], maxIter: Int, numThreads: Int): ClReport

}
