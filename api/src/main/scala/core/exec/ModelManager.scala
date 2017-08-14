package core.exec

import org.ml.trees.{Classifier, Dataset, StreamingDecisionTree}

trait TaskSolver {
  def executeTask(task: Task): TaskResult
}

class ModelManager extends TaskSolver {

  private val classifiers = scala.collection.mutable.HashMap[String, Classifier]()

  def executeTask(task: Task): TaskResult = task match {
    case Classify(ds: Dataset[Double, Int], maxIters: Int, classifierId: String) =>
      val oclassifier = classifiers.get(classifierId)
      if (oclassifier.isEmpty) {
        classifiers.synchronized {
          if (classifiers.get(classifierId).isEmpty) {
            classifiers.put(classifierId, StreamingDecisionTree())
          }
        }
      }
      classifiers.get(classifierId) match {
        case Some(c) =>
          c.train(ds, maxIters)
          ClassifyResult("trained successfully")
        case None => ClassifyResult("something is wrong. the classified is not found. retry your request.")
      }
    case TaskWithCallback(innerTask, cb) =>
      val res = executeTask(innerTask)
      cb.onSuccess(res)
      res
  }

}
