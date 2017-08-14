package core.exec

import org.ml.trees.Dataset

sealed trait Task

case class Callback(onSuccess: (TaskResult) => Unit, onFailure: (TaskResult) => Unit)

case class Classify(ds: Dataset[Double, Int],
                    maxIters: Int, classifierId: String) extends Task

case class TaskWithCallback(task: Task, cb: Callback) extends Task

case class TerminationTask() extends Task


sealed trait TaskResult

case class ClassifyResult(msg: String) extends TaskResult