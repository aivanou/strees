package core.exec

import scala.concurrent.Future

trait TaskExecutor {
  def send(task: Task): Future[TaskResult]

  def sendWithCallback(task: Task, onSuccess: TaskResult => Unit, onFailure: TaskResult => Unit)
}
