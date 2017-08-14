package core.exec

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import scala.concurrent.{Future, Promise}
import scala.util.Try

class TaskManager(taskSolver: TaskSolver, numWorkers: Int) extends TaskProvider with TaskExecutor {

  private val ref = this

  private val taskQueue = new java.util.concurrent.LinkedBlockingQueue[Task](100)

  private val executor: ExecutorService = init()

  def init(): ExecutorService = {
    val exec = Executors.newCachedThreadPool()
    for (i <- 0 until numWorkers) {
      exec.submit(new Worker(ref, taskSolver))
    }
    exec
  }

  def shutdown(): Unit = {
    for (i <- 0 until numWorkers) {
      println(taskQueue.offer(TerminationTask()))
    }
    executor.shutdown()
  }

  override def send(task: Task): Future[TaskResult] = {
    val promise = Promise[TaskResult]()
    val onFinish = (tr: TaskResult) => {
      val _ = promise.complete(Try(tr))
    }
    val cbTask = TaskWithCallback(task, Callback(onFinish, onFinish))
    taskQueue.offer(cbTask)
    promise.future
  }

  override def sendWithCallback(task: Task, onSuccess: TaskResult => Unit, onFailure: TaskResult => Unit) =
    send(TaskWithCallback(task, Callback(onSuccess, onFailure)))

  override def provide: Option[Task] = {
    val task = taskQueue.poll(100, TimeUnit.MILLISECONDS)
    Option(task)
  }

}
