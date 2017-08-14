package core.exec

class Worker(taskProvider: TaskProvider, taskSolver: TaskSolver) extends Runnable {

  @volatile
  private var stopped = false

  override def run(): Unit = {
    while (!stopped && !Thread.currentThread().isInterrupted) {
      try {
        taskProvider.provide match {
          case Some(task) =>
            exec(task)
          case None =>
        }
      }
      catch {
        case e: InterruptedException => println("Interrupted!")
      }
    }
    if (Thread.currentThread().isInterrupted) {
      println("finishing job cause was interrupted")
    }
  }

  private def exec(task: Task): Unit = {
    task match {
      case TerminationTask() =>
        println("received termination task, stopping work")
        stop()
        Thread.currentThread().interrupt()
      case _ => taskSolver.executeTask(task)
    }
  }

  def stop(): Unit = {
    stopped = true
    Thread.currentThread().interrupt()
  }

}
