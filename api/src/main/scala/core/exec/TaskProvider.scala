package core.exec

trait TaskProvider {
  def provide: Option[Task]
}
