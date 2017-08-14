import javax.servlet.ServletContext

import api.servlets.ApiServlet
import core.exec.{ModelManager, TaskManager}
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    val classifer = new ModelManager()
    val tm = new TaskManager(classifer, 4)
    context mount(new ApiServlet(tm), "/*")
  }
}