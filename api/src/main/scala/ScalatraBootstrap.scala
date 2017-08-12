import javax.servlet.ServletContext

import api.servlets.TestServlet
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context mount(new TestServlet, "/*")
  }
}