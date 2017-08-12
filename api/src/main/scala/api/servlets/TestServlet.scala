package api.servlets

import org.scalatra._

import scala.concurrent.{ExecutionContext, Promise}

class TestServlet()
  extends ScalatraServlet
    with MethodOverride
    with FutureSupport {

  get("/classify") {
    Ok("yes")
  }

  get("/async") {
    new AsyncResult {
      val is =
        Promise.successful("yes").future
    }
  }

  override protected implicit def executor: ExecutionContext = scala.concurrent.ExecutionContext.global
}