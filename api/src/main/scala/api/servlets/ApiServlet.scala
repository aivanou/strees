package api.servlets

import core.exec.{Classify, TaskManager}
import io.circe.{Decoder, Encoder}
import org.scalatra._
import io.circe.generic._
import io.circe.generic.semiauto._
import org.ml.trees.Dataset

import scala.concurrent.{ExecutionContext, Promise}

case class Features(data: Array[Array[Double]], iter: Int)

case class ParsedData(features: Array[Array[Double]], labels: Array[Int], iter: Int = 200)

class ApiServlet(tm: TaskManager)
  extends ScalatraServlet
    with MethodOverride
    with FutureSupport {

  /**
    * post - create
    *
    * post - update(dataset, maxIters)
    *
    * get - predict -- {data,cId} -> result
    *
    * report - cid -> model
    * {data:[
    * [label,f1,f2,...fn]
    * ]}
    *
    **/

  implicit val featureDecoder: Decoder[Features] = deriveDecoder[Features]
  implicit val featureEncoder: Encoder[Features] = deriveEncoder[Features]


  post("/classify") {
    processRequest(request.body, "")
  }

  post("/classif/:cid") {
    val cid = params("cid")
    processRequest(request.body, cid)
  }

  def processRequest(body: String, cid: String): Any = {
    val decoder = Decoder[Features]
    val ds: Either[ActionResult, ParsedData] = io.circe.parser.decode(body)(decoder) match {
      case Left(error) =>
        Left(BadRequest("Bad request. Use format: {'data':[[label,f1,f2..]]}"))
      case Right(data) =>
        split(data) match {
          case Left(errorMsg) =>
            Left(BadRequest(errorMsg))
          case Right(pasedData) =>
            Right(pasedData)
        }
    }
    ds match {
      case Left(response) => response
      case Right(pdata) =>
        val dataset = Dataset(pdata.features, pdata.labels)
        val resp = tm.send(Classify(dataset, pdata.iter, cid))
        new AsyncResult {
          val is = resp
        }
    }
  }

  def split(f: Features): Either[String, ParsedData] = {
    if (f.data.isEmpty) {
      return Left("empty dataset")
    }
    val size = f.data.length
    val nfeatures = f.data.head.length - 1
    if (nfeatures <= 0) {
      return Left("empty feature set")
    }
    val features = new Array[Array[Double]](size)
    val labels = new Array[Int](size)
    for (i <- 0 until size) {
      if (f.data(i).length != nfeatures + 1) {
        return Left(s"Row $i does not conform to the first row")
      }
      val label = f.data(i)(0)
      val featureVector = f.data(i).drop(1)
      features(i) = featureVector
    }
    Right(ParsedData(features, labels))
  }

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