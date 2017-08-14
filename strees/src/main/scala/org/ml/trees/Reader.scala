package org.ml.trees

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec
import scala.collection.mutable


object Reader {

  def fromCsv(path: String, hasHeaders: Boolean = false): Dataset[Double, Int] = {
    val (_, lines) = readCsv(path, true)
    val m = toMatirx(lines)
    val (features, labels) = (m(::, 1 to 13), m(::, 14).map(_.toInt))
    new Dataset(features, labels, 13, 2, labels.length)
  }

  def toClass(vector: DenseVector[Double]): (DenseVector[Double], Map[Double, Double]) = {
    val classMap = mutable.HashMap[Double, Double]()
    val newVector = DenseVector.zeros[Double](vector.length)
    var classInd = -1F
    for (i <- 0 until vector.length) {
      classMap.get(vector(i)) match {
        case Some(value) => newVector(i) = value
        case None => classInd += 1; classMap.put(vector(i), classInd); newVector(i) = classInd
      }
    }
    (newVector, classMap.toMap)
  }

  def toMatirx(lines: List[List[String]]): DenseMatrix[Double] = {
    val matr = DenseMatrix.zeros[Double](lines.length, lines.head.length)
    @tailrec
    def processRow(lines: List[List[String]], ind: Int): Unit = lines match {
      case h :: t => matr(ind, ::) := toFloatVector(h).t; processRow(t, ind + 1)
      case Nil =>
    }
    processRow(lines, 0)
    matr
  }

  def toFloatVector(vals: List[String]): DenseVector[Double] = {
    val vector = DenseVector.zeros[Double](vals.length)
    for (i <- vals.indices) {
      vector(i) = vals(i).toDouble
    }
    vector
  }


  def readCsv(path: String, hasHeaders: Boolean = false): (List[String], List[List[String]]) = {
    val lines = scala.io.Source.fromFile(path).getLines()
    val headers = if (hasHeaders) parseLine(lines.next()) else List[String]()
    (headers, lines.toList.map(parseLine))
  }

  def parseLine(line: String): List[String] = line.split(",").map(value => value.trim.toLowerCase).toList
}
