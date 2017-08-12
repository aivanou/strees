package org.ml.trees

import scala.annotation.tailrec
import scala.collection._

case class Bin(value: Double, count: Int)

sealed trait Histogram {

  def update(point: Double): Unit

  def rightSum(point: Double): Double

  def leftSum(point: Double): Double

  def uniform(point: Int): List[Double]

  def npoints: Int

}

/**
  * Represents the histogram of points
  *
  * @param binSeq the sequence of bins
  * @param maxBins the max amount of bins
  * @param _bounds
  */
class BoundHistogram(binSeq: Seq[Bin], maxBins: Int, _bounds: (Double, Double)) extends Histogram {

  private val hist = mutable.TreeMap[Double, Bin]()((p1: Double, p2: Double) => java.lang.Double.compare(p1, p2))

  _init()

  def leftBound = _bounds._1

  def rightBound = _bounds._2

  private def _init(): Unit = {
    binSeq.foreach(p => hist.put(p.value, p))
    //    hist.put(leftBound, Bin(leftBound, 0))
    //    hist.put(rightBound, Bin(leftBound, 0))
    reduceToMaxSize
  }

  def getMaxBins: Int = maxBins

  def update(newBin: Bin) = hist.get(newBin.value) match {
    case Some(bin) => hist.put(bin.value, Bin(bin.value, bin.count + newBin.count))
    case None =>
      hist.put(newBin.value, newBin)
      if (hist.size > maxBins) {
        reduce
      }
  }

  def bins: Iterable[Bin] = hist.values

  override def npoints: Int = bins.map(_.count).sum

  override def update(point: Double): Unit = update(Bin(point, 1))

  override def rightSum(point: Double): Double = npoints - leftSum(point)

  override def leftSum(point: Double): Double = {
    val ans = leftSum(point, 0.0, bins.head, bins.tail)
    ans
  }

  @tailrec
  private def leftSum(point: Double, count: Double, pbin: Bin, bins: Iterable[Bin]): Double =
    bins match {
      case Nil => count
      case h :: t => if (h.value <= point) leftSum(point, count + h.count, h, t)
      else {
        val cbin = h
        val mb = pbin.count + (cbin.count - pbin.count) * (point - pbin.value) / (cbin.value - pbin.value)
        val s =
          if (cbin.value == pbin.value) 0
          else (pbin.count + mb) / 2 * (point - pbin.value) / (cbin.value - pbin.value)
        count - pbin.count / 2 + s
      }
    }

  override def uniform(point: Int): List[Double] = {
    val pointSet = mutable.HashSet[Double]()
    val npoints = bins.map(_.count).sum
    val sums = preComputeSums
    for (i <- 1 until point) {
      compute(sums, i, point, npoints) match {
        case Some(point) =>
          if (point < hist.head._1) {
            pointSet.add(hist.head._1)
          } else if (point > hist.last._1) {
            pointSet.add(hist.last._1)
          } else {
            pointSet.add(point)
          }
        case None => None
      }
    }
    val pnts = pointSet.toList.sorted
    pnts
  }

  def preComputeSums: List[(Bin, Double)] = {
    val lst = mutable.ListBuffer[(Bin, Double)]()
    for (bin <- bins) {
      lst += ((bin, leftSum(bin.value)))
    }
    lst.toList
  }

  def compute(sums: List[(Bin, Double)], i: Int, point: Double, npoints: Int): Option[Double] = {
    val s = i.toFloat / point * npoints
    compute(findBetween(sums, s), s)
  }

  def compute(pair: (Option[Bin], Option[Bin]), s: Double): Option[Double] = pair match {
    case (Some(prevBin), Some(currBin)) =>
      val pr = z(computeParams(prevBin, currBin, s))
      Some(prevBin.value + (currBin.value - prevBin.value) * pr)
    case (Some(prevBin), None) => Some(prevBin.value)
    case (None, Some(cbin)) => Some(cbin.value)
    case (None, None) => None
  }

  def z(params: (Double, Double, Double)): Double = params match {
    case (0, b, c) => -c / b
    case (a, 0, c) => Math.sqrt(-c / a)
    case (a, b, c) if 4 * a * c > b * b => 0
    case (a, b, c) => (-b + Math.sqrt(b * b - 4 * a * c)) / (2 * a)
  }

  def computeParams(prevBin: Bin, currBin: Bin, s: Double): (Double, Double, Double) =
    (Math.abs(currBin.count - prevBin.count).toDouble, 2 * prevBin.count.toDouble, -2 * (s - leftSum(prevBin.value)))

  private def findBetween(sums: List[(Bin, Double)],
                          s: Double): (Option[Bin], Option[Bin]) = findBetween(s, sums, (None, None))

  @tailrec
  private def findBetween(s: Double, preComputedSums: List[(Bin, Double)],
                          pair: (Option[Bin], Option[Bin])): (Option[Bin], Option[Bin]) = preComputedSums match {
    case prevPair :: currPair :: t =>
      val (p1, p2) = (prevPair._2, currPair._2)
      if (s > p1 && s < p2) {
        (Some(prevPair._1), Some(currPair._1))
      } else {
        findBetween(s, currPair :: t, pair)
      }
    case currPair :: Nil =>
      val sum = leftSum(currPair._1.value)
      if (s >= sum) (None, Some(currPair._1))
      else (Some(currPair._1), None)
    case Nil => pair
  }

  def print = println(hist)

  def reduceToMaxSize = while (hist.size > maxBins) reduce

  private def reduce = {
    val arr = bins
    val (mbin1, mbin2) = findBinsToReduce(arr.tail.tail, (arr.head, arr.tail.head))

    hist.remove(mbin1.value)
    hist.remove(mbin2.value)
    val binPoint = (mbin1.value * mbin1.count + mbin2.value * mbin2.count) / (mbin1.count + mbin2.count)
    hist.put(binPoint, Bin(binPoint, mbin1.count + mbin2.count))
  }

  @tailrec
  private def findBinsToReduce(bins: Iterable[Bin], pair: (Bin, Bin)): (Bin, Bin) =
    bins match {
      case prevBin :: cbin :: t => {
        if (Math.abs(pair._1.value - pair._2.value) > Math.abs(prevBin.value - cbin.value)) {
          findBinsToReduce(cbin :: t, (prevBin, cbin))
        } else {
          findBinsToReduce(cbin :: t, pair)
        }
      }
      case _ => pair
    }
}

object BoundHistogram {

  def apply(bins: Seq[Bin], maxPoints: Int = 200): BoundHistogram = new BoundHistogram(bins, maxPoints, (0, 0))

  def fromBin(bin: Bin): BoundHistogram = BoundHistogram(List(bin))

  def fromPoint(point: Double): BoundHistogram = BoundHistogram.fromBin(Bin(point, 1))

  def merge(h1: BoundHistogram, h2: BoundHistogram, maxPoints: Int = 200): BoundHistogram = {
    val newHist = BoundHistogram(h1.bins.toList, maxPoints)
    h2.bins.foreach(bin => newHist.update(bin))
    newHist.reduceToMaxSize
    newHist
  }

  def sum(h: BoundHistogram, p: Double): Double = h.leftSum(p)
}
