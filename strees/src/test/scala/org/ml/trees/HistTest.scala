package org.ml.trees

import org.scalatest._

class HistogramSpec extends FlatSpec with Matchers {

  "An update histogram" should "not increase the amount of bins" in {
    val bins = List[Bin](Bin(23, 1), Bin(19, 1), Bin(10, 1), Bin(16, 1), Bin(36, 1))
    val hist = BoundHistogram(bins, 5)
    hist.bins.map(_.count).sum should be(bins.map(_.count).sum)
    hist.update(2)
    hist.update(9)
    hist.bins.map(_.count).sum should be(bins.map(_.count).sum + 2)
    hist.bins.map(_.value).zip(List[Double](2, 9.5, 17.5, 23.0, 36.0)).map(p => Math.abs(p._1 - p._2)).max should be < 0.01D
  }

  "A merge histograms" should "not increase the amount of bins" in {
    val bins1 = List[Bin](Bin(23, 1), Bin(19, 1), Bin(10, 1), Bin(16, 1), Bin(36, 1))
    val bins2 = List[Bin](Bin(32, 1), Bin(30, 1), Bin(45, 1))
    val hist = BoundHistogram.merge(BoundHistogram(bins1, 5), BoundHistogram(bins2, 3))
    hist.bins.map(_.count).sum should be(bins1.map(_.count).sum + bins2.map(_.count).sum)
    hist.bins.size == hist.getMaxBins
    hist.bins.map(_.value).zip(List[Double](10.0, 17.5, 23.0, 32.66, 45.0)).map(p => Math.abs(p._1 - p._2)).max should be < 0.01D
  }

  "A sum" should "return approximate amount of points on the interval [-inf, point]" in {
    val bins = List[Bin](Bin(2, 1), Bin(9.5, 2), Bin(19.33, 3), Bin(32.67, 3), Bin(45, 1))
    val hist = BoundHistogram(bins, 5)
    hist.leftSum(15) - 3.28 should be < 0.1D
  }

  "A sum" should "do something" in {
    val bins = List[Bin](Bin(2, 1), Bin(9.5, 2), Bin(19.33, 3), Bin(32.67, 3), Bin(45, 1))
    val hist = BoundHistogram(bins, 5)
    println(hist.leftSum(50))
    hist.leftSum(15) - 3.28 should be < 0.1D
  }

  "A union" should "distribute points evenly" in {
    val bins = List[Bin](Bin(2, 1), Bin(9.5, 2), Bin(19.33, 3), Bin(32.67, 3), Bin(45, 1))
    val hist = BoundHistogram(bins, 5)
    hist.uniform(3).zip(List(15.22, 26.74)).map(p => Math.abs(p._1 - p._2)).max should be < 0.1D
  }

  "A union" should "distribute points evenly11" in {
    val bins = List[Bin](Bin(0, 200000), Bin(2901, 2))
    val hist = BoundHistogram(bins, 5)
    println(hist.uniform(1000))
    true
    //    hist.uniform(3).zip(List(15.22, 26.74)).map(p => Math.abs(p._1 - p._2)).max should be < 0.1D
  }
}