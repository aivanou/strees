package org.ml.trees

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck._

object StringSpecification extends Properties("String") {

  val binGen = for {
    value <- Gen.choose(0.0, 10.0)(Gen.Choose.chooseDouble)
    count <- Gen.choose(0, 20)
  } yield Bin(value, count)

  implicit lazy val binArb: Arbitrary[Bin] = Arbitrary(binGen)

  implicit lazy val listBinArg: Arbitrary[List[Bin]] = Arbitrary(Gen.listOf(binGen))

  property("histCreation") = forAll { (bins: List[Bin]) =>
    val hist = BoundHistogram(bins)
    hist.getMaxBins >= hist.bins.size
    hist.bins.map(_.count).sum == bins.map(_.count).sum
  }
}