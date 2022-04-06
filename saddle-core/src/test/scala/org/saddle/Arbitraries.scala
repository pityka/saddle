package org.saddle

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Arbitraries {
  implicit val fillArbitrary: Arbitrary[FillMethod] = Arbitrary(
    Gen.oneOf(FillForward, FillBackward)
  )
}
