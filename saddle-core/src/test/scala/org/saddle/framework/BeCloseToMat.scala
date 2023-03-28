package org.saddle.framework

import org.saddle._
import org.specs2.matcher._

/** A matcher for two numeric Mats that must be equal to within a tolerance
  */
class BeCloseToMat[T: Numeric](m: Mat[T], delta: T) extends Matcher[Mat[T]] {
  def apply[S <: Mat[T]](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]

    result(
      m.length == 0 || {
        val res = m.contents.zipWithIndex map { case (n, i) =>
          num.lteqv(num.minus(n, delta), x.value.contents(i)) &&
            num.lteqv(x.value.contents(i), num.plus(n, delta))
        }
        Vec(res.toIndexedSeq: _*).all
      },
      " are close +/- " + delta,
      " are close +/- " + delta,
      x
    )
  }
}

object BeCloseToMat {
  def apply[T: Numeric](v: Mat[T], delta: T) =
    new BeCloseToMat[T](v, delta)
}
