package org.saddle.framework

import org.saddle._
import org.saddle.Vec
import org.specs2.matcher._
import scala.reflect.ClassTag

/**
  * A matcher for two numeric Vecs that must be equal to within
  * a proportional tolerance
  */
class BeCloseToVecProportional[T: NUM: ClassTag](v: Vec[T], delta: T)
    extends Matcher[Vec[T]] {
  def apply[S <: Vec[T]](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]
    result(
      v.length == 0 || {
        val res = v.toSeq.zipWithIndex map {
          case (n, i) =>
            num.lteqv(
              num.minus(n, num.abs(num.times(delta, n))),
              x.value.raw(i)
            ) &&
              num
                .lteqv(
                  x.value.raw(i),
                  num.plus(n, num.abs(num.times(delta, n)))
                )
        }
        Vec(res: _*).all
      },
      " are close +/- " + delta,
      " are close +/- " + delta,
      x
    )
  }
}

object BeCloseToVecProportional {
  def apply[T: Numeric: ClassTag](v: Vec[T], delta: T) =
    new BeCloseToVecProportional[T](v, delta)
}
