package org.saddle.util
import org.saddle.ST
trait TotalOrder[@scala.specialized A] extends cats.kernel.Order[A]  {

  /** Result of comparing `x` with `y`. Returns an Int whose sign is:
    *   - negative iff `x < y`
    *   - zero iff `x = y`
    *   - positive iff `x > y`
    *
    * nulls and missing values are ordered below all other
    */
  def compare(x: A, y: A): Int
}

object TotalOrder extends OrderInstances {
  def by[@scala.specialized A, @scala.specialized B](
      f: A => B
  )(implicit ev: TotalOrder[B]): TotalOrder[A] =
    new TotalOrder[A] {
      def compare(x: A, y: A): Int = ev.compare(f(x), f(y))
    }
  def fromCats[@scala.specialized A](co: cats.kernel.Order[A], st: ST[A]) =
    new TotalOrder[A] {
      def compare(x: A, y: A): Int = {
        def m(x: A) = st.isMissing(x)
        if (m(x) && !m(y)) -1
        else if (!m(x) && m(y)) 1
        else if (m(x) && m(y)) 0
        else co.compare(x, y)
      }
    }
}
