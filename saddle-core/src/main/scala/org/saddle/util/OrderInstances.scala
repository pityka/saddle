package org.saddle.util

import org.saddle.ORD
import org.saddle.util.TotalOrder
import cats.kernel.Order
import cats.kernel.Hash
import org.saddle.scalar._

object DoubleTotalOrder extends DoubleTotalOrderTrait
object FloatTotalOrder extends FloatTotalOrderTrait

/** An Order[Double] instance which produces a total order by ordering missing
  * values below all other Doubles
  *
  * Contrary to the specification of cats, the DoubleOrder in cats.kernel is not
  * total because NaN is not ordered (all comparison with Order.lt return
  * false). This behaviour is consistent with IEEE-754. DoubleOrder.comparison
  * is a total order though, it orders NaN above all others. This latter
  * behavior is consistent with java.lang.Double.compare.
  *
  * java.lang.Double.compare orders NaN to be largest of all Doubles.
  *
  * In saddle ordering is used extensively for sorting, including missing values
  * thus having a proper total order is practical. 
  * 
  * I choose to order missing
  * values below all other elements to keep consistency with existing saddle
  * code which uses the minimum values of integer types as missing sentinels.
  *
  * See https://github.com/scala/scala/pull/8721 See
  * https://github.com/scala/scala/blob/39e82c3f904380f0b40d106723747faf881640d4/src/library/scala/math/Ordering.scala#L465
  */
private[saddle] trait DoubleTotalOrderTrait
    extends TotalOrder[Double]
    with Hash[Double] {

  def hash(x: Double): Int = x.hashCode()
  def compare(x: Double, y: Double): Int =
    if (x < y) -1
    else if (x > y) 1
    else if (x == y) 0
    else if (x.isNaN && y.isNaN) 0
    else if (x.isNaN) -1
    else 1

  override def eqv(x: Double, y: Double): Boolean = compare(x, y) == 0
  override def neqv(x: Double, y: Double): Boolean = !eqv(x, y)
  override def gt(x: Double, y: Double): Boolean = compare(x, y) > 0
  override def gteqv(x: Double, y: Double): Boolean = compare(x, y) >= 0
  override def lt(x: Double, y: Double): Boolean = compare(x, y) < 0
  override def lteqv(x: Double, y: Double): Boolean = compare(x, y) <= 0

  override def min(x: Double, y: Double): Double =
    if (gteqv(x, y)) x else y
  override def max(x: Double, y: Double): Double =
    if (lteqv(x, y)) x else y
}

/** See DoubleTotalOrder
  */
private[saddle] trait FloatTotalOrderTrait
    extends TotalOrder[Float]
    with Hash[Float] {

  def hash(x: Float): Int = x.hashCode()
  def compare(x: Float, y: Float): Int =
    if (x < y) -1
    else if (x > y) 1
    else if (x == y) 0
    else if (x.isNaN && y.isNaN) 0
    else if (x.isNaN) -1
    else 1

  override def eqv(x: Float, y: Float): Boolean = compare(x, y) == 0
  override def neqv(x: Float, y: Float): Boolean = !eqv(x, y)
  override def gt(x: Float, y: Float): Boolean = compare(x, y) > 0
  override def gteqv(x: Float, y: Float): Boolean = compare(x, y) >= 0
  override def lt(x: Float, y: Float): Boolean = compare(x, y) < 0
  override def lteqv(x: Float, y: Float): Boolean = compare(x, y) <= 0

  override def min(x: Float, y: Float): Float =
    if (gteqv(x, y)) x else y
  override def max(x: Float, y: Float): Float =
    if (lteqv(x, y)) x else y
}

private[saddle] trait LowPriorityOrderInstane {
  /* Generic order which orders missing below all others */
  implicit def fromOrdering[T <: AnyRef](implicit
      ordering: Ordering[T],
      clm: org.saddle.CLM[T]
  ): ORD[T] = {
    val ord1 = Order.fromOrdering(ordering)
    val ord2 = new TotalOrder[T] {
      private val st = new org.saddle.scalar.ScalarTagAnyRef[T]
      def compare(x: T, y: T): Int = {
        def m(x: T) = st.isMissing(x)
        if (m(x) && !m(y)) -1
        else if (!m(x) && m(y)) 1
        else if (m(x) && m(y)) 0
        else ord1.compare(x, y)
      }
    }
    ord2
  }
}

private[saddle] trait OrderInstances extends LowPriorityOrderInstane {

  implicit def boolOrd: ORD[Boolean] =
    TotalOrder.fromCats(
      cats.kernel.instances.boolean.catsKernelStdOrderForBoolean,
      ScalarTagBool
    )
  implicit def intOrd: ORD[Int] =
    TotalOrder.fromCats(
      cats.kernel.instances.int.catsKernelStdOrderForInt,
      ScalarTagInt
    )
  implicit def longOrd: ORD[Long] =
    TotalOrder.fromCats(
      cats.kernel.instances.long.catsKernelStdOrderForLong,
      ScalarTagLong
    )
  implicit def doubleOrd: ORD[Double] = DoubleTotalOrder
  implicit def charOrd: ORD[Char] =
    TotalOrder.fromCats(
      cats.kernel.instances.char.catsKernelStdOrderForChar,
      ScalarTagChar
    )
  implicit def floatOrd: ORD[Float] = FloatTotalOrder
  implicit def byteOrd: ORD[Byte] =
    TotalOrder.fromCats(
      cats.kernel.instances.byte.catsKernelStdOrderForByte,
      ScalarTagByte
    )
  implicit def shortOrd: ORD[Short] =
    TotalOrder.fromCats(
      cats.kernel.instances.short.catsKernelStdOrderForShort,
      ScalarTagShort
    )

  /* String order which orders missing below all others */
  implicit def stringOrd: ORD[String] = {
    val ord1 = cats.kernel.instances.string.catsKernelStdOrderForString
    val ord2 = new TotalOrder[String] {
      def compare(x: String, y: String): Int = {
        def m(x: String) = org.saddle.scalar.ScalarTagString.isMissing(x)
        if (m(x) && !m(y)) -1
        else if (!m(x) && m(y)) 1
        else if (m(x) && m(y)) 0
        else ord1.compare(x, y)
      }
    }
    ord2
  }

  implicit def fromCatsOrder[T <: AnyRef](implicit
      ordering: Order[T],
      clm: org.saddle.CLM[T]
  ): ORD[T] = {
    val ord2 = new TotalOrder[T] {
      private val st = new org.saddle.scalar.ScalarTagAnyRef[T]
      def compare(x: T, y: T): Int = {
        def m(x: T) = st.isMissing(x)
        if (m(x) && !m(y)) -1
        else if (!m(x) && m(y)) 1
        else if (m(x) && m(y)) 0
        else ordering.compare(x, y)
      }
    }
    ord2
  }

}
