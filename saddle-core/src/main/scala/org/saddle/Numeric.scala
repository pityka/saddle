package org.saddle

import org.saddle.util.{DoubleTotalOrderTrait, FloatTotalOrderTrait}

trait Numeric[@specialized(Int, Float, Double) T] extends ORD[T] {
  def plus(a: T, b: T): T
  def minus(a: T, b: T): T
  def negate(a: T): T
  def abs(a: T): T
  def zero: T
  def one: T
  def times(a: T, b: T): T
  def toDouble(a: T): Double
}

object doubleIsNumeric extends Numeric[Double] with DoubleTotalOrderTrait {
  def plus(a: Double, b: Double) = a + b
  def minus(a: Double, b: Double) = a - b
  def negate(a: Double): Double = -a
  def abs(a: Double) = math.abs(a)
  def zero: Double = 0d
  def one: Double = 1d
  def times(a: Double, b: Double): Double = a * b
  def toDouble(a: Double): Double = a
}
object floatIsNumeric extends Numeric[Float] with FloatTotalOrderTrait {
  def plus(a: Float, b: Float) = a + b
  def minus(a: Float, b: Float) = a - b
  def negate(a: Float): Float = -a
  def abs(a: Float) = math.abs(a)
  def zero: Float = 0f
  def one: Float = 1f
  def times(a: Float, b: Float): Float = a * b
  def toDouble(a: Float): Double = a.toDouble
}
object longIsNumeric extends Numeric[Long] {
  def plus(a: Long, b: Long) = a + b
  def minus(a: Long, b: Long) = a - b
  def negate(a: Long): Long = -a
  def abs(a: Long) = math.abs(a)
  def zero: Long = 0L
  def one: Long = 1L
  def times(a: Long, b: Long): Long = a * b
  def toDouble(a: Long): Double = a.toDouble
  def compare(a: Long, b: Long) =
    cats.kernel.instances.long.catsKernelStdOrderForLong.compare(a, b)
}
object intIsNumeric extends Numeric[Int] {
  def plus(a: Int, b: Int) = a + b
  def minus(a: Int, b: Int) = a - b
  def negate(a: Int): Int = -a
  def abs(a: Int) = math.abs(a)
  def zero: Int = 0
  def one: Int = 1
  def times(a: Int, b: Int): Int = a * b
  def toDouble(a: Int): Double = a.toDouble
  def compare(a: Int, b: Int) =
    cats.kernel.instances.int.catsKernelStdOrderForInt.compare(a, b)
}
