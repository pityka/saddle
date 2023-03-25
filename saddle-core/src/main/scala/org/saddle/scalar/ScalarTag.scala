/** Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License"); you may not
  * use this file except in compliance with the License. You may obtain a copy
  * of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  * License for the specific language governing permissions and limitations
  * under the License.
  */
package org.saddle.scalar

import scala.reflect.ClassTag
import scala.{specialized => spec}
import org.saddle.{CLM, ORD, NUM, Vec, Mat, Index, ST}
import org.saddle.locator.Locator
import org.saddle.array.Sorter
import org.saddle.Buffer
import collection.immutable.ArraySeq

/** Typeclass definition for scalar tags. A ScalarTag contains important
  * meta-data regarding a scalar type, including how to instantiate a
  * Buffer/Vec/Mat/Index of that type, as well as an array. Often implicitly
  * required when dealing with objects in Saddle
  */
trait ScalarTag[@spec(Boolean, Int, Long, Float, Double) T]
    extends ClassTag[T]
    with SpecializedFactory[T]
    with CouldBeOrdered[T]
    with CouldBeNumber[T] {
  // representation of missing data
  def missing: T
  def isMissing(t: T): Boolean
  def notMissing(t: T): Boolean = !isMissing(t)

  def parse(s: Array[Char], from: Int, to: Int): T

  def strList(v: T) = List(show(v))
  def strListLossless(v: T) = strList(v)

  /* Not necessarily loss-less (e.g. rounds) */
  def show(v: T): String

  /* Must hold: parse(asString(v)) == v */
  def asString(v: T): String = show(v)

  override def hashCode(): Int =
    runtimeClass.hashCode()

  override def equals(o: Any): Boolean = o match {
    case s: ScalarTag[_] =>
      (this eq s) || runtimeClass == s.runtimeClass
    case _ => false
  }

  override def toString = "ScalarTag[%s]" format runtimeClass

  override def erasure = runtimeClass

  // forward 2.10 compatibility
  def runtimeClass: Class[_]
}

object ScalarTag extends ScalarTagImplicits {
  implicit val stBool: ScalarTag[Boolean] = ScalarTagBool
  implicit val stChar: ScalarTag[Char] = ScalarTagChar
  implicit val stByte: ScalarTag[Byte] = ScalarTagByte
  implicit val stShort: ScalarTag[Short] = ScalarTagShort
  implicit val stInt: ScalarTag[Int] = ScalarTagInt
  implicit val stFloat: ScalarTag[Float] = ScalarTagFloat
  implicit val stLong: ScalarTag[Long] = ScalarTagLong
  implicit val stDouble: ScalarTag[Double] = ScalarTagDouble
  implicit val stString: ScalarTag[String] = ScalarTagString
}

trait ScalarTagImplicits extends ScalarTagImplicitsL1 {
  implicit def stPrd[T <: Product](implicit ev: CLM[T]): ScalarTag[T] =
    new ScalarTagProduct[T]()(ev)
}

trait ScalarTagImplicitsL1 {
  implicit def stAnyRef[T <: AnyRef](implicit ev: CLM[T]): ScalarTag[T] =
    new ScalarTagAnyRef[T]()(ev)
}

trait CouldBeOrdered[@spec(Boolean, Int, Long, Float, Double) T] {
  // for comparable scalars
  def compare(a: T, b: T)(implicit ev: ORD[T]): Int
  def lt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) < 0
  def gt(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) > 0
  def iseq(a: T, b: T)(implicit ev: ORD[T]) = compare(a, b) == 0
}

trait CouldBeNumber[@spec(Boolean, Int, Long, Float, Double) T] {
  // for numeric scalars
  def toDouble(t: T)(implicit ev: NUM[T]): Double

  def zero(implicit ev: NUM[T]): T
  def one(implicit ev: NUM[T]): T
  def inf(implicit ev: NUM[T]): T
  def negInf(implicit ev: NUM[T]): T
}

trait SpecializedFactory[@spec(Boolean, Int, Long, Float, Double) T] {
  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY): Buffer[T]
  def makeLoc(sz: Int = Locator.INIT_CAPACITY): Locator[T]
  def makeVec(arr: Array[T]): Vec[T]
  def makeMat(r: Int, c: Int, arr: Array[T]): Mat[T]
  def makeIndex(vec: Vec[T])(implicit ord: ORD[T]): Index[T]
  def makeSorter(implicit ord: ORD[T]): Sorter[T]

  /** An alternative Mat factory method using array of Vecs
    */
  final def makeMat(arr: Array[Vec[T]])(implicit st: ST[T]): Mat[T] = {
    val c = arr.length
    if (c == 0) st.makeMat(0, 0, st.newArray(0))
    else {
      val r = arr(0).length
      if (r == 0) st.makeMat(0, 0, st.newArray(0))
      else {
        require(
          arr.foldLeft(true)(_ && _.length == r),
          "All vec inputs must have the same length"
        )
        altMatConstructor(r, c, arr)
      }
    }
  }

  /** Can override this default construction methodology to avoid the toArray
    * call if you don't want to extract elements that way.
    */
  protected def altMatConstructor(r: Int, c: Int, arr: Array[Vec[T]])(implicit
      st: ST[T]
  ): Mat[T] =
    makeMat(c, r, org.saddle.concat(ArraySeq.unsafeWrapArray(arr)).toArray).T
}
