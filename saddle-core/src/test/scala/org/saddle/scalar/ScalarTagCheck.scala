/** Copyright (c) 2022 Saddle Development Team
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
package org.saddle

import org.specs2.mutable.Specification
import org.saddle.scalar.ScalarTagAny
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.saddle.scalar._

class ScalarTagCheck extends Specification with ScalaCheck {

  "ScalarTagAny" should {
    val tag = implicitly[ScalarTagAny[Any]]
    "treat null as missing" in {
      tag.isMissing(na: AnyRef)
    }
    "treat plain na as missing" in {
      tag.isMissing(na)
    }
  }

  "ScalarTagByte" should {
    "use Byte.MinValue as the missing value for Byte - check" in {
      def prop(v: Byte) =
        ScalarTagByte.isMissing(v) must_== (v == Byte.MinValue)
      prop(Byte.MinValue)
      forAll { (v: Byte) => prop(v) }
    }
  }
  "ScalarTagShort" should {
    "use Short.MinValue as the missing value for Short - check" in {
      def prop(v: Short) =
        ScalarTagShort.isMissing(v) must_== (v == Short.MinValue)
      prop(Short.MinValue)
      forAll { (v: Short) => prop(v) }
    }
  }
  "ScalarTagChar" should {
    "use Char.MinValue as the missing value for Char" in {
      forAll { (b: Char) =>
        ScalarTagChar.isMissing(b) must_== (b == Char.MinValue)
      }
    }
  }
  "ScalarTagInt" should {
    "use Int.MinValue as the missing value for Int - check" in {
      def prop(v: Int) =
        ScalarTagInt.isMissing(v) must_== (v == Int.MinValue)
      prop(Int.MinValue)
      forAll { (v: Int) => prop(v) }
    }
  }
  "ScalarTagLong" should {
    "use Long.MinValue as the missing value for Long - check" in {
      def prop(v: Long) =
        ScalarTagLong.isMissing(v) must_== (v == Long.MinValue)
      prop(Long.MinValue)
      forAll { (v: Long) => prop(v) }
    }
  }
  "ScalarTagFloat" should {
    "use Float.Nan as the missing value for Float - check" in {
      def prop(v: Float) =
        ScalarTagFloat.isMissing(v) must_== (v != v)
      prop(Float.NaN)
      forAll { (v: Float) => prop(v) }
    }
  }
  "ScalarTagDouble" should {
    "use Double.NaN as the missing value for Double - check" in {
      def prop(v: Double) =
        ScalarTagDouble.isMissing(v) must_== (v != v)
      prop(Double.NaN)
      forAll { (v: Double) => prop(v) }
    }
  }
}
