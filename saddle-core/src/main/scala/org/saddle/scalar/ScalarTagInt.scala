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

import org.saddle.{NUM, ORD, Vec, Mat, Index}
import org.saddle.Buffer
import org.saddle.index.IndexInt
import org.saddle.locator.{LocatorInt, Locator}
import org.saddle.array.Sorter

/** Int ScalarTag
  */
object ScalarTagInt extends ScalarTag[Int] {
  def missing: Int = Int.MinValue
  def isMissing(v: Int): Boolean = v == Int.MinValue
  override def notMissing(v: Int): Boolean = !isMissing(v)

  private final val _max = Int.MaxValue / 10

  override final def parse(s: Array[Char], from: Int, to: Int): Int = {

    val radix = 10
    var length = to - from
    if (length == 0 || length > 11) return Int.MinValue

    val firstChar = s(from)

    val positive = firstChar == '+'
    val negative = firstChar == '-'
    var offset = from
    if (positive || negative) {
      offset += 1
      length -= 1
    }

    if (length == 0) return Int.MinValue

    var result = 0
    if (length > 3) {
      // SWAR validation and parsing of the first max 8 characters

      // pack into a long, little endian
      var p = 0L
      length match {
        case 4 =>
          p = ((s(offset) & 0xff).toLong) |
            ((s(offset + 1) & 0xff).toLong << 8) |
            ((s(offset + 2) & 0xff).toLong << 16) |
            ((s(offset + 3) & 0xff).toLong << 24)
        case 5 =>
          p = ((s(offset) & 0xff).toLong) |
            ((s(offset + 1) & 0xff).toLong << 8) |
            ((s(offset + 2) & 0xff).toLong << 16) |
            ((s(offset + 3) & 0xff).toLong << 24) |
            ((s(offset + 4) & 0xff).toLong << 32)
        case 6 =>
          p = ((s(offset) & 0xff).toLong) |
            ((s(offset + 1) & 0xff).toLong << 8) |
            ((s(offset + 2) & 0xff).toLong << 16) |
            ((s(offset + 3) & 0xff).toLong << 24) |
            ((s(offset + 4) & 0xff).toLong << 32) |
            ((s(offset + 5) & 0xff).toLong << 40)
        case 7 =>
          p = ((s(offset) & 0xff).toLong) |
            ((s(offset + 1) & 0xff).toLong << 8) |
            ((s(offset + 2) & 0xff).toLong << 16) |
            ((s(offset + 3) & 0xff).toLong << 24) |
            ((s(offset + 4) & 0xff).toLong << 32) |
            ((s(offset + 5) & 0xff).toLong << 40) |
            ((s(offset + 6) & 0xff).toLong << 48)
        case _ =>
          p = ((s(offset) & 0xff).toLong) |
            ((s(offset + 1) & 0xff).toLong << 8) |
            ((s(offset + 2) & 0xff).toLong << 16) |
            ((s(offset + 3) & 0xff).toLong << 24) |
            ((s(offset + 4) & 0xff).toLong << 32) |
            ((s(offset + 5) & 0xff).toLong << 40) |
            ((s(offset + 6) & 0xff).toLong << 48) |
            ((s(offset + 7) & 0xff).toLong << 56)
      }
      // pad with '0' = 48 = 0x30
      val m = math.max(0, 8 - length) * 8
      p = p << m | 0x3030303030303030L

      // SWAR validation
      var q = p - 0x3030303030303030L
      val det = ((p + 0x4646464646464646L) | q) &
        0x8080808080808080L;
      if (det != 0L) {
        return Int.MinValue
      }

      // SWAR conversion
      val mask = 0x000000ff_000000ffL;
      q = (q * 0xa_01L) >>> 8; // 1+(10<<8)
      q = (((q & mask) * 0x000f4240_00000064L) // 100 + (1000000 << 32)
        + (((q >>> 16) & mask) * 0x00002710_00000001L)) >>> 32; // 1 + (10000 << 32)
      result = q.toInt
      offset += math.min(8, length)
    }
    // END SWAR, continue regularly with the remaining if any

    while (offset < to) {
      val digit = s(offset).toByte - 48
      val next = result * radix + digit
      if (digit < 0 || digit > 9 || _max < result || next < result) {
        return Int.MinValue
      }
      result = next
      offset += 1
    }

    if (negative) {
      result = -result
      if (result > 0) {
        return Int.MinValue
      }
    }

    result

  }

  def compare(x: Int, y: Int)(implicit ev: ORD[Int]) =
    if (x == y) 0 else if (x > y) 1 else -1

  def toDouble(t: Int)(implicit ev: NUM[Int]) =
    if (isMissing(t)) ScalarTagDouble.missing else t.asInstanceOf[Double]

  def zero(implicit ev: NUM[Int]) = 0
  def one(implicit ev: NUM[Int]) = 1
  def inf(implicit ev: NUM[Int]) = Int.MaxValue
  def negInf(implicit ev: NUM[Int]) = Int.MinValue

  def show(v: Int) = if (isMissing(v)) "%s" format "NA" else "%d" format v

  override def runtimeClass = classOf[Int]

  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY) =
    Buffer.empty[Int](sz)
  def makeLoc(sz: Int = Locator.INIT_CAPACITY) = new LocatorInt(sz)
  def makeVec(arr: Array[Int]) = Vec(arr)(this)
  def makeMat(r: Int, c: Int, arr: Array[Int]) = Mat(r, c, arr)(this)
  def makeIndex(vec: Vec[Int])(implicit ord: ORD[Int]): Index[Int] =
    new IndexInt(vec, ord)
  def makeSorter(implicit ord: ORD[Int]): Sorter[Int] = Sorter.intSorter

  override def toString = "ScalarTagInt"
}
