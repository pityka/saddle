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

import org.saddle.{NUM, ORD, Vec, Index, Mat}
import org.saddle.Buffer
import org.saddle.index.IndexDouble
import org.saddle.locator.{LocatorDouble, Locator}
import org.saddle.array.Sorter

/** Double ScalarTag
  */
object ScalarTagDouble extends ScalarTag[Double] {
  @inline def missing: Double = Double.NaN
  @inline def isMissing(v: Double): Boolean = (v != v)
  @inline override def notMissing(v: Double): Boolean = (v == v)

  override def parse(s: Array[Char], from: Int, to: Int) =
    org.saddle.util.FastDoubleParser.parseFloatValue(s, from, to - from)

  // note, consider N/A's equal
  def compare(x: Double, y: Double)(implicit ev: ORD[Double]) =
    if (x == y) 0 else if (x > y) 1 else if (x < y) -1 else 0

  def toDouble(t: Double)(implicit ev: NUM[Double]): Double = t

  @inline def zero(implicit ev: NUM[Double]) = 0d
  @inline def one(implicit ev: NUM[Double]) = 1d
  @inline def inf(implicit ev: NUM[Double]) = Double.PositiveInfinity
  @inline def negInf(implicit ev: NUM[Double]) = Double.NegativeInfinity

  def show(v: Double) =
    if (isMissing(v)) "%s" format "NA" else "%.4f" format (v)

  override def strListLossless(v: Double) = List(asString(v))

  override def asString(v: Double) = v.toString

  override def runtimeClass = classOf[Double]

  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY) =
    Buffer.empty[Double](sz)
  def makeLoc(sz: Int = Locator.INIT_CAPACITY) = new LocatorDouble(sz)
  def makeVec(arr: Array[Double]) = Vec(arr)(this)
  def makeMat(r: Int, c: Int, arr: Array[Double]) = Mat(r, c, arr)(this)
  def makeIndex(vec: Vec[Double])(implicit ord: ORD[Double]): Index[Double] =
    new IndexDouble(vec, ord)
  def makeSorter(implicit ord: ORD[Double]): Sorter[Double] =
    Sorter.doubleSorter

  override def toString = "ScalarTagDouble"
}
