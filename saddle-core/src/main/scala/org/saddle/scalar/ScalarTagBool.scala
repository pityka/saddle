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

import org.saddle.{ORD, NUM, Vec, Mat, Index}
import org.saddle.index.IndexAny
import org.saddle.locator.{LocatorBool, Locator}
import org.saddle.array.Sorter
import org.saddle.Buffer

/** Boolean ScalarTag. No support for missing value.
  */
object ScalarTagBool extends ScalarTag[Boolean] {
  def missing: Boolean = false
  def isMissing(v: Boolean): Boolean = false
  override def notMissing(v: Boolean): Boolean = true

  def compare(x: Boolean, y: Boolean)(implicit ev: ORD[Boolean]) =
    if (x > y) 1 else 0

  def toDouble(t: Boolean)(implicit ev: NUM[Boolean]) = if (t) 1.0 else 0.0

  def zero(implicit ev: NUM[Boolean]) = false
  def one(implicit ev: NUM[Boolean]) = true
  def inf(implicit ev: NUM[Boolean]) = true
  def negInf(implicit ev: NUM[Boolean]) = false

  def show(v: Boolean) = "%b" format v

  override def runtimeClass = classOf[Boolean]

  def makeBuf(sz: Int = org.saddle.Buffer.INIT_CAPACITY) =
    Buffer.empty[Boolean](sz)
  def makeLoc(sz: Int = Locator.INIT_CAPACITY) = new LocatorBool()
  def makeVec(arr: Array[Boolean]) = Vec(arr)(this)
  def makeMat(r: Int, c: Int, arr: Array[Boolean]) = Mat(r, c, arr)(this)
  def makeIndex(vec: Vec[Boolean])(implicit ord: ORD[Boolean]): Index[Boolean] =
    new IndexAny[Boolean](vec)
  def makeSorter(implicit ord: ORD[Boolean]): Sorter[Boolean] =
    Sorter.boolSorter

  def parse(s: Array[Char], from: Int, to: Int) =
    new String(s, from, to).toBoolean

  override def toString = "ScalarTagBool"
}
