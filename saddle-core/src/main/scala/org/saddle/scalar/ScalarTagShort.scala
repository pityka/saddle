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

import org.saddle.{CLM, ORD}
import org.saddle.array.Sorter

/** Short ScalarTag
  */
object ScalarTagShort extends ScalarTagBase[Short] {
  override def parse(s: Array[Char], from: Int, to: Int) = {
    val i = ScalarTagInt.parse(s, from, to)

    if (i < Short.MinValue.toInt || i > Short.MaxValue.toInt) Short.MinValue
    else i.toShort
  }
  override def makeSorter(implicit ord: ORD[Short]): Sorter[Short] =
    Sorter.shortSorter

  override def missing: Short = Short.MinValue
  def isMissing(v: Short) = v == missing
  def clm = implicitly[CLM[Short]]
}
