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

/** Float ScalarTag
  */
object ScalarTagFloat extends ScalarTagBase[Float] {
  override def parse(s: String) =
    try {
      s.toFloat
    } catch { case _: NumberFormatException => Float.NaN }
  override def makeSorter(implicit ord: ORD[Float]): Sorter[Float] =
    Sorter.floatSorter

  override def missing: Float = Float.NaN
  override def isMissing(v: Float): Boolean = v != v
  def clm = implicitly[CLM[Float]]

}
