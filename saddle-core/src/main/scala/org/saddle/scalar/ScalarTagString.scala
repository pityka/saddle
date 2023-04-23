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

import org.saddle.CLM

object ScalarTagString extends ScalarTagBase[String] {
  override def toString = "ScalarTagString"
  def missing: String = null.asInstanceOf[String]

  def isMissing(v: String): Boolean = (v == missing)
  def clm = implicitly[CLM[String]]
  override def parse(s: Array[Char], from: Int, to: Int): String =
    new String(s, from, to - from)
}
// object ScalarTagCharSequence
//     extends ScalarTagBase[CharSequence] {
//   override def toString = "ScalarTagCharSequence"
//   def missing: CharSequence = null.asInstanceOf[CharSequence]

//   def isMissing(v: CharSequence): Boolean = (v == missing)
//   def clm = implicitly[CLM[CharSequence]]
//   override def parse(s:Array[Char], from: Int, to: Int) : String = new String(s,from,to-from)
// }
