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

final class ScalarTagAnyRef[T <: AnyRef](implicit val clm: CLM[T])
    extends ScalarTagBase[T] {
  override def toString = "ScalarTagAnyRef"
  def missing: T = null.asInstanceOf[T]

  def isMissing(v: T): Boolean = (v == missing)
}
