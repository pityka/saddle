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
package org.saddle.locatorall

import org.saddle.locator.Locator

@scala.annotation.nowarn
private[saddle] class LocatorAllBoolean(sz: Int = Locator.INIT_CAPACITY)
    extends LocatorAll[Boolean] {
  val trues = org.saddle.Buffer.empty[Int]
  val falses = org.saddle.Buffer.empty[Int]

  def contains(key: Boolean): Boolean =
    if (key) trues.length > 0 else falses.length > 0
  def get(key: Boolean): Array[Int] = if (key) trues.toArray else falses.toArray
  def put(key: Boolean, value: Int) = {
    if (key) trues.+=(value)
    else falses.+=(value)
  }

}
