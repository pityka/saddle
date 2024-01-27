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

import org.saddle.Buffer
import org.saddle.ST

@scala.annotation.nowarn
private[saddle] class LocatorAllAny[T: ST]
    extends LocatorAll[T] {
  val map = scala.collection.mutable.HashMap.empty[T, Buffer[Int]]
  val falses = org.saddle.Buffer.empty[Int]

  def contains(key: T): Boolean = map.contains(key)
  def get(key: T): Array[Int] =
    if (map.contains(key)) map(key).toArray else LocatorAll.empty
  def put(key: T, value: Int) = {
    map.get(key) match {
      case None =>
        val b = Buffer.empty[Int]
        b += value
        map.update(key, b)
      case Some(b) => b += value
    }
  }

}
