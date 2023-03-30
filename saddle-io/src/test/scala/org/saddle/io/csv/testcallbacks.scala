/** Copyright (c) 2023 Saddle Development Team
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
package org.saddle.io.csv

object TestCallbacks {
  val noop = new Callback {
    def apply(s: Array[Char], from: Array[Int], to: Array[Int], len: Int) =
      if (len >= 0) Next else Error("Unclosed")
  }
}
class BufferCallback extends Callback {
  val buffer = scala.collection.mutable.ArrayBuffer[String]()
  def toList = buffer.toList
  def apply(s: Array[Char], from: Array[Int], to: Array[Int], len: Int) = {
    var i = 0
    if (len < 0) Error("Unclosed quote")
    else {
      while (i < len) {
        buffer.append(new String(s, from(i), math.abs(to(i)) - from(i)))
        i += 1
      }
      Next
    }

  }

}
class ForeachCallback(t: (CharSequence, Int) => Unit) extends Callback {
  val buffer = scala.collection.mutable.ArrayBuffer[String]()
  def toList = buffer.toList
  def apply(s: Array[Char], from: Array[Int], to: Array[Int], len: Int) = {
    var i = 0
    var loc = 0
    if (len < 0) Error("Unclosed quote")
    else {
      while (i < len) {
        t(new String(s, from(i), math.abs(to(i)) - from(i)), loc)
        if (to(i) < 0) loc = 0
        else loc += 1
        i += 1
      }
      Next
    }

  }

}
