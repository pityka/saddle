/**
  * Copyright (c) 2019 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package org.saddle.linalg

import org.saddle.Vec
import NetLib._
import org.saddle.vec.VecDefault

final class VecPimp(val self: Vec[Double]) {
  type B = Vec[Double]

  def linalg = this

  def vv(other: Vec[Double]): Double = {
    val a = self
    val b = other
    assert(a.length == b.length)
    assert(a.length > 0)
    BLAS.ddot(a.length, a.toArray, 1, b.toArray, 1)

  }
  def vv2(other: Vec[Double]): Double = {
    if (other.isInstanceOf[VecDefault[Double]] && self
          .isInstanceOf[VecDefault[Double]])
      vv_spec(other.asInstanceOf[VecDefault[Double]])
    else {
      var i = 0
      var s = 0d
      val N = self.length
      while (i < N) {
        s += self.raw(i) * other.raw(i)
        i += 1
      }
      s
    }
  }
  private def vv_spec(other: VecDefault[Double]): Double = {
    var i = 0
    var s = 0d
    val N = self.length
    val selfD = self.asInstanceOf[VecDefault[Double]]
    while (i < N) {
      s += selfD.raw(i) * other.raw(i)
      i += 1
    }
    s
  }

}
