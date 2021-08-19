/** Copyright (c) 2019 Saddle Development Team
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
package org.saddle.jsoniter
import org.saddle._
import org.saddle.order._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import org.scalatest.funsuite.AnyFunSuite

class BinarySuite extends AnyFunSuite {
  test("2x3") {
    implicit val codec0: JsonValueCodec[Double] = JsonCodecMaker.make
    implicit val codec1: JsonValueCodec[String] = JsonCodecMaker.make

    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    assert(
      readFromString[Frame[String, String, Double]](
        writeToString(frame)
      )
        == frame
    )
    assert(
      readFromString[Series[String, Double]](
        writeToString(frame.rowAt(0))
      ) == frame.rowAt(0)
    )
    assert(
      readFromString[Mat[Double]](
        writeToString(frame.toMat)
      ) == frame.toMat
    )

  }

}
