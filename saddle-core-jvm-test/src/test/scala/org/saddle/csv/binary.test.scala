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
package org.saddle.binary
import org.saddle._
import org.scalatest.funsuite.AnyFunSuite
import org.saddle.order._

class BinarySuite extends AnyFunSuite {

  test("frame into file") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toLong),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val file = java.io.File.createTempFile("saddletest", "saddle")
    val os = new java.io.FileOutputStream(file)
    val writableChannel = os.getChannel
    Writer.writeFrameIntoChannel(frame, writableChannel).toOption.get
    writableChannel.close
    val is = new java.io.FileInputStream(file)
    val readableChannel = is.getChannel
    val deserFrame = Reader.readFrameFromChannel[Long](readableChannel)
    readableChannel.close
    assert(deserFrame.toOption.get == frame)
  }
  test("mat into file") {
    val frame = Frame(
      Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)).map(_.toLong),
      Index("r1", "r2"),
      Index("c1", "c2", "c3")
    )
    val file = java.io.File.createTempFile("saddletest", "saddle")
    val os = new java.io.FileOutputStream(file)
    val writableChannel = os.getChannel
    Writer.writeMatIntoChannel(frame.toMat, writableChannel).toOption.get
    writableChannel.close
    val is = new java.io.FileInputStream(file)
    val readableChannel = is.getChannel
    val deser = Reader.readMatFromChannel[Long](readableChannel)
    readableChannel.close
    assert(deser.toOption.get == frame.toMat)
  }

}
