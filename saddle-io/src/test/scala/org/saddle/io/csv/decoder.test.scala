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

import org.scalatest.funsuite.AnyFunSuite
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.Charset

class ByteChannel(src: ByteBuffer) extends ReadableByteChannel {
  def read(dst: ByteBuffer) = {
    var i = 0
    while (dst.hasRemaining() && src.hasRemaining()) {
      dst.put(src.get)
      i += 1
    }
    if (dst.hasRemaining() && i == 0) -1
    else i
  }
  def isOpen(): Boolean = true
  def close(): Unit = ()
}

object ByteChannel {
  def apply(s: Array[Byte]) = {
    val data =
      ByteBuffer.wrap(s)
    new ByteChannel(data)
  }
  def apply(s: String) = {
    val data =
      ByteBuffer.wrap(s.getBytes("UTF-8"))
    new ByteChannel(data)
  }
  def prep(s: String, bufferSize: Int = 4) = {
    val data =
      ByteBuffer.wrap(s.getBytes("UTF-8"))
    val channel = new ByteChannel(data)
    readChannel(
      channel,
      bufferSize,
      Charset
        .forName("UTF-8")
        .newDecoder()
    )
  }
}

class DecoderSuite extends AnyFunSuite {

  test("single line") {

    val bytes = s"""a,b,€€,c,d,e""".getBytes("UTF-8")
    val data =
      ByteBuffer.wrap(bytes)
    val channel = new ByteChannel(data)
    val decoded = readChannel(
      channel,
      4,
      Charset
        .forName("UTF-8")
        .newDecoder()
    ).map { b =>
      val s = b.toString
      b.position(b.limit)
      s
    }.toList
      .mkString
    assert(decoded == """a,b,€€,c,d,e""")

  }

}
