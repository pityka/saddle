/** This code is copied from https://github.com/denisrosset/meta The MIT License
  * (MIT) \=====================
  *
  * Copyright (c) 2015 Denis Rosset Hash set and hash map implementations based
  * on code (c) 2012-2014 Eirk Osheim
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to
  * deal in the Software without restriction, including without limitation the
  * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  * sell copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  * IN THE SOFTWARE.
  */
package org.saddle

import scala.reflect.ClassTag
import scala.collection.mutable.{Buffer => SBuffer}
import scala.collection.mutable.ListBuffer

final class Buffer[@specialized V] private[saddle] (
    private var arrays: SBuffer[Array[V]],
    var length: Int
)(implicit
    val ctV: ClassTag[V]
) {

  private var cursor = 0

  final def toArray: Array[V] = {
    val res = ctV.newArray(length.toInt)
    var i = 0
    var j = 0
    arrays.foreach { array =>
      val l =
        if (j == arrays.length - 1) length - i
        else array.length
      Array.copy(array, 0, res, i, l)
      j += 1
      i += array.length

    }
    res
  }

  final def toArrays: List[Array[V]] = {
    arrays.zipWithIndex.toList.map { case (array, idx) =>
      if (idx < arrays.size - 1) {
        array
      } else {
        val res = ctV.newArray(cursor)
        Array.copy(array, 0, res, 0, res.length)
        res
      }
    }

  }

  final def +=(elem: V): this.type = {
    arrays.last(cursor) = elem
    length += 1
    cursor += 1
    fill()
    this
  }

  private final def fill(): Buffer.Dummy[V] = {
    if (arrays.last.length <= cursor) {
      val newArray =
        ctV.newArray(math.min(Buffer.maxSize, arrays.last.length * 2))
      arrays.append(newArray)
      cursor = 0
    }
    null
  }

}

object Buffer {

  class Dummy[@specialized A]

  object Dummy {

    implicit def apply[@specialized A]: Dummy[A] = null
  }

  val startSize = 256
  val maxSize = 16777216

  val INIT_CAPACITY = startSize


  def empty[@specialized(Int, Double, Boolean, Float) T: ClassTag]: Buffer[T] =
    new Buffer(ListBuffer(new Array[T](startSize)), 0)

  def empty[@specialized(Int, Double, Boolean, Float) T: ClassTag](
      initSize: Int
  ): Buffer[T] =
    new Buffer(ListBuffer(new Array[T](initSize)), 0)

}
