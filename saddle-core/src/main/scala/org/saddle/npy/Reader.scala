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
package org.saddle.npy
import org.saddle.{Buffer => _, _}
import org.saddle.scalar._
import java.nio._
import scala.util.{Left, Right, Either}
import java.nio.channels.ReadableByteChannel

case class Descriptor(fortran: Boolean, shape: List[Int], dtype: String)

object Reader {

  def parseHeader(s: String): Descriptor = {
    val fortran = s.contains("'fortran_order': True")
    val dtype = "'descr': '([^']*)'".r
      .findFirstMatchIn(s)
      .get
      .matched
      .split(":")
      .last
      .trim
      .drop(1)
      .dropRight(1)
    val shape = "'shape': \\((\\d+)(, \\d+)*\\)".r
      .findFirstMatchIn(s)
      .get
      .matched
      .split(":")
      .last
      .trim
      .drop(1)
      .dropRight(1)
      .split(",")
      .map(_.trim.toInt)
      .toList

    Descriptor(fortran, shape, dtype)
  }

  private[npy] def width[T: ST] =
    implicitly[ST[T]] match {
      case ScalarTagDouble => Right(8)
      case ScalarTagInt    => Right(4)
      case ScalarTagFloat  => Right(4)
      case ScalarTagLong   => Right(8)
      case ScalarTagByte   => Right(1)
      case other           => Left(s"Type $other not supported.")
    }
  private[npy] def dtype[T: ST] =
    implicitly[ST[T]] match {
      case ScalarTagDouble => Right("<f8")
      case ScalarTagInt    => Right("<i4")
      case ScalarTagFloat  => Right("<f4")
      case ScalarTagLong   => Right("<i8")
      case ScalarTagByte   => Right("<i1")
      case other           => Left(s"Type $other not supported.")
    }
  def parse[T: ST](size: Int, from: ByteBuffer): Either[String, Array[T]] =
    implicitly[ST[T]] match {
      case ScalarTagDouble =>
        Right {
          val to = DoubleBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getDouble)
          }
          to.array
        }
      case ScalarTagInt =>
        Right {
          val to = IntBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getInt)
          }
          to.array
        }
      case ScalarTagFloat =>
        Right {
          val to = FloatBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getFloat())
          }
          to.array
        }
      case ScalarTagLong =>
        Right {
          val to = LongBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.put(from.getLong())
          }
          to.array
        }
      case ScalarTagByte =>
        Right {
          val to = ByteBuffer.allocate(size)
          while (to.hasRemaining() && from.hasRemaining()) {
            to.asInstanceOf[ByteBuffer].put(from.get)
          }
          to.array
        }
      case other => Left(s"Type $other not supported.")
    }

  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    if (s.forall(_.isRight))
      Right(s.map(_.right.get))
    else s.find(_.isLeft).get.asInstanceOf[Left[A, Seq[B]]]

  def readFully(bb: ByteBuffer, channel: ReadableByteChannel) = {
    bb.clear
    var i = 0
    while (bb.hasRemaining && i >= 0) {
      i = channel.read(bb)
    }
    bb.flip
  }

  def readHeaderFromChannel[T: ST](channel: ReadableByteChannel) =
    dtype[T].right.flatMap { expectedDataType =>
      val magicAndVersion = Array.ofDim[Byte](8)
      readFully(ByteBuffer.wrap(magicAndVersion), channel)
      val magic = String.valueOf(magicAndVersion.map(_.toChar), 0, 6)
      val major = magicAndVersion(6)
      val minor = magicAndVersion(7)
      if (magic.drop(1) != "NUMPY")
        Left(
          s"Magic string is incorrect. Found in file: $magic / $major / $minor"
        )
      else {
        val headerLengthBB =
          ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
        readFully(headerLengthBB, channel)
        val headerLength = headerLengthBB.getShort.toInt
        val headerArray = Array.ofDim[Byte](headerLength)
        readFully(ByteBuffer.wrap(headerArray), channel)
        val header = new String(headerArray, "UTF-8")
        val descriptor = parseHeader(header)
        if (descriptor.fortran || descriptor.dtype != expectedDataType)
          Left("Data layout not supported")
        else Right(descriptor)
      }
    }

  def readMatDataFromChannel[T: ST](
      channel: ReadableByteChannel,
      numRows: Int,
      numCols: Int,
      width: Int
  ): Either[String, Mat[T]] = {
    val bb = ByteBuffer
      .allocate(width * numRows * numCols)
      .order(ByteOrder.LITTLE_ENDIAN)
    readFully(bb, channel)
    parse[T](numRows * numCols, bb).right.flatMap { data =>
      if (data.size != numRows * numCols) {
        Left("Premature end of input")
      } else {
        Right(Mat(numRows, numCols, data.asInstanceOf[Array[T]]))
      }
    }
  }
  def readVecDataFromChannel[T: ST](
      channel: ReadableByteChannel,
      len: Int,
      width: Int
  ): Either[String, Vec[T]] = {
    val bb = ByteBuffer
      .allocate(width * len)
      .order(ByteOrder.LITTLE_ENDIAN)
    readFully(bb, channel)
    parse[T](len, bb).right.flatMap { data =>
      if (data.size != len) {
        Left("Premature end of input")
      } else {
        Right(Vec(data.asInstanceOf[Array[T]]))
      }
    }
  }

  def readMatFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, Mat[T]] = {
    readHeaderFromChannel[T](channel).right.flatMap { descriptor =>
      width[T].right.flatMap { width =>
        if (descriptor.shape.size != 2) Left("Not a matrix shape")
        else {
          val numRows =
            descriptor.shape(0)
          val numCols = descriptor.shape(1)
          readMatDataFromChannel(channel, numRows, numCols, width)
        }
      }
    }
  }
  def readVecFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, (Vec[T], List[Int])] = {
    readHeaderFromChannel[T](channel).right.flatMap { descriptor =>
      width[T].right.flatMap { width =>
        val len = descriptor.shape.reduce(_ * _)
        readVecDataFromChannel(channel, len, width).right.map(s =>
          (s, descriptor.shape)
        )
      }
    }
  }

  class ByteChannel(src: ByteBuffer) extends ReadableByteChannel {
    def read(dst: ByteBuffer) = {
      var i = 0
      while (dst.hasRemaining() && src.hasRemaining()) {
        dst.put(src.get)
        i += 1
      }
      i
    }
    def isOpen(): Boolean = true
    def close(): Unit = ()
  }

  def readMatFromArray[T: ST](
      array: Array[Byte]
  ): Either[String, Mat[T]] =
    readMatFromChannel(new ByteChannel(ByteBuffer.wrap(array)))
}
