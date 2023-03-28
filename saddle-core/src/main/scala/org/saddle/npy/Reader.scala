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
package org.saddle.npy
import org.saddle.{Buffer => _, _}
import org.saddle.scalar._
import java.nio._
import scala.util.{Left, Right, Either}
import java.nio.channels.ReadableByteChannel
import org.saddle.io.npy.DType

case class Descriptor(fortran: Boolean, shape: List[Int], dtype: String)

object Reader {

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
  private[npy] def dtype2[T: ST]: Either[String, DType[T]] =
    implicitly[ST[T]] match {
      case ScalarTagDouble => Right(org.saddle.io.npy.DoubleType)
      case ScalarTagInt    => Right(org.saddle.io.npy.IntType)
      case ScalarTagFloat  => Right(org.saddle.io.npy.FloatType)
      case ScalarTagLong   => Right(org.saddle.io.npy.LongType)
      case ScalarTagByte   => Right(org.saddle.io.npy.ByteType)
      case other           => Left(s"Type $other not supported.")
    }

  private def parse[T: ST](
      size: Int,
      from: ByteBuffer
  ): Either[String, Array[T]] =
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

  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    if (s.forall(_.isRight))
      Right(s.map(_.toOption.get))
    else s.find(_.isLeft).get.asInstanceOf[Left[A, Seq[B]]]

  private def readFully(bb: ByteBuffer, channel: ReadableByteChannel) =
    org.saddle.io.npy.readFully(bb, channel)

  def readMatFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, Mat[T]] = {
    dtype2[T].flatMap { dtype =>
      org.saddle.io.npy.readFromChannel[T](dtype, channel).flatMap {
        case (descr, arrays) =>
          if (descr.shape.size != 2) Left("Not matrix shape")
          else
            sequence(arrays.toList).map { arrays =>
              val vec = arrays
                .map(_.toVec.asInstanceOf[Vec[T]])
                .foldLeft(Vec.empty[T])(_ concat _)
              Mat(descr.shape(0).toInt, descr.shape(1).toInt, vec)
            }
      }
    }
  }
  def readVecFromChannel[T: ST](
      channel: ReadableByteChannel
  ): Either[String, (Vec[T], List[Int])] = {
    dtype2[T].flatMap { dtype =>
      org.saddle.io.npy.readFromChannel(dtype, channel).flatMap {
        case (descr, arrays) =>
          sequence(arrays.toList).map { arrays =>
            val vec = arrays
              .map(_.toVec.asInstanceOf[Vec[T]])
              .foldLeft(Vec.empty[T])(_ concat _)
            (vec, descr.shape.map(_.toInt))
          }
      }

    }
  }

  private class ByteChannel(src: ByteBuffer) extends ReadableByteChannel {
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

  def readVecDataFromChannel[T: ST](
      channel: ReadableByteChannel,
      len: Int,
      width: Int
  ): Either[String, Vec[T]] = {
    val bb = ByteBuffer
      .allocate(width * len)
      .order(ByteOrder.LITTLE_ENDIAN)
    readFully(bb, channel)
    parse[T](len, bb).flatMap { data =>
      if (data.size != len) {
        Left("Premature end of input")
      } else {
        Right(Vec(data.asInstanceOf[Array[T]]))
      }
    }
  }
}
