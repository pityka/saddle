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
import org.saddle.scalar._
import org.saddle._
import java.nio.ByteOrder
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel

object Writer {
  private def int(i: Int) = {
    ByteBuffer
      .allocate(4)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putInt(i)
      .array
  }

  private[binary] val KEY_datatype = "datatype"
  private[binary] val KEY_numcols = "numcols"
  private[binary] val KEY_numrows = "numrows"
  private[binary] val KEY_rowmajor = "rowmajor"
  private[binary] val KEY_v = "v"
  private[binary] val KEY_colix = "colix"
  private[binary] val KEY_rowix = "rowix"

  private def createMatDescriptor[T: ST](
      mat: Mat[T]
  ) = dtype[T].map { dtype =>
    ujson
      .write(
        ujson.Obj(
          KEY_datatype -> dtype,
          KEY_numcols -> mat.numCols,
          KEY_numrows -> mat.numRows,
          KEY_rowmajor -> true,
          KEY_v -> 1
        )
      )
  }
  private def createFrameDescriptor[RX, CX, T: ST](
      frame: Frame[RX, CX, T]
  ) = dtype[T].map { dtype =>
    ujson
      .write(
        ujson.Obj(
          KEY_datatype -> dtype,
          KEY_colix -> frame.colIx.toSeq.map(_.toString),
          KEY_rowix -> frame.rowIx.toSeq.map(_.toString),
          KEY_rowmajor -> false,
          KEY_v -> 1
        )
      )
  }

  private def createHeader(
      descriptor: Either[String, String]
  ): Either[String, Array[Byte]] = {
    descriptor.map { descriptorJson =>
      val descriptor = {
        val json = descriptorJson
          .getBytes("UTF-8")
        val jsonLength = json.length
        val usefulLength = jsonLength + 12
        val padding = usefulLength / 16 + 16 - usefulLength
        json ++ Array.fill(padding)(' '.toByte)
      }
      val magic = "SADDLE".getBytes("US-ASCII")
      val version = Array(1.toByte, 0.toByte)
      val headerLength = int(descriptor.length)
      val header = magic ++ version ++ headerLength ++ descriptor
      header
    }
  }

  private[binary] def dtype[T: ST] = implicitly[ST[T]] match {
    case ScalarTagDouble => Right("double")
    case ScalarTagInt    => Right("int")
    case ScalarTagFloat  => Right("float")
    case ScalarTagLong   => Right("long")
    case ScalarTagByte   => Right("byte")
    case ScalarTagChar   => Right("char")
    case other           => Left(s"Type $other not supported.")
  }

  private[binary] def width[T: ST] = implicitly[ST[T]] match {
    case ScalarTagDouble => Right(8)
    case ScalarTagInt    => Right(4)
    case ScalarTagFloat  => Right(4)
    case ScalarTagLong   => Right(8)
    case ScalarTagChar   => Right(2)
    case ScalarTagByte   => Right(1)
    case other           => Left(s"Type $other not supported.")
  }

  private def put[@specialized(Double, Long, Int, Float, Byte) T: ST](
      startOffset: Int,
      t: Array[T],
      bb: ByteBuffer
  ) = implicitly[ST[T]] match {
    case ScalarTagDouble =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.putDouble(t(i))
          i += 1
        }
        i - startOffset
      }
    case ScalarTagInt =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.putInt(t(i).asInstanceOf[Int])
          i += 1
        }
        i - startOffset
      }
    case ScalarTagFloat =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.putFloat(t(i).asInstanceOf[Float])
          i += 1
        }
        i - startOffset
      }
    case ScalarTagLong =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.putLong(t(i).asInstanceOf[Long])
          i += 1
        }
        i - startOffset
      }
    case ScalarTagByte =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.put(t(i).asInstanceOf[Byte])
          i += 1
        }
        i
      }
    case ScalarTagChar =>
      Right {
        var i = startOffset
        val n = t.length
        while (i < n && bb.hasRemaining()) {
          bb.putChar(t(i).asInstanceOf[Char])
          i += 1
        }
        i
      }
    case other => Left(s"Type $other not supported.")
  }

  def writeMatIntoChannel[T: ST](
      mat: Mat[T],
      channel: WritableByteChannel
  ): Either[String, Unit] = {
    val header = createHeader(createMatDescriptor(mat))
    header.flatMap { header =>
      width[T].map { width =>
        writeFully(ByteBuffer.wrap(header), channel)

        mat.rows.foreach { row =>
          val bb = ByteBuffer
            .allocate(row.length * width)
            .order(ByteOrder.LITTLE_ENDIAN)
          put(0, row.toArray, bb)
          writeFully(bb, channel)
        }
      }
    }
  }

  private def writeFully(bb: ByteBuffer, channel: WritableByteChannel) = {
    bb.rewind
    while (bb.hasRemaining) {
      channel.write(bb)
    }
  }

  def writeFrameIntoChannel[RX, CX, T: ST](
      frame: Frame[RX, CX, T],
      channel: WritableByteChannel
  ): Either[String, Unit] = {

    val header = createHeader(createFrameDescriptor(frame))
    header.flatMap { header =>
      width[T].map { width =>
        writeFully(ByteBuffer.wrap(header), channel)

        frame.values.foreach { col =>
          val bb = ByteBuffer
            .allocate(col.length * width)
            .order(ByteOrder.LITTLE_ENDIAN)
          put(0, col.toArray, bb)
          writeFully(bb, channel)
        }
      }
    }
  }

  def writeMatIntoArray[T: ST](
      mat: Mat[T]
  ): Either[String, Array[Byte]] = {
    val header = createHeader(createMatDescriptor(mat))
    header.flatMap { header =>
      width[T].map { width =>
        val result =
          Array.ofDim[Byte](header.length + width * mat.numRows * mat.numCols)
        System.arraycopy(header, 0, result, 0, header.length)
        val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
        bb.position(header.length)

        val ar = mat.toArray
        put(0, ar, bb)
        result
      }
    }
  }
  def writeMatIntoArrays[T: ST](
      mat: Mat[T],
      maxArrayLength: Long = 2147483544
  ): Either[String, IndexedSeq[Array[Byte]]] = {
    val header = createHeader(createMatDescriptor(mat))
    header.flatMap { header =>
      width[T].map { width =>
        val ar = mat.toArray
        val totalLength: Long =
          width.toLong * ar.length
        var i = 0
        val numArrays =
          org.saddle.util.dividePositiveRoundUp(totalLength, maxArrayLength)
        header +: (0L until numArrays).map { arrayIdx =>
          val offset = arrayIdx * maxArrayLength
          val length = math.min(maxArrayLength, totalLength - offset).toInt
          val result =
            Array.ofDim[Byte](length)

          val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)

          val usedElements = put(i, ar, bb).toOption.get
          i += usedElements
          result
        }
      }
    }
  }
  def writeFrameIntoArray[RX, CX, T: ST](
      frame: Frame[RX, CX, T]
  ): Either[String, Array[Byte]] = {
    val header = createHeader(createFrameDescriptor(frame))
    header.flatMap { header =>
      width[T].map { width =>
        val result =
          Array.ofDim[Byte](
            header.length + width * frame.numRows * frame.numCols
          )
        System.arraycopy(header, 0, result, 0, header.length)
        val bb = ByteBuffer.wrap(result).order(ByteOrder.LITTLE_ENDIAN)
        bb.position(header.length)

        frame.values.foreach { col =>
          put(0, col.toArray, bb).toOption.get
        }
        result
      }
    }
  }

}
