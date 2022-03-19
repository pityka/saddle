package org.saddle.io

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ByteOrder

package object npy {

  sealed trait DType[T] { def width: Int }
  case object DoubleType extends DType[Double] { val width = 8 }
  case object FloatType extends DType[Float] { val width = 4 }
  case object LongType extends DType[Long] { val width = 8 }
  case object ByteType extends DType[Byte] { val width = 1 }
  case object IntType extends DType[Int] { val width = 4 }

  case class Descriptor(fortran: Boolean, shape: List[Long], dtype: String)

  private[saddle] def readFully(bb: ByteBuffer, channel: ReadableByteChannel) : Unit = {
    bb.clear
    var i = 0
    while (bb.hasRemaining && i >= 0) {
      i = channel.read(bb)
    }
    bb.flip
  }

  def parse[T](
      tpe: DType[T],
      size: Int,
      from: ByteBuffer
  ): Either[String, Array[T]] =
    tpe match {
      case DoubleType =>
        Right {
          val to = DoubleBuffer.allocate(size)
          to.put(from.asDoubleBuffer)
          to.array
        }
      case IntType =>
        Right {
          val to = IntBuffer.allocate(size)
          to.put(from.asIntBuffer)
          to.array
        }
      case FloatType =>
        Right {
          val to = FloatBuffer.allocate(size)
          to.put(from.asFloatBuffer)
          to.array
        }
      case LongType =>
        Right {
          val to = LongBuffer.allocate(size)
          to.put(from.asLongBuffer)
          to.array
        }
      case ByteType =>
        Right {
          val to = ByteBuffer.allocate(size)
          to.asInstanceOf[ByteBuffer].put(from)
          to.array
        }
      case null => Left(s"null type not supported (likely programmer error).")
    }

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
    val shape = "'shape': \\((\\d+,)( \\d+,)*( \\d+)*\\)".r
      .findFirstMatchIn(s)
      .get
      .matched
      .split(":")
      .last
      .trim
      .drop(1)
      .dropRight(1)
      .split(",")
      .map(_.trim.toLong)
      .toList

    Descriptor(fortran, shape, dtype)
  }

  def readHeaderFromChannel(
      channel: ReadableByteChannel
  ) : Either[String, Descriptor] = {
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

      Right(descriptor)
    }
  }

  def readDataFromChannel[T](
      tpe: DType[T],
      channel: ReadableByteChannel,
      len: Long
  ): Iterator[Either[String, Array[T]]] = {
    val maxBufferSize = 1073741816
    val width = tpe.width
    val neededSize = width * len
    val m = (neededSize / maxBufferSize).toInt
    val m2 = (neededSize % maxBufferSize).toInt
    val arraySizes = (0 until m).map(_ => maxBufferSize) :+ m2
    arraySizes.iterator.map { size =>
      val bb = ByteBuffer
        .allocate(size)
        .order(ByteOrder.LITTLE_ENDIAN)
      readFully(bb, channel)

      assert((size / width) * width == size)

      parse(tpe, size / width, bb)
    }

  }

  def readFromChannel[T](
      dtype: DType[T],
      channel: ReadableByteChannel
  ): Either[String, (Descriptor, Iterator[Either[String, Array[T]]])] = {
    readHeaderFromChannel(channel).map { descriptor =>
      (
        descriptor,
        readDataFromChannel(
          dtype,
          channel,
          descriptor.shape.foldLeft(1L)(_ * _)
        )
      )

    }
  }

}
