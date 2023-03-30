package org.saddle

import org.openjdk.jmh.annotations._
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class CSVParserBench {

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

  }

  val noop = new org.saddle.io.csv.Callback {
    def apply(s: Array[Char], from: Array[Int], to: Array[Int], len: Int) =
      org.saddle.io.csv.Next
  }

  @Param(Array("5000"))
  var size: Int = _

  var v1: Array[Byte] = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = {
      val str = 0 until size map { _ =>
        val line = 0 until 20 map { _ =>
          scala.util.Random.nextLong().toString
        } mkString (",")

        line
      } mkString ("\n")
      str.getBytes
    }
    print(f"Data size ${v1.size.toDouble / (1024 * 1024)}%.2f MB ")
  }
  @Benchmark
  def csvparser(): Unit = {

    val byteChannel = ByteChannel(v1)

    org.saddle.io.csv.parse(
      byteChannel,
      noop,
      recordSeparator = "\n"
    )

  }

}
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class CSVParserBenchCRLF {

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

  }

  val noop = new org.saddle.io.csv.Callback {
    def apply(s: Array[Char], from: Array[Int], to: Array[Int], len: Int) =
      org.saddle.io.csv.Next
  }

  @Param(Array("5000"))
  var size: Int = _

  var v1: Array[Byte] = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = {
      val str = 0 until size map { _ =>
        val line = 0 until 20 map { _ =>
          scala.util.Random.nextLong().toString
        } mkString (",")

        line
      } mkString ("\r\n")
      str.getBytes
    }
    print(f"Data size ${v1.size.toDouble / (1024 * 1024)}%.2f MB ")
  }
  @Benchmark
  def csvparser(): Unit = {

    val byteChannel = ByteChannel(v1)

    org.saddle.io.csv.parse(
      byteChannel,
      noop,
      recordSeparator = "\r\n"
    )

  }

}
