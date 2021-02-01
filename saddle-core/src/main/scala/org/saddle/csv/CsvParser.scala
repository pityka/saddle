/** Copyright (c) 2013 Saddle Development Team
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
package org.saddle.csv

import org.saddle.{Frame, Vec, ST, Index}
import org.saddle.order._
import org.saddle.Index
import scala.io.Source
import org.saddle.Buffer
import scala.{specialized => spec}
import java.nio.CharBuffer
import java.io.File
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction

/** Csv parsing utilities
  */
object CsvParser {

  val asciiSilentCharsetDecoder = Charset
    .forName("US-ASCII")
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)

  def readFile(
      file: File,
      bufferSize: Int,
      charset: CharsetDecoder
  ): Iterator[CharBuffer] = {
    val buffer = java.nio.ByteBuffer.allocate(bufferSize)
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    var eof = false
    def fillBuffer() = {
      buffer.clear()
      var count = channel.read(buffer)
      while (count >= 0 && buffer.remaining > 0) {
        count = channel.read(buffer)
      }
      if (count < 0) {
        eof = true
      }
      buffer.flip
    }
    new Iterator[CharBuffer] {
      def hasNext = !eof
      def next = {
        fillBuffer()
        charset.decode(buffer)
      }
    }
  }

  def parseFile[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue,
      charsetDecoder: CharsetDecoder = asciiSilentCharsetDecoder
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] =
    parseFromIterator(
      readFile(file, bufferSize, charsetDecoder),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines
    ).map(_._1)

  def parseFileWithHeader[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue,
      charsetDecoder: CharsetDecoder = asciiSilentCharsetDecoder
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] =
    parseFromIterator(
      readFile(file, bufferSize, charsetDecoder),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines,
      header = true
    ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }

  def parseSource[@spec(Int, Double, Long, Float) T](
      source: Source,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] =
    parseFromIterator(
      source.grouped(bufferSize).map(v => CharBuffer.wrap(v.toArray)),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines
    ).map(_._1)

  def parseSourceWithHeader[@spec(Int, Double, Long, Float) T](
      source: Source,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] =
    parseFromIterator(
      source.grouped(bufferSize).map(v => CharBuffer.wrap(v.toArray)),
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines,
      header = true
    ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }

  /** Parse CSV files according to RFC 4180
    *
    * @param cols The column offsets to parse (if empty, parse everything)
    * @param fieldSeparator The separator; default is comma
    * @param quoteChar Within matching quotes, treat separChar as normal char;
    *                  default is double-quote
    * @param recordSeparator Record separator (line ending)
    * @param source The csv data source to operate on
    * @param maxLines The maximum number of records that will be read from the file. Includes header.
    * @param header indicates whether the first line should be set aside
    */
  def parseFromIterator[@spec(Int, Double, Long, Float) T](
      source: Iterator[CharBuffer],
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      maxLines: Long = Long.MaxValue,
      header: Boolean = false
  )(implicit
      st: ST[T]
  ): Either[String, (Frame[Int, Int, T], Option[Index[String]])] = {

    var locs = Set(cols: _*).toArray[Int].sorted

    var bufdata: Seq[Buffer[T]] = null

    def prepare(headerLength: Int) = {

      if (locs.length == 0) locs = (0 until headerLength).toArray

      // set up buffers to store parsed data
      bufdata =
        for { _ <- locs } yield new Buffer[T](
          Array.ofDim[T](1024),
          0
        )
    }

    def addToBuffer(s: String, buf: Int) = {
      import scala.Predef.{wrapRefArray => _}
      bufdata(buf).+=(st.parse(s))
    }

    val done = org.saddle.io.csv.parseFromIteratorCallback(
      source,
      cols,
      fieldSeparator,
      quoteChar,
      recordSeparator,
      maxLines,
      header
    )(prepare, addToBuffer)

    done.right.flatMap { colIndex =>
      val columns = bufdata map { b => Vec(b.toArray) }
      if (columns.map(_.length).distinct.size != 1)
        Left(s"Uneven length ${columns.map(_.length).toVector} columns")
      else
        Right((Frame(columns: _*), colIndex.map(i => Index(i))))
    }
  }

  // this is here to maintain binary compatibility
  @scala.annotation.nowarn
  private class DataBuffer(
      data: Iterator[CharBuffer],
      var buffer: CharBuffer,
      var position: Int,
      var save: Boolean
  ) {
    private def concat(buffer1: CharBuffer, buffer2: CharBuffer) = {
      val b = CharBuffer.allocate(buffer1.remaining + buffer2.remaining)
      b.put(buffer1)
      b.put(buffer2)
      b.flip
      b
    }
    @scala.annotation.tailrec
    private def fillBuffer: Boolean = {
      if (!data.hasNext) false
      else {
        if (!save) {
          buffer = data.next
          position = 0
        } else {
          buffer = concat(buffer, data.next)
        }
        if (buffer.length > position) true
        else fillBuffer
      }
    }
    @inline final def hasNext = position < buffer.length || fillBuffer

    @inline final def next =
      if (position < buffer.length) {
        val c = buffer.get(position)
        position += 1
        c
      } else {
        fillBuffer
        val c = buffer.get(position)
        position += 1
        c
      }
  }

}
