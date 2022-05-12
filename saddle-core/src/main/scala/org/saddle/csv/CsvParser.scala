/** Copyright (c) 2013 Saddle Development Team
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
package org.saddle.csv

import org.saddle.{Frame, Vec, ST}
import org.saddle.order._
import org.saddle.Index
import scala.io.Source
import org.saddle.Buffer
import scala.{specialized => spec}
import java.nio.CharBuffer
import java.io.File
import java.nio.charset.CharsetDecoder

/** Csv parsing utilities
  */
object CsvParser {

  val asciiSilentCharsetDecoder = org.saddle.io.csv.asciiSilentCharsetDecoder

  def readFile(
      file: File,
      bufferSize: Int,
      charset: CharsetDecoder
  ): Iterator[CharBuffer] = {
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    org.saddle.io.csv.readChannel(channel, bufferSize, charset)
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
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] = {
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    try {
      parseFromIterator(
        org.saddle.io.csv.readChannel(channel, bufferSize, charsetDecoder),
        cols,
        fieldSeparator,
        quoteChar,
        recordSeparator,
        maxLines
      ).map(_._1)
    } finally {
      channel.close()
    }
  }

  def parseFileWithHeader[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      bufferSize: Int = 8192,
      maxLines: Long = Long.MaxValue,
      charsetDecoder: CharsetDecoder = asciiSilentCharsetDecoder
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] = {
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    try {
      parseFromIterator(
        org.saddle.io.csv.readChannel(channel, bufferSize, charsetDecoder),
        cols,
        fieldSeparator,
        quoteChar,
        recordSeparator,
        maxLines,
        header = true
      ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }
    } finally {
      channel.close()
    }
  }

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
    * @param cols
    *   The column offsets to parse (if empty, parse everything)
    * @param fieldSeparator
    *   The separator; default is comma
    * @param quoteChar
    *   Within matching quotes, treat separChar as normal char; default is
    *   double-quote
    * @param recordSeparator
    *   Record separator (line ending)
    * @param source
    *   The csv data source to operate on
    * @param maxLines
    *   The maximum number of records that will be read from the file. Includes
    *   header.
    * @param header
    *   indicates whether the first line should be set aside
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
        for { _ <- locs.toIndexedSeq } yield new Buffer[T](
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

    done.flatMap { colIndex =>
      val columns = bufdata map { b => Vec(b.toArray) }
      if (columns.map(_.length).distinct.size != 1)
        Left(s"Uneven length ${columns.map(_.length).toVector} columns")
      else
        Right((Frame(columns: _*), colIndex.map(i => Index(i))))
    }
  }

  private[csv] class DataBuffer(
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
          buffer = data.next()
          position = 0
        } else {
          buffer = concat(buffer, data.next())
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
