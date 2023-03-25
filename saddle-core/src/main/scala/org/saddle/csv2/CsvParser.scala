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
package org.saddle.csv2

import org.saddle.{Frame, Vec, ST}
import org.saddle.order._
import org.saddle.Index
import scala.io.Source
import org.saddle.Buffer
import scala.{specialized => spec}
import java.nio.CharBuffer
import java.io.File
import java.nio.charset.CharsetDecoder
import java.nio.channels.ReadableByteChannel
import org.saddle.io.csv2.Callback
import org.saddle.io.csv2.Control

/** Csv parsing utilities
  */
object CsvParser {

  def makeAsciiSilentCharsetDecoder: CharsetDecoder =
    org.saddle.io.csv2.makeAsciiSilentCharsetDecoder

  def parseFile[@spec(Int, Double, Long, Float) T](
      file: File,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      maxLines: Long = Long.MaxValue,
      charset: CharsetDecoder = makeAsciiSilentCharsetDecoder,
      bufferSize: Int = 8192
  )(implicit st: ST[T]): Either[String, Frame[Int, Int, T]] = {
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    try {
      parseFromChannel(
        channel = channel,
        cols = cols,
        fieldSeparator = fieldSeparator,
        quoteChar = quoteChar,
        recordSeparator = recordSeparator,
        maxLines = maxLines,
        header = false,
        charset = charset,
        bufferSize = bufferSize
      ).map { case (frame, _) => frame}
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
      maxLines: Long = Long.MaxValue,
      charset: CharsetDecoder = makeAsciiSilentCharsetDecoder,
      bufferSize: Int = 8192
  )(implicit st: ST[T]): Either[String, Frame[Int, String, T]] = {
    val is = new java.io.FileInputStream(file)
    val channel = is.getChannel
    try {
      parseFromChannel(
        channel = channel,
        cols = cols,
        fieldSeparator = fieldSeparator,
        quoteChar = quoteChar,
        recordSeparator = recordSeparator,
        maxLines = maxLines,
        header = true,
        charset = charset,
        bufferSize = bufferSize
      ).map { case (frame, colIndex) => frame.setColIndex(colIndex.get) }
    } finally {
      channel.close()
    }
  }

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
  def parseFromChannel[@spec(Int, Double, Long, Float) T](
      channel: ReadableByteChannel,
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      maxLines: Long = Long.MaxValue,
      header: Boolean = false,
      charset: CharsetDecoder = makeAsciiSilentCharsetDecoder,
      bufferSize: Int = 8192
  )(implicit
      st: ST[T]
  ): Either[String, (Frame[Int, Int, T], Option[Index[String]])] = {
    val locs = Set(cols: _*).toArray[Int].sorted

    val callback = new ColumnBufferCallback[T](locs, maxLines, header)

    val done: Option[String] = org.saddle.io.csv2.parse(
      channel = channel,
      callback = callback,
      bufferSize = bufferSize,
      charset = charset,
      fieldSeparator = fieldSeparator,
      quoteChar = quoteChar,
      recordSeparator = recordSeparator
    )

    done match {
      case Some(error) => Left(error)
      case None =>
        val colIndex = if (header) Some(callback.headerFields) else None
        val columns = callback.bufdata.map(b => Vec(b.toArray)).toVector
        if (locs.length > 0 && callback.numFields != locs.length) {

          Left(
            s"Header line to short for given locs: ${locs.mkString("[", ", ", "]")}. Header line: ${callback.allHeaderFields
              .mkString("[", ", ", "]")}"
          )
        } else if (columns.map(_.length).distinct.size != 1)
          Left(s"Uneven length ${columns.map(_.length).toVector} columns")
        else
          Right((Frame(columns: _*), colIndex.map(i => Index(i.toVector: _*))))
    }

  }

  class ColumnBufferCallback[@spec(Int, Double, Long, Float) T](
      locs: Array[Int],
      maxLines: Long,
      header: Boolean
  )(implicit
      st: ST[T]
  ) extends Callback {

    val locsIdx = Index(locs)

    val headerFields = scala.collection.mutable.ArrayBuffer[String]()
    val allHeaderFields = scala.collection.mutable.ArrayBuffer[String]()

    val bufdata = scala.collection.mutable.ArrayBuffer[Buffer[T]]()
    var numFields = 0


    val emptyLoc = locs.length == 0

    private final def add(s: Array[Char],from:Int,to:Int, buf: Int) = {
      import scala.Predef.{wrapRefArray => _}
      bufdata(buf).+=(st.parse(s,from, to))
    }

    var loc = 0
    var line = 0L
    def apply(
        s: Array[Char],
        from: Array[Int],
        to: Array[Int],
        len: Int
    ): org.saddle.io.csv2.Control = {
      var i = 0

      var error = false
      var errorString = ""

      while (i < len && line < maxLines && !error) {
        val fromi = from(i)
        val toi = to(i)
        val ptoi = math.abs(toi)
        if (line == 0) {
          allHeaderFields.append(new String(s,fromi,ptoi-fromi))
          if (emptyLoc || locsIdx.contains(loc)) {
            bufdata.append(Buffer.empty[T](8192))
            numFields += 1
          }
        }

        if (emptyLoc || locsIdx.contains(loc)) {
          if (header && line == 0) {
            headerFields.append(new String(s,fromi,ptoi-fromi))
          } else {
            if (loc >= numFields) {
              error = true
              errorString =
                s"Too long line ${line + 1} (1-based). Expected $numFields fields, got ${loc + 1}."
            } else {
              add(s,fromi,ptoi, loc)
            }
          }
        }

        if (toi < 0) {
          if (line == 0 && !emptyLoc && numFields != locs.length) {
            error = true
            errorString =
              s"Header line to short for given locs: ${locs.mkString("[", ", ", "]")}. Header line: ${allHeaderFields
                .mkString("[", ", ", "]")}"
          }
          if (loc < numFields - 1) {
            error = true
            errorString =
              s"Too short line ${line + 1} (1-based). Expected $numFields fields, got ${loc + 1}."
          }

          loc = 0
          line += 1
        } else loc += 1
        i += 1
      }

      if (error) org.saddle.io.csv2.Error(errorString)
      else if (line >= maxLines) org.saddle.io.csv2.Done
      else org.saddle.io.csv2.Next
    }

  }

}
