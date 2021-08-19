package org.saddle.io

import java.nio.CharBuffer
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.CharsetDecoder
import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction

package object csv {

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
    @tailrec
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
  def parseFromIteratorCallback(
      source: Iterator[CharBuffer],
      cols: Seq[Int] = Nil,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n",
      maxLines: Long = Long.MaxValue,
      header: Boolean = false
  )(
      prepare: Int => Unit,
      dataCallback: (String, Int) => Unit
  ): Either[String, Option[Array[String]]] =
    if (fieldSeparator == quoteChar)
      Left("Separator character and quote character cannot be the same")
    else if (recordSeparator.size != 1 && recordSeparator.size != 2)
      Left(
        s"Record separator must have 1 or 2 characters. ${recordSeparator.toCharArray.map(_.toByte).toVector}"
      )
    else if (source.isEmpty || maxLines == 0)
      Right(None)
    else {

      val data = new DataBuffer(source, CharBuffer.allocate(0), 0, false)

      // sorted, unique column locations to parse
      var locs = Set(cols: _*).toArray[Int].sorted

      // parse first line
      val (firstLine, errorMessage) = {
        val buffer = new ArrayBuffer[String](1024)
        val callback: (String, Int) => Unit = (s: String, _: Int) => {
          buffer.append(s)
          ()
        }
        val errorMessage = extractFields(
          data,
          callback,
          Array.empty,
          quoteChar,
          fieldSeparator,
          recordSeparator.toCharArray,
          1
        )
        (buffer.toArray, errorMessage)
      }

      if (errorMessage.nonEmpty) Left(errorMessage)
      else {
        // what column locations to extract
        if (locs.length == 0) locs = (0 until firstLine.length).toArray

        prepare(locs.length)

        def addToBuffer(s: String, buf: Int) = {
          dataCallback(s, buf)
        }

        val fields = locs.map(i => firstLine(i)).toArray
        val colIndex = if (header) {
          Some(fields.toArray)
        } else {
          fields.toSeq.zipWithIndex.map { case (s, i) => addToBuffer(s, i) }
          None
        }

        // parse remaining rows
        val errorMessage = extractFields(
          data,
          addToBuffer,
          locs,
          quoteChar,
          fieldSeparator,
          recordSeparator.toCharArray,
          maxLines - 1
        )

        if (errorMessage.nonEmpty) Left(errorMessage)
        else {

          Right(colIndex)
        }
      }
    }

  private def extractFields(
      data: DataBuffer,
      callback: (String, Int) => Unit,
      locs: Array[Int],
      quoteChar: Char,
      separChar: Char,
      recordSeparator: Array[Char],
      maxLines: Long
  ) = {

    // 0 - init
    // 1 - non-escaped data
    // 2 - quoted data
    // 3 - quote in quoted data
    // 4 - CR in init
    // 5 - CR in data
    // 6 - CR in quote
    var state = 0

    var curField = 0 // current field of parse
    var curBegin = data.position // offset of start of current field in buffer
    var locIdx = 0 // current location within locs array
    var lineIdx = 0L
    var error = false
    var openS = false
    var errorMessage = ""
    val empty = ""

    val CR = recordSeparator.head
    val LF =
      if (recordSeparator.size == 2) recordSeparator.last
      else recordSeparator.head
    val singleRecordSeparator = recordSeparator.size == 1

    val allFields = locs.isEmpty

    def fail(str: String) = {
      error = true
      errorMessage = str + s".. line=$lineIdx, field=$curField"
    }

    def emit(offset: Int) = {
      if (allFields || (locs.size > locIdx && curField == locs(locIdx))) {
        callback(
          if (curBegin >= (data.position - offset)) empty
          else
            ((data.buffer: CharSequence)
              .subSequence(
                curBegin,
                data.position - offset
              ))
              .toString,
          locIdx
        )
        locIdx += 1
      }
    }

    def close() = {
      curField += 1
      openS = false
      data.save = false
    }
    def open(offset: Int) = {
      curBegin = data.position - offset
      data.save = true
    }
    def openNext() = {
      openS = true
      curBegin = data.position + 1
    }

    def newline() = {
      if (locIdx < locs.size) {
        fail(
          s"Incomplete line $lineIdx. Expected ${locs.size} fields, got $locIdx"
        )
      }
      lineIdx += 1
      locIdx = 0
      curField = 0
    }

    while (data.hasNext && lineIdx < maxLines && !error) {
      val chr = data.next
      if (state == 0) { // init
        if (chr == separChar) {
          open(0)
          emit(1)
          close()
          openNext()
        } else if (chr == quoteChar) {
          state = 2
          open(0)
        } else if (chr == CR) {
          if (singleRecordSeparator) {
            open(0)
            emit(1)
            close()
            newline()
          } else {
            state = 4
            open(1)
          }
        } else {
          state = 1
          open(1)
        }
      } else if (state == 1) { // data
        if (chr == separChar) {
          emit(1)
          close()
          openNext()
          state = 0
        } else if (chr == quoteChar) {
          fail("quote must not occur in unquoted field")
        } else if (chr == CR) {
          if (singleRecordSeparator) {
            emit(1)
            close()
            state = 0
            newline()
          } else {
            state = 5
          }
        }
      } else if (state == 2) { //quoted data
        if (chr == quoteChar) {
          state = 3
        }
      } else if (state == 3) { // quote in quoted data
        if (chr == quoteChar) {
          state = 2
        } else if (chr == separChar) {
          emit(2)
          close()
          openNext()
          state = 0
        } else if (chr == CR) {
          if (singleRecordSeparator) {
            emit(2)
            close()
            state = 0
            newline()
          } else {
            state = 6
          }
        } else {
          fail(
            "quotes in quoted field must be escaped by doubling them"
          )
        }
      } else if (state == 4) { // CR in init
        if (chr == LF) {
          emit(2)
          close()
          state = 0
          newline()
        } else if (chr == separChar) {
          callback(s"$CR", locIdx)
          locIdx += 1
          curField += 1
          openNext()
          state = 0
        } else if (chr == quoteChar) {
          fail("invalid quote")
        } else if (chr == CR) {
          state = 5
        } else {
          state = 1
        }
      } else if (state == 5) { // CR in data
        if (chr == LF) {
          emit(2)
          close()
          state = 0
          newline()
        } else if (chr == separChar) {
          emit(1)
          close()
          openNext()
          state = 0
        } else if (chr == quoteChar) {
          fail("invalid quote")
        } else if (chr == CR) {
          state = 5
        } else {
          state = 1
        }
      } else if (state == 6) { // CR in quote
        if (chr == LF) {
          emit(3)
          close()
          state = 0
          newline()
        } else {
          fail("Closing quote followed by data")
        }
      }
    }

    if (state == 1) {
      emit(0)
    } else if (state == 3) {
      emit(1)
    } else if (state == 2) {
      fail("Unclosed quote")
    } else if (state == 0 && openS) {
      emit(0)
    }
    openS = false

    errorMessage

  }

  def readChannel(
      channel: ReadableByteChannel,
      bufferSize: Int,
      charset: CharsetDecoder
  ): Iterator[CharBuffer] = {
    val buffer = java.nio.ByteBuffer.allocate(bufferSize)
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
      def next() = {
        fillBuffer()
        charset.decode(buffer)
      }
    }
  }

  val asciiSilentCharsetDecoder = Charset
    .forName("US-ASCII")
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)
}
