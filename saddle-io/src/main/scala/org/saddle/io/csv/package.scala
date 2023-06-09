package org.saddle.io

import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.CharsetDecoder
import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction
import java.nio.charset.CoderResult

package object csv {

  sealed trait Control
  case object Done extends Control
  case object Next extends Control
  case class Error(error: String) extends Control

  trait Callback {

    /** Callback of CSV parser.
      *
      * Called whenever the parser detects tokens in the buffer.
      *
      * @param chars
      *   Character array. This is a shared array owned by the parser. Do not
      *   modify, copy out whatever is needed.
      * @param from
      *   Int array of indices of starting offsets of tokens in the `char`
      *   array. Owned by the parser.
      * @param to
      *   Int array of indices of end (exclusive) offsets of tokens in the
      *   `char` array. Owned by the parser.
      * @param len
      *   Number of tokens. Do not read the index arrays beyond this number.
      * @return
      *   a control object which can be Done, Next or Error. The parser stops on
      *   Done. It continues on Next and it stops and reports error on Error
      */
    def apply(
        chars: Array[Char],
        from: Array[Int],
        to: Array[Int],
        len: Int
    ): Control
  }

  /** Parse CSV files according to RFC 4180
    *
    * @param fieldSeparator
    *   The separator; default is comma
    * @param quoteChar
    *   Within matching quotes, treat separChar as normal char; default is
    *   double-quote
    * @param recordSeparator
    *   Record separator (line ending). Its length must be one or two.
    * @param channel
    *   The csv data channel to read from
    * @param callback
    *   An instance of the `Callback` which will be called on a group of tokens.
    * @param charset
    *   A charset decoder. Must be in new or reset state.
    * @param bufferSize
    *   Must be larger than the longest token (CSV cell) in the data.
    */
  @noinline
  def parse(
      channel: ReadableByteChannel,
      callback: Callback,
      charset: CharsetDecoder = makeAsciiSilentCharsetDecoder,
      bufferSize: Int = 8192,
      fieldSeparator: Char = ',',
      quoteChar: Char = '"',
      recordSeparator: String = "\r\n"
  ): Option[String] =
    if (fieldSeparator == quoteChar)
      Some("Separator character and quote character cannot be the same")
    else if (recordSeparator.size != 1 && recordSeparator.size != 2)
      Some(
        s"Record separator must have 1 or 2 characters. ${recordSeparator.toCharArray.map(_.toByte).toVector}"
      )
    else {

      val source = readChannel(channel, bufferSize, charset)

      val data =
        if (recordSeparator.size == 1)
          new DataBuffer1(
            data = source,
            quoteChar = quoteChar,
            fieldSeparator = fieldSeparator,
            recordSeparator = recordSeparator.head,
            bufferSize
          )
        else
          new DataBuffer2(
            data = source,
            quoteChar = quoteChar,
            fieldSeparator = fieldSeparator,
            recordSeparator1 = recordSeparator.head,
            recordSeparator2 = recordSeparator(1),
            bufferSize
          )
      var error = false
      var done = false
      var errorString = ""
      while (data.hasNext && !error && !done) {
        val (chars, from, to, len) = data.nextBatch
        callback(chars, from, to, len) match {
          case Done => done = true
          case Next => ()
          case Error(err) =>
            error = true
            errorString = err
        }
      }

      if (data.bufferTooShort) {
        error = true
        errorString = "Buffer too short for this data."
      }

      if (!error && !done) {
        val (chars, from, to, len) = data.emitRest
        callback(chars, from, to, len) match {
          case Error(err) =>
            error = true
            errorString = err
          case _ =>
        }
      }

      if (error) Some(errorString)
      else None

    }

  private[csv] def readChannel(
      channel: ReadableByteChannel,
      bufferSize: Int,
      charset: CharsetDecoder
  ): Iterator[CharBuffer] = {
    require(bufferSize >= 4, "Input buffer must be at least 4 length")
    def format(b: ByteBuffer) = 0 until b.remaining() map (i =>
      b.get(b.position() + i)
    ) mkString ("[", ", ", "]")

    val buffer = java.nio.ByteBuffer.allocate(bufferSize)
    val charBuffer = java.nio.CharBuffer.allocate(bufferSize)
    charBuffer.limit(0)

    var eof = false
    var decFlushed = false
    charset.reset()
    def fillBuffer(): Unit = if (!eof) {
      var count = channel.read(buffer)
      while (count >= 0 && buffer.remaining > 0) {
        count = channel.read(buffer)
      }

      buffer.flip
      charBuffer.compact()
      val result = charset.decode(buffer, charBuffer, count < 0)
      if (result.isError())
        throw new RuntimeException(
          s"Decoder error $result, buffer=${format(buffer)}, charbuffer='${charBuffer}'"
        )
      buffer.compact
      if (count < 0 && result == CoderResult.UNDERFLOW) {

        eof = true
        val result = charset.flush(charBuffer)
        charBuffer.flip
        if (result == CoderResult.UNDERFLOW) { decFlushed = true }
      } else {
        charBuffer.flip

      }

    } else {
      charBuffer.compact
      val result = charset.flush(charBuffer)
      charBuffer.flip
      if (result == CoderResult.UNDERFLOW) { decFlushed = true }
      else
        throw new RuntimeException(
          s"Decoder flush error or overflow for the second time ($result), buffer=${format(buffer)}, charbuffer='${charBuffer}. If this is an overflow then the caller must provide a larger buffer.'"
        )
    }
    new Iterator[CharBuffer] {
      def hasNext = !decFlushed
      def next() = {
        fillBuffer()
        charBuffer
      }
    }
  }

  def makeAsciiSilentCharsetDecoder: CharsetDecoder = {
    Charset
      .forName("US-ASCII")
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)
  }
}
