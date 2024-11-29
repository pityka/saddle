package org.saddle.csv

import java.nio.charset.Charset
import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

private[saddle] class ByteChannel(src: ByteBuffer) extends ReadableByteChannel {
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
  def apply(s: String, charset: Charset = StandardCharsets.UTF_8) = {
    val data =
      ByteBuffer.wrap(s.getBytes(charset))
    new ByteChannel(data)
  }

}
