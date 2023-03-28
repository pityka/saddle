package org.saddle.spire
package util

import java.nio.ByteBuffer

/** These methods are all big-endian.
  *
  * That is, bytes[0] is the most-significant byte.
  */
private[spire] object Pack {

  @inline private[this] def lsm(n: Long, shift: Int): Byte =
    ((n >>> shift) & 0xffL).toByte

  def longToBytes(n: Long): Array[Byte] = {
    val arr = new Array[Byte](8)
    arr(0) = lsm(n, 56)
    arr(1) = lsm(n, 48)
    arr(2) = lsm(n, 40)
    arr(3) = lsm(n, 32)
    arr(4) = lsm(n, 24)
    arr(5) = lsm(n, 16)
    arr(6) = lsm(n, 8)
    arr(7) = lsm(n, 0)
    arr
  }

  def longFromBytes(bytes: Array[Byte]): Long =
    longFromByteBuffer(ByteBuffer.wrap(bytes))

  def longFromBytes(
      b1: Byte,
      b2: Byte,
      b3: Byte,
      b4: Byte,
      b5: Byte,
      b6: Byte,
      b7: Byte,
      b8: Byte
  ): Long =
    (b1 & 0xffL) << 56 | (b2 & 0xffL) << 48 | (b3 & 0xffL) << 40 |
      (b4 & 0xffL) << 32 | (b5 & 0xffL) << 24 | (b6 & 0xffL) << 16 |
      (b7 & 0xffL) << 8 | (b8 & 0xffL)

  def longFromByteBuffer(bb: ByteBuffer): Long =
    if (bb.remaining >= 8) {
      bb.getLong()
    } else {
      var n = 0L
      while (bb.remaining > 0) n = (n << 8) | bb.get
      n
    }

  def longsFromBytes(bytes: Array[Byte], n: Int): Array[Long] =
    longsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  def longsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Long] = {
    val out = new Array[Long](n)
    var i = 0
    while (i < n && bb.remaining >= 8) {
      out(i) = bb.getLong();
      i += 1
    }
    if (i < n && bb.remaining > 0) out(i) = longFromByteBuffer(bb)
    out
  }

}
