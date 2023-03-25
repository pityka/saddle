package org.saddle.io.csv

private[csv] object BitSet {

  final val LogWL = 6
  final val WordLength = 64
  final val MaxSize = (Int.MaxValue >> LogWL) + 1

  def allocate(i: Int) = new BitSet(
    Array.ofDim[Long](math.ceil(i / 64.0).toInt)
  )
  def wrap(bb: Array[Byte]) = {
    val bs = allocate(bb.length)
    bb.zipWithIndex.foreach { case (i, idx) =>
      if (i != 0) bs.addOne(idx)
    }
    bs
  }

}

private[csv] final class BitSet(final val elems: Array[Long]) {
  import BitSet._

  def array = (0 until elems.size * WordLength).map { i =>
    if (contains(i)) 1 else 0
  } toArray

  override def toString = {
    (0 until elems.size * WordLength).map { i =>
      if (contains(i)) "1" else "0"
    }.mkString
  }

  @inline
  val capacity = elems.length * WordLength

  @inline
  def min(from: Int) = {
    val minIdx = from >> LogWL
    var minRemainder = from & 63
    var i = minIdx
    val n = elems.length
    var c = 0L
    while (i < n && c == 0L) {
      val x = elems(i)
      c = (x >> minRemainder) << minRemainder
      i += 1
      minRemainder = 0
    }
    if (c == 0L) -2
    else java.lang.Long.numberOfTrailingZeros(c & -c) + (i - 1) * 64
  }

  @inline
  def contains(elem: Int): Boolean = {
    val idx = elem >> LogWL
    val n = elems.length

    0 <= elem && n != 0 && n > idx && (elems(idx) & (1L << elem)) != 0L
  }

  @inline
  def get(elem: Int): Long = {
    val idx = elem >> LogWL
    if (elems.length <= idx) 0L
    else {
      val w = elems(idx)
      val r = elem & 63
      (w & (1L << r)) >>> r
    }
  }

  @inline
  private final def nwords: Int = elems.length

  def addOne(elem: Int) = {
    val idx = elem >> LogWL
    val r = elem & 63
    elems(idx) = elems(idx) | (1L << r)
  }

  def shiftForwardInPlace(fill: Long): Unit = {
    var f = fill
    var i = 0
    val n = elems.length
    while (i < n) {
      val e = elems(i)
      elems(i) = e << 1 | f
      f = e >>> 63
      i += 1
    }

  }

  def |(other: BitSet): BitSet = {
    var i = 0
    val othernwords = other.nwords
    val o = BitSet.allocate(math.max(this.capacity, other.capacity))
    while (i < othernwords) {
      o.elems(i) = elems(i) | other.elems(i)
      i += 1
    }
    o
  }
  def &(other: BitSet): BitSet = {
    var i = 0
    val othernwords = other.nwords
    val o = BitSet.allocate(math.max(this.capacity, other.capacity))
    while (i < othernwords) {
      o.elems(i) = elems(i) & other.elems(i)
      i += 1
    }
    o
  }
  def negateInplace(): Unit = {
    var i = 0
    while (i < nwords) {
      elems(i) = ~elems(i)
      i += 1
    }
  }
  def zero(): Unit = {
    var i = 0
    while (i < nwords) {
      elems(i) = 0
      i += 1
    }
  }

  def prefixSumXor(): BitSet = {
    // https://nullprogram.com/blog/2021/12/04/
    // uint32_t r = x;
    // while (x) {
    //     r ^= -x ^ x;
    //     x &= x - 1;
    // }
    // return r;
    val carry = 0L // or -1L

    val o = Array.ofDim[Long](elems.length)
    var i = 0
    val n = elems.length

    while (i < n) {
      var x = elems(i)
      var r = x
      while (x != 0L) {
        r ^= -x ^ x
        x &= x - 1
      }
      o(i) = r ^ carry
      x = r >>> 63
      i += 1
    }

    new BitSet(o)

  }

  def &=(other: BitSet): this.type = {
    var i = 0
    val thisnwords = nwords
    while (i < thisnwords) {
      elems(i) = elems(i) & other.elems(i)
      i += 1
    }
    this
  }

}
