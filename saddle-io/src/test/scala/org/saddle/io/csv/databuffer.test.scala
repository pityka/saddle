package org.saddle.io.csv

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class DataBufferSpec extends Specification with ScalaCheck {
  val crlf = "\r\n"
  val lf = "\n"
  import DataBuffer._

  final def firstOne(o: BitSet, from: Int) = {
    o.min(from)

  }

  "bitset" in {
    val bs = BitSet.allocate(100)
    // println(BitSet.wrap(Array(0, 1, 0, 0 )))
    bs.addOne(0)
    bs.addOne(1)
    bs.addOne(2)
    // println(bs)
    1 must_== 1
  }
  "bitset shift" in {
    val bs = BitSet.allocate(100)
    // println(BitSet.wrap(Array(0, 1, 0, 0 )))
    bs.addOne(0)
    bs.addOne(1)
    bs.addOne(2)
    bs.addOne(63)
    bs.addOne(64)
    bs.addOne(65)
    bs.shiftForwardInPlace(0)
    bs.shiftForwardInPlace(1)
    bs.shiftForwardInPlace(0)
    // println(bs.shiftForward(0))
    // println(bs.shiftForward(0).shiftForward(1))
    1 must_== 1
  }

  "crlfmask works" in {

    makeMaskFromCrLfInPlace(
      BitSet.wrap(Array(0, 1, 0, 0)),
      BitSet.wrap(Array(0, 0, 1, 0))
    ).array.toList must_== (List(0, 0, 1, 0) ++ List.fill(60)(0))

    // makeMaskFromCrLfInPlace(
    //   BitSet.wrap(Array( 0, 0 )),
    //   BitSet.wrap(Array( 1, 0)),
    //   1

    // ).array.toList must_== (List(1, 0)++ List.fill(62)(0))
    makeMaskFromCrLfInPlace(
      BitSet.wrap(Array(0, 0)),
      BitSet.wrap(Array(1, 0))
    ).array.toList must_== (List(0, 0) ++ List.fill(62)(0))

  }
  // "remove unneeded quotes" in {

  //   removeUnneededQuotes(
  //     CharBuffer.wrap(Array('a', 'b', 'c', 'd', 'e')),
  //     // BitSet.wrap(Array(0, 0, 0, 0, 0))
  //   ).array.toList must_== List('a', 'b', 'c', 'd', 'e')

  //   val tmp1 = removeUnneededQuotes(
  //     CharBuffer.wrap(Array('"', 'b', 'c', 'd', '"')),
  //     // BitSet.wrap(Array(0, 0, 0, 0, 0))
  //   )
  //   tmp1.get must_== 'b'
  //   tmp1.get must_== 'c'
  //   tmp1.get must_== 'd'
  //   tmp1.hasRemaining must_== false

  //   val tmp2 = removeUnneededQuotes(
  //     CharBuffer.wrap(Array('"', 'b', 'c', 'd', '"')),
  //     // ByteBuffer.wrap(Array(0, 1, 0, 1, 0))
  //   )
  //   println(tmp2)
  //   tmp2.get must_== 'c'
  //   tmp2.hasRemaining must_== false

  // }

  "prefix sum xor  works" in {
    prefixSumXor(
      BitSet.wrap(Array(0, 0, 1, 1))
    ).array.toList must_== List(0, 0, 1, 0) ++ List.fill(60)(0)
    // prefixSumXor(
    //   BitSet.wrap(Array(0, 0, 1, 1)),
    //   true
    // ).array.toList must_== List(1, 1, 0, 1) ++ List.fill(60)(1)
    prefixSumXor(
      BitSet.wrap(Array(1, 0, 0, 1, 1, 0, 1))
    ).array.toList must_== List(1, 1, 1, 0, 1, 1, 0) ++ List.fill(64 - 7)(0)
  }
  "first one works" in {

    firstOne(BitSet.wrap(Array(0, 0, 1, 1)), 0) must_== 2
    firstOne(BitSet.wrap(Array(0, 0, 1, 1)), 1) must_== 2
    firstOne(BitSet.wrap(Array(0, 0, 1, 1)), 2) must_== 2
    firstOne(BitSet.wrap(Array(0, 0, 1, 1)), 3) must_== 3

    firstOne(
      {
        val bs = BitSet.allocate(256)
        bs.addOne(64)
        bs
      },
      0
    ) must_== 64
    firstOne(
      {
        val bs = BitSet.allocate(256)
        bs.addOne(65)
        bs
      },
      0
    ) must_== 65
    firstOne(
      {
        val bs = BitSet.allocate(256)
        bs.addOne(128)
        bs.addOne(65)
        bs
      },
      66
    ) must_== 128
    firstOne(
      {
        val bs = BitSet.allocate(256)
        bs.addOne(129)
        bs.addOne(65)
        bs
      },
      66
    ) must_== 129
  }

  "data buffer works 4" in {
    val data =
      s"""a,a"""

    val iter = ByteChannel.prep(data)
    val buffer =
      new org.saddle.io.csv.DataBuffer1(iter, '"', ',', '[', 4)
    buffer.hasNext must_== true
    val (chars : Array[Char], from, to, len) = buffer.nextBatch
 
    chars.toList must_== List(97, 44, 97, 0)
    from.toList must_== List(0, 2, -1, 0,0,0)
    to.toList must_== List(1, 3, 0, 0,0,0)
    len must_== 1
    buffer.hasNext must_== false
    val (chars2, from2, to2, len2) = buffer.emitRest
    chars2.toList must_== List(97, 44, 97, 0)
    from2.toList must_== List(2, 2, -1, 0,0,0)
    to2.toList must_== List(3, 3, 0, 0,0,0)
    len2 must_== 1

    1 must_== 1

  }
}
