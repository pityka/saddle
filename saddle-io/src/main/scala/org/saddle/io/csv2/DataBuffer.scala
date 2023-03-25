package org.saddle.io.csv2

import java.nio.CharBuffer
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.CharsetDecoder
import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction

private[csv2] object DataBuffer {

  def elementWiseEqual3TSV(
      a: CharBuffer,
      o1: BitSet,
      o2: BitSet,
      o3: BitSet
  ): Unit = {
    var i = 0
    val n = a.remaining()
    val b = a.array()
    val off = a.arrayOffset()

    val a1 = o1.elems
    val a2 = o2.elems
    val a3 = o3.elems

    // unroll
    while (i + 7 < n) {
      val ch0 = b(off + i)
      val ch1 = b(off + i + 1)
      val ch2 = b(off + i + 2)
      val ch3 = b(off + i + 3)
      val ch4 = b(off + i + 4)
      val ch5 = b(off + i + 5)
      val ch6 = b(off + i + 6)
      val ch7 = b(off + i + 7)

      (ch0: @scala.annotation.switch) match {
        case '"' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch1: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch2: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch3: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch4: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch5: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch6: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch7: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a2(idx) |= sL
        case '\t' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a3(idx) |= sL
        case _ =>
      }

      i += 8
    }

    while (i < n) {
      val ch = b(off + i)

      ch match {
        case '"' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        }
        case '\n' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        }

        case '\t' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        }
        case _ =>
      }

      i += 1
    }

  }
  def elementWiseEqual3CSV_LF(
      a: CharBuffer,
      o1: BitSet,
      o2: BitSet,
      o3: BitSet
  ): Unit = {
    var i = 0
    val n = a.remaining()
    val b = a.array()
    val off = a.arrayOffset()

    val a1 = o1.elems
    val a2 = o2.elems
    val a3 = o3.elems

    // unroll
    while (i + 7 < n) {
      val ch0 = b(off + i)
      val ch1 = b(off + i + 1)
      val ch2 = b(off + i + 2)
      val ch3 = b(off + i + 3)
      val ch4 = b(off + i + 4)
      val ch5 = b(off + i + 5)
      val ch6 = b(off + i + 6)
      val ch7 = b(off + i + 7)

      (ch0: @scala.annotation.switch) match {
        case '"' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch1: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch2: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch3: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch4: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch5: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch6: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a3(idx) |= sL
        case _ =>
      }

      (ch7: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a2(idx) |= sL
        case ',' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a3(idx) |= sL
        case _ =>
      }

      i += 8
    }

    while (i < n) {
      val ch = b(off + i)

      ch match {
        case '"' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        }
        case '\n' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        }

        case ',' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        }
        case _ =>
      }

      i += 1
    }

  }
  def elementWiseEqual4RFC(
      a: CharBuffer,
      o1: BitSet,
      o2: BitSet,
      o3: BitSet,
      o4: BitSet
  ): Unit = {
    var i = 0
    val n = a.remaining()
    val b = a.array()
    val off = a.arrayOffset()

    val a1 = o1.elems
    val a2 = o2.elems
    val a3 = o3.elems
    val a4 = o4.elems

    // unroll
    while (i + 7 < n) {
      val ch0 = b(off + i)
      val ch1 = b(off + i + 1)
      val ch2 = b(off + i + 2)
      val ch3 = b(off + i + 3)
      val ch4 = b(off + i + 4)
      val ch5 = b(off + i + 5)
      val ch6 = b(off + i + 6)
      val ch7 = b(off + i + 7)

      (ch0: @scala.annotation.switch) match {
        case '"' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch1: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 1) >> BitSet.LogWL
          val sL = 1L << ((i + 1) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch2: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 2) >> BitSet.LogWL
          val sL = 1L << ((i + 2) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch3: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 3) >> BitSet.LogWL
          val sL = 1L << ((i + 3) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch4: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 4) >> BitSet.LogWL
          val sL = 1L << ((i + 4) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch5: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 5) >> BitSet.LogWL
          val sL = 1L << ((i + 5) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch6: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 6) >> BitSet.LogWL
          val sL = 1L << ((i + 6) & 63)
          a4(idx) |= sL
        case _ =>
      }

      (ch7: @scala.annotation.switch) match {
        case '"' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a1(idx) |= sL
        case '\n' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a2(idx) |= sL
        case '\r' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a3(idx) |= sL
        case ',' =>
          val idx = (i + 7) >> BitSet.LogWL
          val sL = 1L << ((i + 7) & 63)
          a4(idx) |= sL
        case _ =>
      }

      i += 8
    }

    while (i < n) {
      val ch = b(off + i)

      ch match {
        case '"' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a1(idx) |= sL
        }
        case '\n' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a2(idx) |= sL
        }
        case '\r' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a3(idx) |= sL
        }
        case ',' => {
          val idx = i >> BitSet.LogWL
          val sL = 1L << (i & 63)
          a4(idx) |= sL
        }
        case _ =>
      }

      i += 1
    }

  }

  def elementWiseEqual4Variable(
      a: CharBuffer,
      t1: Char,
      o1: BitSet,
      t2: Char,
      o2: BitSet,
      t3: Char,
      o3: BitSet,
      t4: Char,
      o4: BitSet
  ): Unit = {
    var i = 0
    val n = a.remaining()
    val b = a.array()
    val off = a.arrayOffset()

    val a1 = o1.elems
    val a2 = o2.elems
    val a3 = o3.elems
    val a4 = o4.elems

    // unroll
    while (i + 7 < n) {
      val ch0 = b(off + i)
      val ch1 = b(off + i + 1)
      val ch2 = b(off + i + 2)
      val ch3 = b(off + i + 3)
      val ch4 = b(off + i + 4)
      val ch5 = b(off + i + 5)
      val ch6 = b(off + i + 6)
      val ch7 = b(off + i + 7)

      if (ch0 == t1) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a1(idx) |= sL
      }
      if (ch0 == t2) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a2(idx) |= sL
      }
      if (ch0 == t3) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a3(idx) |= sL
      }
      if (ch0 == t4) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a4(idx) |= sL
      }

      if (ch1 == t1) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a1(idx) |= sL
      }
      if (ch1 == t2) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a2(idx) |= sL
      }
      if (ch1 == t3) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a3(idx) |= sL
      }
      if (ch1 == t4) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a4(idx) |= sL
      }

      if (ch2 == t1) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a1(idx) |= sL
      }
      if (ch2 == t2) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a2(idx) |= sL
      }
      if (ch2 == t3) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a3(idx) |= sL
      }
      if (ch2 == t4) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a4(idx) |= sL
      }

      if (ch3 == t1) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a1(idx) |= sL
      }
      if (ch3 == t2) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a2(idx) |= sL
      }
      if (ch3 == t3) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a3(idx) |= sL
      }
      if (ch3 == t4) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a4(idx) |= sL
      }

      if (ch4 == t1) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a1(idx) |= sL
      }
      if (ch4 == t2) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a2(idx) |= sL
      }
      if (ch4 == t3) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a3(idx) |= sL
      }
      if (ch4 == t4) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a4(idx) |= sL
      }

      if (ch5 == t1) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a1(idx) |= sL
      }
      if (ch5 == t2) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a2(idx) |= sL
      }
      if (ch5 == t3) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a3(idx) |= sL
      }
      if (ch5 == t4) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a4(idx) |= sL
      }

      if (ch6 == t1) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a1(idx) |= sL
      }
      if (ch6 == t2) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a2(idx) |= sL
      }
      if (ch6 == t3) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a3(idx) |= sL
      }
      if (ch6 == t4) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a4(idx) |= sL
      }

      if (ch7 == t1) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a1(idx) |= sL
      }
      if (ch7 == t2) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a2(idx) |= sL
      }
      if (ch7 == t3) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a3(idx) |= sL
      }
      if (ch7 == t4) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a4(idx) |= sL
      }

      i += 8
    }

    while (i < n) {
      val ch = b(off + i)
      if (ch == t1) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a1(idx) |= sL
      } else if (ch == t2) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a2(idx) |= sL
      } else if (ch == t3) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a3(idx) |= sL
      } else if (ch == t4) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a4(idx) |= sL
      }

      i += 1
    }

  }
  def elementWiseEqual3Variable(
      a: CharBuffer,
      t1: Char,
      o1: BitSet,
      t2: Char,
      o2: BitSet,
      t3: Char,
      o3: BitSet
  ): Unit = {
    var i = 0
    val n = a.remaining()
    val b = a.array()
    val off = a.arrayOffset()

    val a1 = o1.elems
    val a2 = o2.elems
    val a3 = o3.elems

    while (i + 7 < n) {
      val ch0 = b(off + i)
      val ch1 = b(off + i + 1)
      val ch2 = b(off + i + 2)
      val ch3 = b(off + i + 3)
      val ch4 = b(off + i + 4)
      val ch5 = b(off + i + 5)
      val ch6 = b(off + i + 6)
      val ch7 = b(off + i + 7)

      if (ch0 == t1) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a1(idx) |= sL
      }
      if (ch0 == t2) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a2(idx) |= sL
      }
      if (ch0 == t3) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a3(idx) |= sL
      }

      if (ch1 == t1) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a1(idx) |= sL
      }
      if (ch1 == t2) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a2(idx) |= sL
      }
      if (ch1 == t3) {
        val idx = (i + 1) >> BitSet.LogWL
        val sL = 1L << ((i + 1) & 63)
        a3(idx) |= sL
      }

      if (ch2 == t1) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a1(idx) |= sL
      }
      if (ch2 == t2) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a2(idx) |= sL
      }
      if (ch2 == t3) {
        val idx = (i + 2) >> BitSet.LogWL
        val sL = 1L << ((i + 2) & 63)
        a3(idx) |= sL
      }

      if (ch3 == t1) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a1(idx) |= sL
      }
      if (ch3 == t2) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a2(idx) |= sL
      }
      if (ch3 == t3) {
        val idx = (i + 3) >> BitSet.LogWL
        val sL = 1L << ((i + 3) & 63)
        a3(idx) |= sL
      }

      if (ch4 == t1) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a1(idx) |= sL
      }
      if (ch4 == t2) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a2(idx) |= sL
      }
      if (ch4 == t3) {
        val idx = (i + 4) >> BitSet.LogWL
        val sL = 1L << ((i + 4) & 63)
        a3(idx) |= sL
      }

      if (ch5 == t1) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a1(idx) |= sL
      }
      if (ch5 == t2) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a2(idx) |= sL
      }
      if (ch5 == t3) {
        val idx = (i + 5) >> BitSet.LogWL
        val sL = 1L << ((i + 5) & 63)
        a3(idx) |= sL
      }

      if (ch6 == t1) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a1(idx) |= sL
      }
      if (ch6 == t2) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a2(idx) |= sL
      }
      if (ch6 == t3) {
        val idx = (i + 6) >> BitSet.LogWL
        val sL = 1L << ((i + 6) & 63)
        a3(idx) |= sL
      }

      if (ch7 == t1) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a1(idx) |= sL
      }
      if (ch7 == t2) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a2(idx) |= sL
      }
      if (ch7 == t3) {
        val idx = (i + 7) >> BitSet.LogWL
        val sL = 1L << ((i + 7) & 63)
        a3(idx) |= sL
      }

      i += 8
    }

    while (i < n) {
      val ch = b(off + i)
      if (ch == t1) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a1(idx) |= sL
      } else if (ch == t2) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a2(idx) |= sL
      } else if (ch == t3) {
        val idx = i >> BitSet.LogWL
        val sL = 1L << (i & 63)
        a3(idx) |= sL
      }

      i += 1
    }

  }

  def makeMaskFromCrLfInPlace(
      crMask: BitSet,
      lfMask: BitSet
  ): BitSet = {
    crMask.shiftForwardInPlace(0L)
    crMask &= lfMask
    crMask
  }

  def prefixSumXor(quoteMask: BitSet): BitSet = {
    quoteMask.prefixSumXor()
  }

  @inline private def contains(elems: Array[Long], elem: Int) = {
    val idx = elem >> 6
    val n = elems.length

    0 <= elem && n != 0 && n > idx && (elems(idx) & (1L << elem)) != 0L
  }

  def toIndices1(
      chars: CharBuffer,
      structuralMask: BitSet,
      recordSeparatorMask: BitSet,
      quoteMask: BitSet,
      from: Array[Int],
      to: Array[Int]
  ) = {
    var i = 0
    from(i) = 0
    var next = -1
    var m = next
    val recordSeparatorMaskElems = recordSeparatorMask.elems
    val quoteElems = quoteMask.elems
    while (next != -2) {
      m = next + 1
      next = structuralMask.min(m)
      val p = if (contains(recordSeparatorMaskElems, next)) -1 else 1
      var offset = 0

      if (contains(quoteElems, from(i))) {
        from(i) += 1
        offset -= 1
      }
      to(i) = (next + offset) * p

      i += 1
      from(i) = next + 1
    }
    to(i - 1) = chars.remaining()
    if (contains(quoteElems, to(i - 1) - 1)) {
      to(i - 1) -= 1
    }
    chars.position(m)

    i - 1
  }
  def toIndices2(
      chars: CharBuffer,
      structuralMask: BitSet,
      recordSeparatorMask: BitSet,
      quoteMask: BitSet,
      from: Array[Int],
      to: Array[Int]
  ) = {
    var i = 0
    from(i) = 0
    var next = -1
    var m = next
    val recordSeparatorMaskElems = recordSeparatorMask.elems
    val quoteElems = quoteMask.elems
    while (next != -2) {
      m = next + 1
      next = structuralMask.min(m)
      val p = if (contains(recordSeparatorMaskElems, next)) -1 else 1
      var offset = if (p == -1) -1 else 0
      if (contains(quoteElems, from(i))) {
        from(i) += 1
        offset -= 1
      }
      to(i) = (next + offset) * p

      i += 1
      from(i) = next + 1
    }
    to(i - 1) = chars.remaining()
    if (contains(quoteElems, to(i - 1) - 1)) {
      to(i - 1) -= 1
    }
    chars.position(m)

    i - 1
  }
}

private[csv2] sealed trait DataBuffer {
  def hasNext: Boolean

  def bufferTooShort: Boolean

  def nextBatch: (Array[Char], Array[Int], Array[Int], Int)
  def emitRest: (Array[Char], Array[Int], Array[Int], Int)
}

private[csv2] final class DataBuffer1(
    data: Iterator[CharBuffer],
    quoteChar: Char,
    fieldSeparator: Char,
    recordSeparator: Char,
    bufferSize: Int
) extends DataBuffer {
  import DataBuffer._
  private var outputChars: Array[Char] = null
  private var outputLength = -1
  private val outputFrom = Array.ofDim[Int](bufferSize + 2)
  private val outputTo = Array.ofDim[Int](bufferSize + 2)
  private val quoteMask = BitSet.allocate(bufferSize)
  private val lfMask = BitSet.allocate(bufferSize)
  private val fieldSeparatorMask = BitSet.allocate(bufferSize)
  var bufferTooShort = false
  var lastPos = -1

  var filledNewData = false
  var lineClosed = false

  val commonCSV =
    quoteChar == '"' && fieldSeparator == ',' && recordSeparator == '\n'
  val commonTSV =
    quoteChar == '"' && fieldSeparator == '\t' && recordSeparator == '\n'

  final def hasNext = fillBuffer()

  final def nextBatch = {
    filledNewData = false
    (outputChars, outputFrom, outputTo, outputLength)
  }

  final def emitRest =
    if (filledNewData || !lineClosed) {
      outputFrom(0) = outputFrom(outputLength)
      outputTo(0) = outputTo(outputLength)
      (outputChars, outputFrom, outputTo, 1)
    } else {
      (outputChars, outputFrom, outputTo, 0)
    }

  private def fillBuffer(): Boolean = {
    if (!data.hasNext) false
    else {
      filledNewData = true
      val next = data.next()

      quoteMask.zero()
      lfMask.zero()
      fieldSeparatorMask.zero()

      if (commonCSV)
        elementWiseEqual3CSV_LF(
          a = next,
          o1 = quoteMask,
          o2 = lfMask,
          o3 = fieldSeparatorMask
        )
      else if (commonTSV)
        elementWiseEqual3TSV(
          a = next,
          o1 = quoteMask,
          o2 = lfMask,
          o3 = fieldSeparatorMask
        )
      else
        elementWiseEqual3Variable(
          a = next,
          t1 = quoteChar,
          o1 = quoteMask,
          t2 = recordSeparator,
          o2 = lfMask,
          t3 = fieldSeparator,
          o3 = fieldSeparatorMask
        )

      val nInQuote: BitSet = prefixSumXor(quoteMask)
      nInQuote.negateInplace()

      val (structuralMask: BitSet, recordSeparatorMask: BitSet) = {

        lfMask &= nInQuote
        fieldSeparatorMask &= nInQuote

        val structuralMask = lfMask | fieldSeparatorMask

        (structuralMask, lfMask)
      }
      outputChars = next.array()
      assert(next.arrayOffset() == 0)
      outputLength = toIndices1(
        chars = next,
        structuralMask = structuralMask,
        recordSeparatorMask = recordSeparatorMask,
        quoteMask = quoteMask,
        from = outputFrom,
        to = outputTo
      )

      if (recordSeparatorMask.contains(next.limit() - 1)) {
        lineClosed = true
      } else {
        lineClosed = false
      }

      if (next.position() == 0 && lastPos == 0) {
        bufferTooShort = true
        false
      } else {
        lastPos = next.position()
        true
      }

    }
  }

}
private[csv2] final class DataBuffer2(
    data: Iterator[CharBuffer],
    quoteChar: Char,
    fieldSeparator: Char,
    recordSeparator1: Char,
    recordSeparator2: Char,
    bufferSize: Int
) extends DataBuffer {
  import DataBuffer._

  private var outputChars: Array[Char] = null
  private var outputLength = -1
  private val outputFrom = Array.ofDim[Int](bufferSize + 2)
  private val outputTo = Array.ofDim[Int](bufferSize + 2)
  private val quoteMask = BitSet.allocate(bufferSize)
  private val crMask = BitSet.allocate(bufferSize)
  private val lfMask = BitSet.allocate(bufferSize)
  private val fieldSeparatorMask = BitSet.allocate(bufferSize)

  val rfcCompatible =
    quoteChar == '"' && fieldSeparator == ',' && recordSeparator1 == '\r' && recordSeparator2 == '\n'

  var bufferTooShort = false
  var lastPos = -1
  var filledNewData = false
  var lineClosed = false

  final def hasNext = fillBuffer()

  final def nextBatch = {
    filledNewData = false
    (outputChars, outputFrom, outputTo, outputLength)
  }

  final def emitRest =
    if (filledNewData || !lineClosed) {
      outputFrom(0) = outputFrom(outputLength)
      outputTo(0) = outputTo(outputLength)
      (outputChars, outputFrom, outputTo, 1)
    } else {
      (outputChars, outputFrom, outputTo, 0)
    }

  private def fillBuffer(): Boolean = {
    if (!data.hasNext) false
    else {
      filledNewData = true
      val next = data.next()

      quoteMask.zero()
      crMask.zero()
      lfMask.zero()
      fieldSeparatorMask.zero()

      if (rfcCompatible)
        elementWiseEqual4RFC(
          a = next,
          o1 = quoteMask,
          o2 = lfMask,
          o3 = crMask,
          o4 = fieldSeparatorMask
        )
      else
        elementWiseEqual4Variable(
          a = next,
          t1 = quoteChar,
          o1 = quoteMask,
          t2 = recordSeparator2,
          o2 = lfMask,
          t3 = recordSeparator1,
          o3 = crMask,
          t4 = fieldSeparator,
          o4 = fieldSeparatorMask
        )

      val nInQuote: BitSet = prefixSumXor(quoteMask)
      nInQuote.negateInplace()

      val (structuralMask: BitSet, recordSeparatorMask: BitSet) = {

        crMask &= nInQuote
        lfMask &= nInQuote
        fieldSeparatorMask &= nInQuote

        // overwrites crMask
        makeMaskFromCrLfInPlace(crMask, lfMask)
        val structuralMask = crMask | fieldSeparatorMask

        (structuralMask, crMask)

      }
      outputChars = next.array()
      assert(next.arrayOffset() == 0)
      outputLength = toIndices2(
        chars = next,
        structuralMask = structuralMask,
        recordSeparatorMask = recordSeparatorMask,
        quoteMask = quoteMask,
        from = outputFrom,
        to = outputTo
      )

      if (recordSeparatorMask.contains(next.limit() - 1)) {
        lineClosed = true
      } else {
        lineClosed = false
      }

      if (next.position() == 0 && lastPos == 0) {
        bufferTooShort = true
        false
      } else {
        lastPos = next.position()
        true
      }

    }
  }

}
