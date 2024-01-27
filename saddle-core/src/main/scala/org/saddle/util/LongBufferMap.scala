package org.saddle.util

import org.saddle.Buffer

/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 *
 * Modified to specialize manually on Int as value type
 * Modified to remove unneeded features
 */

/** This class implements mutable maps with Long keys based on a hash table with
  * open addressing.
  *
  * Basic map operations on single entries, including `contains` and `get`, are
  * typically substantially faster with `LongMap` than HashMap. Methods that act
  * on the whole map, including `foreach` and `map` are not in general expected
  * to be faster than with a generic map, save for those that take particular
  * advantage of the internal structure of the map: `foreachKey`,
  * `foreachValue`, `mapValuesNow`, and `transformValues`.
  *
  * Maps with open addressing may become less efficient at lookup after repeated
  * addition/removal of elements. Although `LongMap` makes a decent attempt to
  * remain efficient regardless, calling `repack` on a map that will no longer
  * have elements removed but will be used heavily may save both time and
  * storage space.
  *
  * This map is not intended to contain more than 2^29 entries (approximately
  * 500 million). The maximum capacity is 2^30, but performance will degrade
  * rapidly as 2^30 is approached.
  */
private[saddle] final class LongBufferMap(
    initialBufferSize: Int,
    initBlank: Boolean
) {
  import LongBufferMap._

  def this() = this(16, true)

  /** Creates a new `LongMap` with an initial buffer of specified size.
    *
    * A LongMap can typically contain half as many elements as its buffer size
    * before it requires resizing.
    */
  def this(initialBufferSize: Int) =
    this(initialBufferSize, true)

  private[this] var mask = 0
  private[this] var extraKeys: Int = 0
  private[this] var zeroValue: Buffer[Int] = Buffer.empty[Int]
  private[this] var minValue: Buffer[Int] = Buffer.empty[Int]
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _keys: Array[Long] = null
  private[this] var _values: Array[Buffer[Int]] = null

  if (initBlank) defaultInitialize(initialBufferSize)

  private[this] def defaultInitialize(n: Int) = {
    mask =
      if (n < 0) 0x7
      else
        (((1 << (32 - java.lang.Integer
          .numberOfLeadingZeros(n - 1))) - 1) & 0x3fffffff) | 0x7
    _keys = new Array[Long](mask + 1)
    _values = new Array[Buffer[Int]](mask + 1)
  }

  def initializeTo(
      m: Int,
      ek: Int,
      zv: Buffer[Int],
      mv: Buffer[Int],
      sz: Int,
      vc: Int,
      kz: Array[Long],
      vz: Array[Buffer[Int]]
  ): Unit = {
    mask = m; extraKeys = ek; zeroValue = zv; minValue = mv; _size = sz;
    _vacant = vc; _keys = kz; _values = vz
  }

  def size: Int = _size + (extraKeys + 1) / 2

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5 * mask || _vacant > _size

  private def toIndex(k: Long): Int = {
    // Part of the MurmurHash3 32 bit finalizer
    val h = ((k ^ (k >>> 32)) & 0xffffffffL).toInt
    val x = (h ^ (h >>> 16)) * 0x85ebca6b
    (x ^ (x >>> 13)) & mask
  }

  private def seekEmpty(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    while (_keys(e) != 0) { x += 1; e = (e + 2 * (x + 1) * x - 3) & mask }
    e
  }

  private def seekEntry(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q == k) return e; q != 0 }) {
      x += 1; e = (e + 2 * (x + 1) * x - 3) & mask
    }
    e | MissingBit
  }

  private def seekEntryOrOpen(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q == k) return e; q + q != 0 }) {
      x += 1
      e = (e + 2 * (x + 1) * x - 3) & mask
    }
    if (q == 0) return e | MissingBit
    val o = e | MissVacant
    while ({ q = _keys(e); if (q == k) return e; q != 0 }) {
      x += 1
      e = (e + 2 * (x + 1) * x - 3) & mask
    }
    o
  }

  def contains(key: Long): Boolean = {
    if (key == -key) (((key >>> 63).toInt + 1) & extraKeys) != 0
    else seekEntry(key) >= 0
  }

  /* must call contains before */
  def get(key: Long): Buffer[Int] = {
    if (key == -key) {
      if ((((key >>> 63).toInt + 1) & extraKeys) == 0) throw new NoSuchElementException(key.toString)
      else if (key == 0) zeroValue
      else minValue
    } else {
      val i = seekEntry(key)
      if (i < 0) throw new NoSuchElementException(key.toString) else _values(i)
    }
  }

  private def repack(newMask: Int): Unit = {
    val ok = _keys
    val ov = _values
    mask = newMask
    _keys = new Array[Long](mask + 1)
    _values = new Array[Buffer[Int]](mask + 1)
    _vacant = 0
    var i = 0
    while (i < ok.length) {
      val k = ok(i)
      if (k != -k) {
        val j = seekEmpty(k)
        _keys(j) = k
        _values(j) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `LongMap` for maximum efficiency of lookup.
    *
    * For maps that undergo a complex creation process with both addition and
    * removal of keys, and then are used heavily with no further removal of
    * elements, calling `repack` after the end of the creation can result in
    * improved performance. Repacking takes time proportional to the number of
    * entries in the map.
    */
  def repack(): Unit = {
    var m = mask
    if (_size + _vacant >= 0.5 * mask && !(_vacant > 0.2 * mask))
      m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8 * _size < m) m = m >>> 1
    repack(m)
  }

  /** Updates the map to include a new key-value pair.
    *
    * This is the fastest way to add an entry to a `LongMap`.
    */
  def update(key: Long, value: Int): Unit = {
    if (key == -key) {
      if (key == 0) {
        zeroValue.+=(value)
        extraKeys |= 1
      } else {
        minValue.+=(value)
        extraKeys |= 2
      }
    } else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        val _v = _values(j)
        if (_v == null) {
          val b = Buffer.empty[Int]
          b += (value)
          _values(j) = b
        } else {
          _v.+=(value)
        }
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
      } else {
        _keys(i) = key
        val _v = _values(i)
        if (_v == null) {
          val b = Buffer.empty[Int]
          b += (value)
          _values(i) = b
        } else {
          _v.+=(value)
        }
      }
    }
  }


}

private[saddle] object LongBufferMap {
  private final val IndexMask = 0x3fffffff
  private final val MissingBit = 0x80000000
  private final val VacantBit = 0x40000000
  private final val MissVacant = 0xc0000000

  /** Creates a new empty `LongMap`. */
  def empty = new LongBufferMap

}
