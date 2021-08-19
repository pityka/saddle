/** The code in this file is adapted from spire
  * (https://github.com/typelevel/spire) Spire is Copyright (c) 2011-2012 Erik
  * Osheim, Tom Switzer and is released under MIT license.
  *
  * Modifications:
  *   - add index sorters (arg sort, permutation index)
  *   - add ORD shorthand
  */
package org.saddle.array

import org.saddle.ORD
import scala.{specialized => sp}
import scala.reflect.ClassTag

/** An implementation of insertion sort.
  *
  * Works well for small arrays but due to quadratic complexity is not generally
  * optimal.
  */
object InsertionSort {

  /** Sorts `data` in place using insertion sort.
    *
    * @param data
    *   the array to be sorted
    * @tparam A
    *   a member of the type class `Order`
    */
  final def sort[@sp A: ORD](data: Array[A]): Unit =
    sort(data, 0, data.length)

  /** Uses insertion sort on `data` to sort the entries from the index `start`
    * up to, but not including, the index `end`. Operates in place.
    *
    * @param data
    *   the array to be sorted
    * @param start
    *   the index of the first element, inclusive, to be sorted
    * @param end
    *   the index of the last element, exclusive, to be sorted
    * @tparam A
    *   a member of the type class `Order`
    */
  final def sort[@sp A](data: Array[A], start: Int, end: Int)(implicit
      o: ORD[A]
  ): Unit = {
    require(start <= end && start >= 0 && end <= data.length)
    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/** In-place merge sort implementation. This sort is stable but does mutate the
  * given array. It is an in-place sort but it does allocate a temporary array
  * of the same size as the input. It uses InsertionSort for sorting very small
  * arrays.
  */
object MergeSort {
  @inline final def startWidth: Int = 8
  @inline final def startStep: Int = 16

  /** Uses merge sort to sort the array `data` in place.
    *
    * If the size of the input array does not exceed the threshold `startStep`,
    * uses insertion sort instead.
    *
    * @param data
    *   the array to be sorted
    * @tparam A
    *   a member of the type class `Order`
    */
  final def sort[@sp A: ORD: ClassTag](data: Array[A]): Unit = {
    val len = data.length

    if (len <= startStep) {
      InsertionSort.sort(data)
      return
    }

    var buf1: Array[A] = data
    var buf2: Array[A] = new Array[A](len)
    var tmp: Array[A] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) {
      InsertionSort.sort(data, i, i + startWidth); i += startWidth
    }
    if (i < len) InsertionSort.sort(data, i, len)
    var width = startWidth
    var step = startStep
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        merge(buf1, buf2, i, i + width, i + step); i += step
      }
      while (i < len) {
        merge(buf1, buf2, i, scala.math.min(i + width, len), len); i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    if (!buf1.eq(data)) System.arraycopy(buf1, 0, data, 0, len)
  }

  /** Helper method for mergeSort, used to do a single "merge" between two
    * sections of the input array, and write the result to the output array.
    *
    * The first input section starts at `start` (inclusive) and ends at `mid`
    * (exclusive). The second input section starts at `mid` (inclusive) and ends
    * at `end` (exclusive).
    *
    * Writing to the output begins at `start` (inclusive).
    *
    * @param in
    *   the input array
    * @param out
    *   the output array
    * @param start
    *   the start of the first input section (inclusive) as well as the start of
    *   the merged output
    * @param mid
    *   the end of the first input section (exclusive) and the beginning of the
    *   second input section (inclusive)
    * @param end
    *   the end of the second input section (exclusive)
    * @tparam A
    *   a member of the type class `Order`
    */
  @inline final def merge[@sp A](
      in: Array[A],
      out: Array[A],
      start: Int,
      mid: Int,
      end: Int
  )(implicit o: ORD[A]): Unit = {
    require(
      start >= 0 && start <= mid && mid <= end && end <= in.length && end <= out.length
    )
    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteqv(in(ii), in(jj)))) {
        out(kk) = in(ii); ii += 1
      } else {
        out(kk) = in(jj); jj += 1
      }
      kk += 1
    }
  }
}

/** An implementation of insertion sort.
  *
  * Works well for small arrays but due to quadratic complexity is not generally
  * optimal.
  */
object PermuteInsertionSort {

  /** Uses insertion sort on `data` to sort the entries from the index `start`
    * up to, but not including, the index `end`. Permutation indices are
    * returned in `perm`: data[perm[k]] is the k-th smallest elem in data `data`
    * is not modified. Does not allocate, except on stack.
    *
    * @param data
    *   the array to be sorted
    * @param start
    *   the index of the first element, inclusive, to be sorted
    * @param end
    *   the index of the last element, exclusive, to be sorted
    * @param perm
    *   array of permutation indices
    * @tparam A
    *   a member of the type class `Order`
    */
  final def sort[@sp A](data: Array[A], start: Int, end: Int, perm: Array[Int])(
      implicit o: ORD[A]
  ): Unit = {
    require(
      start <= end && start >= 0 && end <= data.length && end <= perm.length
    )
    if (perm.isEmpty) ()
    else {
      perm(start) = start
      var i = start + 1
      while (i < end) {
        val item = data(i)
        var hole = i
        perm(hole) = hole
        while (hole > start && o.gt(data(perm(hole - 1)), item)) {
          val swap = perm(hole)
          perm(hole) = perm(hole - 1)
          perm(hole - 1) = swap
          hole -= 1
        }
        i += 1
      }
    }
  }
}

/** In-place merge sort implementation. This sort is stable but does mutate the
  * given array. It is an in-place sort but it does allocate a temporary array
  * of the same size as the input. It uses InsertionSort for sorting very small
  * arrays.
  */
object PermuteMergeSort {
  @inline final def startWidth: Int = 8
  @inline final def startStep: Int = 16

  final def sort[@sp A: ORD](
      data: Array[A],
      perm: Array[Int]
  ): Unit = {
    val len = data.length

    if (len <= startStep) {
      PermuteInsertionSort.sort(data, 0, len, perm)
      return
    }

    var buf1: Array[Int] = perm
    var buf2: Array[Int] = new Array[Int](len)
    var tmp: Array[Int] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) {
      PermuteInsertionSort.sort(data, i, i + startWidth, perm); i += startWidth
    }
    if (i < len) PermuteInsertionSort.sort(data, i, len, perm)
    var width = startWidth
    var step = startStep
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        mergePerm(data, buf1, buf2, i, i + width, i + step); i += step
      }
      while (i < len) {
        mergePerm(data, buf1, buf2, i, math.min(i + width, len), len); i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    if (!buf1.eq(perm)) System.arraycopy(buf1, 0, perm, 0, len)
  }

  @inline final def mergePerm[@sp A](
      data: Array[A],
      in: Array[Int],
      out: Array[Int],
      start: Int,
      mid: Int,
      end: Int
  )(implicit o: ORD[A]): Unit = {
    require(
      start >= 0 && start <= mid && mid <= end && end <= in.length && end <= out.length
    )
    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteqv(data(in(ii)), data(in(jj))))) {
        out(kk) = in(ii); ii += 1
      } else {
        out(kk) = in(jj); jj += 1
      }
      kk += 1
    }
  }
}
