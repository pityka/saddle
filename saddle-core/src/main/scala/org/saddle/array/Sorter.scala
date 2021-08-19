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
package org.saddle.array

import org.saddle.vec.VecBool
import org.saddle.ORD

/** Typeclass interface for sorting implementations
  */
trait Sorter[T] {
  def argSorted(arr: Array[T])(implicit ord: ORD[T]): Array[Int]
  def sorted(arr: Array[T])(implicit ord: ORD[T]): Array[T]
  // def sortBy[Q](arr: Array[T])(f: T => Q)(implicit ord: ORD[Q]): Array[T]
}

object Sorter {
  object boolSorter extends Sorter[Boolean] {
    def argSorted(arr: Array[Boolean])(implicit ord: ORD[Boolean]) =
      VecBool.argSort(arr)
    def sorted(arr: Array[Boolean])(implicit ord: ORD[Boolean]) =
      VecBool.sort(arr)

  }

  object byteSorter extends Sorter[Byte] {
    def argSorted(arr: Array[Byte])(implicit ord: ORD[Byte]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Byte])(implicit ord: ORD[Byte]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object charSorter extends Sorter[Char] {
    def argSorted(arr: Array[Char])(implicit ord: ORD[Char]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Char])(implicit ord: ORD[Char]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object shortSorter extends Sorter[Short] {
    def argSorted(arr: Array[Short])(implicit ord: ORD[Short]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Short])(implicit ord: ORD[Short]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object intSorter extends Sorter[Int] {
    def argSorted(arr: Array[Int])(implicit ord: ORD[Int]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Int])(implicit ord: ORD[Int]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object floatSorter extends Sorter[Float] {
    def argSorted(arr: Array[Float])(implicit ord: ORD[Float]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Float])(implicit ord: ORD[Float]) = {
      val res = arr.clone
      MergeSort.sort(res)
      res
    }
  }

  object longSorter extends Sorter[Long] {
    def argSorted(arr: Array[Long])(implicit ord: ORD[Long]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Long])(implicit ord: ORD[Long]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  object doubleSorter extends Sorter[Double] {
    def argSorted(arr: Array[Double])(implicit ord: ORD[Double]) = {
      val res = range(0, arr.length)
      PermuteMergeSort.sort(arr, res)
      res
    }

    def sorted(arr: Array[Double])(implicit ord: ORD[Double]) = {
      val res = arr.clone()
      MergeSort.sort(res)
      res
    }
  }

  def anySorter[T] =
    new Sorter[T] {
      def argSorted(arr: Array[T])(implicit ord: ORD[T]) = {
        val res = range(0, arr.length)
        PermuteMergeSort.sort(arr, res)
        res
      }

      def sorted(arr: Array[T])(implicit ord: ORD[T]) = {
        val res = arr.clone
        val offsets = argSorted(arr)
        var i = 0
        while (i < offsets.length) {
          val idx = offsets(i)
          res(i) = arr(idx)
          i += 1
        }
        res
      }
    }
}
