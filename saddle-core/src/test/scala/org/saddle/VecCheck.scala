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
package org.saddle

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import org.saddle.scalar.Value
import org.saddle.ops.BinOps._
import org.saddle.util.DoubleTotalOrder
import org.saddle.scalar.FromScalars
import org.saddle.scalar.FromScalarGen
import org.saddle.scalar.ScalarGen

/** Test on properties of Vec
  */
class VecCheck extends Specification with ScalaCheck {

  "heterogeneous scalar generators checks" in {
    implicit val fromScalarGenVec = new FromScalarGen[Vec] {
      override def apply[T](sg: ScalarGen[T]): Gen[Vec[T]] =
        VecArbitraries.vec(sg.gen)(sg.tag)
    }
    "takeLeft" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        v.takeLeft(5) must_== v.toSeq.take(5).toVec
      }
    }

    "takeRight" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        v.takeRight(5) must_== v.toSeq.takeRight(5).toVec
      }
    }

    "dropLeft" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        v.dropLeft(5) must_== v.toSeq.drop(5).toVec
      }
    }

    "dropRight" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        v.dropRight(5) must_== v.toSeq.dropRight(5).toVec
      }
    }

    "vectors equality" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        (v must_== Vec(v.contents)) and (v must_== v)
      }
    }

    "last works" in {
      forAll { (fs: FromScalars[Vec]) =>
        val v = fs.value
        if (v.isEmpty)
          v.last must_== scalar.NA
        else
          v.last must_== v.at(v.length - 1)
      }
    }

    "concat works" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        val data = v.contents
        v.concat(v) must_== Vec(data ++ data)
      }
    }

    "dropNA works" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        val data = v.contents
        v.dropNA must_== Vec(data.filter(st.notMissing))
      }
    }

    "hasNA works" in {
      forAll { (fs: FromScalars[Vec]) =>
        implicit val st = fs.tag
        val v = fs.value
        val data = v.contents
        v.hasNA must_== (data.indexWhere(st.isMissing) >= 0)
      }
    }

    // make more tests use the heterogenous generators
  }

  "further checks" in {
    implicit val vec = Arbitrary(VecArbitraries.vecDoubleWithNA)
    "assignment operator in view" in {
      val v = org.saddle.vec.ones(2)

      v.view(Array(0)) += 1d
      v must_== Vec(2d, 1d)
    }
    "apply+update in apply" in {
      val v = org.saddle.vec.ones(2)

      v(0) += 1d
      v must_== Vec(2d, 1d)
    }
    "update in slice" in {
      val v = org.saddle.vec.ones(10)
      val v2 = v.slice(2, 5)
      v2(1) = 0d
      v.toArray(3) must_== 0d
    }
    "apply+update in slice" in {
      val v = org.saddle.vec.ones(10)
      val v2 = v.slice(2, 5)
      v2(1) += -1d
      v.toArray(4) must_== 1d
      v.toArray(3) must_== 0d
      v.toArray(2) must_== 1d
    }
    "update slice" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> 3) = 0d
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 1d)
    }
    "update slice" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> *) = 0d
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 0d)
    }
    "update slice with Vec" in {
      val v = org.saddle.vec.ones(5)
      v(2 -> *) = Vec(0d, 0d, 0d)
      v.toSeq must_== Seq(1d, 1d, 0d, 0d, 0d)
    }
    "update slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      v2(1 -> 3) = Vec(-1, -1)
      v must_== Vec(0, 1, 2, 3, -1, 5, 6, -1, 8, 9)
    }
    "scalar operation in place works" in {
      forAll { (m: Vec[Double], b: Int) =>
        val m2 = m * b
        val m1 = m.copy
        m1 *= b
        m1 must_== m2
      }
    }
    "scalar operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      val v3 = v2.slice(1, 3)
      v3 *= 10
      v must_== Vec(0, 1, 2, 3, 40, 5, 6, 70, 8, 9)
    }
    "scalar operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      v2.take(1 -> 3) *= 10
      v must_== Vec(0, 1, 2, 3, 40, 5, 6, 70, 8, 9)
    }
    "vec operations on slice with Vec in slice" in {
      val v = Vec(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val v2 = v.slice(1, 8, 3)
      val v3 = v2.slice(1, 3)
      v3 *= Vec(-10, 10)
      v must_== Vec(0, 1, 2, 3, -40, 5, 6, 70, 8, 9)
    }

    "single element access of vector" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.choose(0, v.length - 1)
          val data = v.contents
          forAll(idx) { i =>
            (v.at(i).isNA must beTrue) or (v.at(i) must_== Value(data(i)))
            (v.raw(i).isNaN must beTrue) or (v.raw(i) must_== data(i))
          }
        }
      }
    }

    "multiple element access / slicing of vector" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 1) ==> {
          val idx = Gen.choose(0, v.length - 2)
          val data = v.contents
          forAll(idx) { i =>
            v.take(i, i + 1) must_== Vec(data(i), data(i + 1))
            v.take(i -> (i + 1)) must_== Vec(data(i), data(i + 1))
            v.take((i + 1) -> i) must_== Vec.empty[Double]
            v.take(i -> *) must_== Vec(Range(i, v.length).map(data(_)): _*)
            v.take(* -> i) must_== Vec(Range(0, i + 1).map(data(_)): _*)
            v.take(*) must_== v
          }
        }
      }
    }

    "first works" in {
      forAll { (v: Vec[Double]) =>
        if (v.isEmpty)
          v.first must_== scalar.NA
        else
          v.first must_== v.at(0)
      }
    }

    "last works" in {
      forAll { (v: Vec[Double]) =>
        if (v.isEmpty)
          v.last must_== scalar.NA
        else
          v.last must_== v.at(v.length - 1)
      }
    }

    "map works" in {
      forAll { (v: Vec[Double]) =>
        val data = v.contents
        v.map(_ + 1.0) must_== Vec(data.toSeq.map(_ + 1.0).toArray)
        v.map(_ => 5.0) must_== Vec(
          data.toSeq.map(d => if (d.isNaN) na[Double] else 5.0).toArray
        )
        v.map(_ => 5) must_== Vec[Int](
          data.toSeq.map(d => if (d.isNaN) na[Int] else 5).toArray
        )
      }
    }

    "zipmap works" in {
      forAll { (v: Vec[Double]) => v.zipMap(v)(_ + _) must_== v * 2.0 }
    }
    "zipmap idx works" in {
      forAll { (v: Vec[Double]) =>
        v.zipMapIdx((a, b) => (a + b)) must_== v.toSeq.zipWithIndex.map {
          case (a, b) => a + b
        }.toVec
      }
    }

    "findOne works" in {
      val v = Vec(1d, 2, 3, na[Double], 5)
      v.findOne(_ == 3d) must_== 2
      v.findOne(_ == 5d) must_== 4
      v.findOne(_ == 7d) must_== -1
    }

    "find works" in {
      val v = Vec(1d, 2, 3, na[Double], 3, 4)
      v.find(_ == 3d) must_== Vec(2, 4)
      v.find(_ == 4d) must_== Vec(5)
      v.find(_ == 7d) must_== Vec.empty[Int]
    }

    "exists works" in {
      val v = Vec(1d, 2, 3, na[Double], 3, 4)
      v.exists(_ == 3d) must beTrue
      v.exists(_ == 2d) must beTrue
      v.exists(_ == 9d) must beFalse
    }

    "filter works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.filter(_ < 0)
        res.contents.indexWhere(_ >= 0) must_== -1
      }
    }

    "filterAt works" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.choose(0, v.length)
          forAll(idx) { i =>
            val res = v.filterAt(_ != i)
            (res.length <= i) || (res.length must_== v.length - 1)
          }
        }
      }
    }

    "where works" in {
      forAll { (v: Vec[Double]) =>
        val whereVec = (v < 0)
        v.where(whereVec) must_== v.filter(_ < 0)
      }
    }
    "whereNot works" in {
      forAll { (v: Vec[Double]) =>
        val whereVec = (v < 0)
        v.whereNot(whereVec) must_== v.where(whereVec.map(v => !v))
      }
    }
    "partition works" in {
      forAll { (v: Vec[Double]) =>
        val whereVec = (v < 0)
        v.partition(whereVec)._1 must_== v.where(whereVec)
        v.partition(whereVec)._2 must_== v.whereNot(whereVec)
      }
    }

    "sorted works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.sorted
        val exp = Vec(v.contents.sorted(DoubleTotalOrder.toOrdering))
        val nas = v.length - v.count

        res.slice(0, res.length - nas) must_== exp.slice(0, res.length - nas)
      }
    }

    "forall works" in {
      forAll { (v: Vec[Double]) =>
        var c = 0
        v.forall(_ > 0.5) { i => if (!i.isNaN) c += 1 }
        val exp = v.filter(_ > 0.5).count
        c must_== exp
      }
    }

    "foreach works" in {
      forAll { (v: Vec[Double]) =>
        var c = 0
        v.foreach { i => if (!i.isNaN) c += 1 }
        val exp = v.count
        c must_== exp
      }
    }

    "reversed works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.reversed
        val exp = Vec(v.contents.reverse)
        res must_== exp
      }
    }

    "fillNA works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.fillNA(_ => 5.0)
        val exp =
          Vec(v.contents.toSeq.map(x => if (x.isNaN) 5.0 else x).toArray)
        res.hasNA must beFalse
        res must_== exp
      }
    }

    "fillForward fills values forward" in {
      val v = Vec[Int](1, na[Int], na[Int], 2, na[Int], na[Int])
      v.fillForward() mustEqual Vec[Int](1, 1, 1, 2, 2, 2)
    }

    "fillForward fills last value forward" in {
      val v = Vec[Int](1, 2, na[Int], na[Int])
      v.fillForward() mustEqual Vec[Int](1, 2, 2, 2)
    }

    "fillForward fills values forward until the limit if greater than 0" in {
      val v = Vec[Int](1, na[Int], na[Int], 2, na[Int], na[Int], na[Int])
      v.fillForward(0) mustEqual Vec[Int](1, 1, 1, 2, 2, 2, 2)
      v.fillForward(1) mustEqual Vec[Int](1, 1, na[Int], 2, 2, na[Int], na[Int])
      v.fillForward(2) mustEqual Vec[Int](1, 1, 1, 2, 2, 2, na[Int])
    }

    "fillForward fills values forward only" in {
      val v = Vec[Int](na[Int], na[Int], 2, na[Int], na[Int])
      v.fillForward() mustEqual Vec[Int](na[Int], na[Int], 2, 2, 2)
    }

    "fillBackward fills values backward" in {
      val v = Vec[Int](na[Int], na[Int], 1, na[Int], na[Int], 2)
      v.fillBackward() mustEqual Vec[Int](1, 1, 1, 2, 2, 2)
    }

    "fillBackward fills last value backward" in {
      val v = Vec[Int](na[Int], na[Int], 2, 1)
      v.fillBackward() mustEqual Vec[Int](2, 2, 2, 1)
    }

    "fillBackword fills values backward until the limit if greater than 0" in {
      val v = Vec[Int](na[Int], na[Int], na[Int], 2, na[Int], na[Int], 1)
      v.fillBackward(0) mustEqual Vec[Int](2, 2, 2, 2, 1, 1, 1)
      v.fillBackward(1) mustEqual Vec[Int](
        na[Int],
        na[Int],
        2,
        2,
        na[Int],
        1,
        1
      )
      v.fillBackward(2) mustEqual Vec[Int](na[Int], 2, 2, 2, 1, 1, 1)
    }

    "fillBackward fills values backward only" in {
      val v = Vec[Int](na[Int], na[Int], 2, na[Int], na[Int])
      v.fillBackward() mustEqual Vec[Int](2, 2, 2, na[Int], na[Int])
    }

    "sliceAt works" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.choose(0, v.length)
          forAll(idx) { i =>
            val slc = v.slice(1, i)
            val exp = v.contents.slice(1, i)
            slc must_== Vec(exp)
          }
        }
      }
    }

    "foldLeft works" in {
      forAll { (v: Vec[Double]) =>
        val res =
          v.foldLeft(0)((c: Int, x: Double) => c + { if (x.isNaN) 0 else 1 })
        val exp = v.count
        res must_== exp
      }
    }

    "filterFoldLeft works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.filterFoldLeft(_ < 0)(0)((c: Int, _: Double) => c + 1)
        val exp = v.filter(_ < 0).count
        res must_== exp
      }
    }

    "foldLeftWhile works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.foldLeftWhile(0)((c: Int, _: Double) => c + 1)(
          (c: Int, _: Double) => c < 3
        )
        var c = 0
        val exp = v.contents.takeWhile { (v: Double) =>
          v.isNaN || { c += 1; c <= 3 }
        }
        res must_== Vec(exp).count
      }
    }

    "scanLeft works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.scanLeft(0)((c: Int, _: Double) => c + 1)
        res.length must_== v.length
        (res.last.isNA must beTrue) or (res.last must_== Value(v.count))
      }
    }

    "filterScanLeft works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.filterScanLeft(_ > 0.5)(0)((c: Int, _: Double) => c + 1)
        res.length must_== v.length
        (res.last.isNA must beTrue) or (res.last must_== Value(
          v.filter(_ > 0.5).count
        ))
      }
    }

    "concat works" in {
      forAll { (v1: Vec[Double], v2: Vec[Double]) =>
        val res = v1 concat v2
        val exp = Vec(v1.toArray ++ v2.toArray)
        res must_== exp
      }
    }

    "negation works" in {
      forAll { (v: Vec[Double]) =>
        val res = -v
        val exp = Vec(v.toArray.toSeq.map(_ * -1).toArray)
        res must_== exp
      }
    }

    "take works" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose(0, v.length - 1))
          forAll(idx) { i =>
            val res = v.take(i.toArray)
            val exp = Vec(i.toArray.toSeq.map(v.raw(_)).toArray)
            res must_== exp
            res must_== v.take(i: _*)
          }
        }
      }
    }

    "mask works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.mask(_ > 0.5).count
        val exp = v.countif(_ <= 0.5)
        res must_== exp

        val res2 = v.mask(v.map(_ > 0.5)).count
        res2 must_== exp
      }
    }

    "splitAt works" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.choose(0, v.length - 1)
          forAll(idx) { i =>
            val (res1, res2) = v.splitAt(i)
            res1.length must_== i
            res2.length must_== (v.length - i)
            (res1 concat res2) must_== v
          }
        }
      }
    }

    "shift works" in {
      forAll { (v: Vec[Double]) =>
        v.shift(0) must_== v

        (v.length > 0) ==> {
          val idx = Gen.choose(0, v.length - 1)
          forAll(idx) { i =>
            val res = v.shift(i)
            res.length must_== v.length
            res.slice(i, res.length) must_== v.slice(0, v.length - i)
          }
        }
      }
    }

    "flatMap works" in {
      forAll { (v: Vec[Double]) =>
        v.flatMap(v => Vec(v, v)) must_== v.toSeq
          .filterNot(_.isNaN)
          .flatMap(v => List(v, v))
          .toVec
      }
    }

    "without works" in {
      forAll { (v: Vec[Double]) =>
        (v.length > 0) ==> {
          val idx = Gen.listOfN(3, Gen.choose(0, v.length - 1))
          forAll(idx) { i =>
            val res = v.without(i.toArray)
            val tmp = scala.collection.mutable.ArrayBuffer[Double]()
            for (k <- 0 until v.length if !i.toSet.contains(k))
              tmp.append(v.raw(k))
            res must_== Vec(tmp.toArray)
          }
        }
      }
    }

    "rolling works" in {
      forAll { (v: Vec[Double]) =>
        val res = v.rolling(2, _.sum)

        if (v.length == 0)
          res must_== Vec.empty[Double]
        else if (v.length == 1) {
          res.raw(0) must_== v.sum
        } else {
          val dat = v.contents
          val exp = for {
            i <- 0 until v.length - 1
            a = dat(i)
            b = dat(i + 1)
          } yield (if (a.isNaN) 0 else a) + (if (b.isNaN) 0 else b)

          res must_== Vec(exp: _*)
        }
      }
    }

    "prod works" in {
      forAll { (v: Vec[Double]) =>
        v.prod must_== v.toSeq.filterNot(_.isNaN).foldLeft(1d)(_ * _)
      }
    }

    "sum works" in {
      forAll { (v: Vec[Double]) =>
        v.sum must_== v.toSeq.filterNot(_.isNaN).foldLeft(0d)(_ + _)
      }
    }

    "argmin works" in {
      forAll { (v: Vec[Double]) =>
        v.argmin must_== {
          if (v.dropNA.toSeq.isEmpty) -1
          else (v.toSeq.zipWithIndex.filterNot(_._1.isNaN).minBy(_._1)._2)
        }
      }
    }
    "argmax works" in {
      forAll { (v: Vec[Double]) =>
        v.argmax must_== {
          if (v.dropNA.toSeq.isEmpty) -1
          else (v.toSeq.zipWithIndex.filterNot(_._1.isNaN).maxBy(_._1)._2)
        }
      }
    }
  }
}
