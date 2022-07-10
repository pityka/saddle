package org.saddle

import org.openjdk.jmh.annotations._

/** Benchmark suite comparing different loop patterns and the impact of loop optimizations.
  * https://wiki.openjdk.java.net/pages/viewpage.action?pageId=20415918
  */
@State(Scope.Benchmark)
@Warmup(iterations = 10) 
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class LoopOptimisations {
  @Param(Array("10000"))
  var size: Int = _
  var arr: Array[Double] = _
  var vec1: Vec[Double] = _
  var b: Double = _

  @Setup(Level.Iteration)
  def setup() = {
    arr = vec.rand(size).toArray
    vec1 = Vec(arr)
    b = scala.util.Random.nextDouble()
  }

  @Benchmark
  /** Simplest variant, should trigger vectorization and other optimisations.
    */
  def arrayAdd(): Array[Double] = {
    var i = 0
    while (i < arr.length) {
      arr(i) += b
      i += 1
    }
    arr
  }

  @Benchmark
  def arrayAddExpandedAssignmentOperator(): Array[Double] = {
    var i = 0
    while (i < arr.length) {
      // assignement operator seems to perform worse
      arr(i) = arr(i) + b
      i += 1
    }
    arr
  }


  /** hand-unrolled loops prevent vectorization and potentially other
    * optimisations
    */
  @Benchmark
  def arrayAddHandUnrolled2(): Array[Double] = {
    var i = 0
    val length = arr.length
    val unrolledStride = 2
    val preloopIterations = length % unrolledStride
    while (i < preloopIterations) {
      arr(i) += b
      i += 1
    }
    while (i < length) {
      arr(i + 0) += b
      arr(i + 1) += b
      i += unrolledStride
    }
    arr
  }

  @Benchmark
  def arrayAddHandUnrolled5(): Array[Double] = {
    var i = 0
    val length = arr.length
    val unrolledStride = 5
    val preloopIterations = length % unrolledStride
    while (i < preloopIterations) {
      arr(i) += b
      i += 1
    }
    while (i < length) {
      arr(i + 0) += b
      arr(i + 1) += b
      arr(i + 2) += b
      arr(i + 3) += b
      arr(i + 4) += b
      i += unrolledStride
    }
    arr
  }

  @Benchmark
  def arrayDiv(): Array[Double] = {
    var i = 0
    while (i < arr.length) {
      arr(i) /= b
      i += 1
    }
    arr
  }

  @Benchmark
  def arrayDivHandUnrolled2(): Array[Double] = {
    var i = 0
    val length = arr.length
    val unrolledStride = 2
    val preloopIterations = length % unrolledStride
    while (i < preloopIterations) {
      arr(i) /= b
      i += 1
    }
    while (i < length) {
      arr(i + 0) /= b
      arr(i + 1) /= b
      i += unrolledStride
    }
    arr
  }

  @Benchmark
  def vecBinOps(): Vec[Double] = {
    import ops.BinOps._
    vec1 += b
    vec1
  }

  @Benchmark
  def vecBinOpsMacros(): Vec[Double] = {
    import macros.BinOps._
    vec1 += b
    vec1
  }
}