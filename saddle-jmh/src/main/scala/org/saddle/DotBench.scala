package org.saddle

import org.openjdk.jmh.annotations._
@State(Scope.Benchmark)
@Warmup(iterations = 4, time=3)
@Measurement(iterations = 6, time=5)
@Fork(1)
@Threads(1)
class DotBench {
  @Param(Array("500", "1000", "10000"))
  var size: Int = _

  var v1: Vec[Double] = _
  var v2: Vec[Double] = _

  def vv_java_unrolled(self: Vec[Double], other: Vec[Double]): Double = {
    var i = 0
    var s = 0d
    val M = self.length

    val m = M % 5
    while (i < m) {
      s += self.raw(i) * other.raw(i)
      i += 1
    }

    while (i < M) {
      val v1 = self.raw(i) * other.raw(i)
      val v2 = self.raw(i + 1) * other.raw(i + 1)
      val v3 = self.raw(i + 2) * other.raw(i + 2)
      val v4 = self.raw(i + 3) * other.raw(i + 3)
      val v5 = self.raw(i + 4) * other.raw(i + 4)

      s += v1 + v2 + v3 + v4 + v5
      i += 5
    }
    s
  }

  @Setup(Level.Iteration)
  def setup() = {
    v1 = vec.randn(size)
    v2 = vec.randn(size)
  }
  @Benchmark
  def blas(): Double = {
    import org.saddle.linalg._
    v1 vv_blas v2
  }
  @Benchmark
  def raw_unrolled(): Double = {
    vv_java_unrolled(v1,v2)
  }
  @Benchmark
  def raw(): Double = {
    import org.saddle.linalg._
    v1 vv_java v2
  }

}
