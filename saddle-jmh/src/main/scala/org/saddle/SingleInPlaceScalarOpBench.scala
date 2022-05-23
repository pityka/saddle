package org.saddle

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@Warmup(iterations = 4, time=3)
@Measurement(iterations = 6, time=5)
@Fork(1)
@Threads(1)
class SingleInPlaceScalarOpBench {
  @Param(Array("10", "100", "10000"))
  var size: Int = _

  var m1: Mat[Double] = _
  var b: Double = _

  @Setup(Level.Iteration)
  def setup() = {
    m1 = mat.randn(size, size)
    b = scala.util.Random.nextDouble()
  }
  @Benchmark
  def saddleVirtual(): Mat[Double] = {
    import org.saddle.ops.BinOps._
    m1 += b
    m1
  }
  @Benchmark
  def saddleInlined(): Mat[Double] = {
    org.saddle.macros.BinOps.matSclr_DD_Add(m1, b)
    m1
  }

  @Benchmark
  def array(): Mat[Double] = {
    val m1a = m1.toArray
    var i = 0
    i = 0
    val N = m1a.length
    while (i < N) {
      m1a(i) += b
      i += 1
    }
    m1
  }

}
