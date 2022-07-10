package org.saddle

import org.saddle.scalar.ScalarTag
import org.saddle.vec.VecDefault

import org.openjdk.jmh.annotations._

class VecBoxed[T: ScalarTag](values: Array[T])
    extends VecDefault[T](values, implicitly[ScalarTag[T]])

/** Benchmark suite illustrating the performance boost of specialization.
  */
@State(Scope.Benchmark)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@Threads(1)
class Specialization {
  @Param(Array("10", "10000"))
  var size: Int = _

  var v1: Vec[Double] = _
  var v2: VecBoxed[Double] = _
  var b: Double = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = vec.rand(size)
    v2 = new VecBoxed(v1.toArray)
    b = scala.util.Random.nextDouble()
  }

  @Benchmark
  def vecSpecialized(): Vec[Double] = {
    import org.saddle.ops.BinOps._
    v1 += b
    v1
  }

  @Benchmark
  def vecBoxed(): Vec[Double] = {
    import org.saddle.ops.BinOps._
    v2 += b
    v2
  }

}
