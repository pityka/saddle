package org.saddle

import org.openjdk.jmh.annotations._
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@Fork(1)
@Threads(1)
class IntParserBench {
  @Param(Array( "5000"))
  var size: Int = _

  var v1: Array[Array[Char]] = _
  var v2: Array[String] = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = vec.randn(size).map(_ => scala.util.Random.nextInt().toString).toArray.map(_.toCharArray())
    v2 = v1.map(s => new String(s,0,s.length))
  }
  @Benchmark
  def saddle(): Unit = {
    var i = 0 
    while (i < v1.length) {
      org.saddle.scalar.ScalarTagInt.parse(v1(i),0,v1(i).length)
      i+=1
    }
  }
  @Benchmark
  def javalanginteger(): Unit = {
    var i = 0 
    while (i < v1.length) {
      try{
      java.lang.Integer.parseInt(v2(i))
      } catch {
        case _ => Int.MinValue
      }
      i+=1
    }
  }
 

}
