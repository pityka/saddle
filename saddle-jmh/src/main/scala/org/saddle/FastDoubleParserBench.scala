package org.saddle

import org.openjdk.jmh.annotations._
@State(Scope.Benchmark)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@Fork(1)
@Threads(1)
class FastDoubleParserBench {
  @Param(Array( "5000"))
  var size: Int = _

  var v1: Array[Array[Char]] = _
  var v2: Array[String] = _

  @Setup(Level.Iteration)
  def setup() = {
    v1 = vec.randn(size).map(_.toString).toArray.map(_.toCharArray())
    v2 = v1.map(s => new String(s,0,s.length))
  }
  @Benchmark
  def fastdoubleparser(): Unit = {
    var i = 0 
    while (i < v1.length) {
      org.saddle.util.FastDoubleParser.parseFloatValue(v1(i),0,v1(i).length)
      i+=1
    }
  }
  @Benchmark
  def javalangdouble(): Unit = {
    var i = 0 
    while (i < v1.length) {
      v2(i).toDouble
      i+=1
    }
  }
 

}
