package org.saddle.util


import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._

class FastDoubleParserTest extends Specification  with ScalaCheck {

  "FastDoubleParser pass prepared data" in {
    val file = getClass.getResourceAsStream("/lemire-fast-double-parser.txt")
    val lines = scala.io.Source.fromInputStream(file).getLines()
    var count = 0L
    lines.foreach{ line =>
      val spl = line.split(" ")
      val expected = if (spl(3).toLowerCase.contains("x")) Double.NaN
      else java.lang.Double.longBitsToDouble(java.lang.Long.parseLong(spl(2), 16)) 
      
      val got = org.saddle.util.FastDoubleParser.parseFloatValue(spl(3).toCharArray(),0,spl(3).length)
       assert((expected.isNaN && got.isNaN) || expected == got)
       count+=1
    }
    94313 must_== count
  }

   "FastDoubleParser pass on random doubles" in {

     0 until 10000000 foreach { _ =>  
      val a = scala.util.Random.nextDouble()
        val str = a.toString 
        val got = org.saddle.util.FastDoubleParser.parseFloatValue(str.toCharArray(),0,str.length)
        
       assert((a.isNaN && got.isNaN) || a == got,s"$a != $got")
      
    }
    1 must_== 1
  }

}
