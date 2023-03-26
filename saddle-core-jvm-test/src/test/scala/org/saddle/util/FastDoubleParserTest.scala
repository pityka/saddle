/** These test cases are licensed as Copyright © 2023 Werner Randelshofer,
  * Switzerland. MIT License.
  */
package org.saddle.util

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._

class FastDoubleParserTest extends Specification with ScalaCheck {

  def testOK(s: String, exp: Double) = {
    val got = FastDoubleParser.parseFloatValue(s.toCharArray(), 0, s.length)
    assert(
      got == exp,
      s"chars: '$s'  expected: $exp , but got $got"
    )
  }
  def testOK(comment: String, s: String, exp: Double) = {
    assert(
      FastDoubleParser.parseFloatValue(s.toCharArray(), 0, s.length) == exp
    )
  }
  def testIllegal(s: String) = {
    assert(
      FastDoubleParser.parseFloatValue(s.toCharArray(), 0, s.length).isNaN
    )
  }

  "test" in {
    testOK("1.2e3", 1.2e3)
    1 must_== 1
  }

  "illegal" in {
    testIllegal("empty")
    testIllegal("+")
    testIllegal("-")
    testIllegal("+e")
    testIllegal("-e")
    testIllegal("+e123")
    testIllegal("-e456")
    testIllegal("78 e9")
    testIllegal("-01 e23")
    testIllegal("- 1")
    testIllegal("-0 .5")
    testIllegal("-0. 5")
    testIllegal("-0.5 e")
    testIllegal("-0.5e 3")
    testIllegal("45\ne6")
    testIllegal("d")
    testIllegal(".f")
    testIllegal("7_8e90")
    testIllegal("12e3_4")
    testIllegal("00x5.6p7")
    testIllegal("89p0")
    testIllegal("cafebabe.1p2")
    testIllegal("0x123pa")
    testIllegal("0x1.2e7")
    testIllegal("0xp89")
    testIllegal("1d")
    1 must_== 1
  }

  "legal" in {
    testOK(
      Double.MinValue.toString,
      Double.MinValue
    )
    testOK("0", 0)
    testOK("00", 0)
    testOK("007", 7)
    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e3", 1.2e3)
    testOK("1.2E3", 1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("+1", 1)
    testOK("+1.2", 1.2)
    testOK("+1.2e3", 1.2e3)
    testOK("+1.2E3", 1.2e3)
    testOK("+1.2e3", 1.2e3)
    testOK("-1", -1)
    testOK("-1.2", -1.2)
    testOK("-1.2e3", -1.2e3)
    testOK("-1.2E3", -1.2e3)
    testOK("-1.2e3", -1.2e3)
    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e-3", 1.2e-3)
    testOK("1.2E-3", 1.2e-3)
    testOK("1.2e-3", 1.2e-3)

    testOK("1.2", 1.2)
    testOK("1.2e-3", 1.2e-3)
    testOK("1.2E-3", 1.2e-3)
    testOK("1.2e-3", 1.2e-3)

    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e-3", 1.2e-3)
    testOK("1.2E-3", 1.2e-3)
    testOK("1.2e-3", 1.2e-3)
    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e-3", 1.2e-3)
    testOK("1.2E-3", 1.2e-3)
    testOK("1.2e-3", 1.2e-3)
    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e-3", 1.2e-3)
    testOK("1.2E-3", 1.2e-3)
    testOK("1.2e-3", 1.2e-3)

    testOK("1", 1)
    testOK("1.2", 1.2)
    testOK("1.2e+3", 1.2e3)
    testOK("1.2E+3", 1.2e3)
    testOK("1.2e+3", 1.2e3)
    testOK("-1.2e+3", -1.2e3)
    testOK("-1.2E-3", -1.2e-3)
    testOK("+1.2E+3", 1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("-1.2e3", -1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("1.2e3", 1.2e3)
    testOK("1234567890", 1234567890d)
    testOK("000000000", 0d)
    testOK("0000.0000", 0d)
    testOK(
      Double.MaxValue.toString,
      Double.MaxValue
    )
    testOK(
      math.nextUp(0.0).toString,
      math.nextUp(0.0)
    )
    testOK(
      math.nextDown(0.0).toString,
      Math.nextDown(0.0)
    )

    testOK(
      "Dec Double: Inside Clinger fast path \"1000000000000000000e-325\")",
      "1000000000000000000e-325",
      1000000000000000000e-325d
    )
    testOK(
      "Dec Double: Inside Clinger fast path (max_clinger_significand, max_clinger_exponent)",
      "9007199254740991e22",
      9007199254740991e22d
    )
    testOK(
      "Dec Double: Outside Clinger fast path (max_clinger_significand, max_clinger_exponent + 1)",
      "9007199254740991e23",
      9007199254740991e23d
    )
    testOK(
      "Dec Double: Outside Clinger fast path (max_clinger_significand + 1, max_clinger_exponent)",
      "9007199254740992e22",
      9007199254740992e22d
    )
    testOK(
      "Dec Double: Inside Clinger fast path (min_clinger_significand + 1, min_clinger_exponent)",
      "1e-22",
      1e-22d
    )
    testOK(
      "Dec Double: Outside Clinger fast path (min_clinger_significand + 1, min_clinger_exponent - 1)",
      "1e-23",
      1e-23d
    )
    testOK(
      "Dec Double: Outside Clinger fast path, bail-out in semi-fast path, 1e23",
      "1e23",
      1e23d
    )
    testOK(
      "Dec Double: Outside Clinger fast path, mantissa overflows in semi-fast path, 7.2057594037927933e+16",
      "7.2057594037927933e+16",
      7.2057594037927933e+16d
    )
    testOK(
      "Dec Double: Outside Clinger fast path, bail-out in semi-fast path, 7.3177701707893310e+15",
      "7.3177701707893310e+15",
      7.3177701707893310e+15d
    )
    1 must_== 1
  }

  "FastDoubleParser on 768 long" in {
    testOK(
      "2.22507385850720212418870147920222032907240528279439037814303133837435107319244194686754406432563881851382188218502438069999947733013005649884107791928741341929297200970481951993067993290969042784064731682041565926728632933630474670123316852983422152744517260835859654566319282835244787787799894310779783833699159288594555213714181128458251145584319223079897504395086859412457230891738946169368372321191373658977977723286698840356390251044443035457396733706583981055420456693824658413747607155981176573877626747665912387199931904006317334709003012790188175203447190250028061277777916798391090578584006464715943810511489154282775041174682194133952466682503431306181587829379004205392375072083366693241580002758391118854188641513168478436313080237596295773983001708984374E-308",
      2.225073858507202e-308
    )

    testOK(
      "2.22507385850720212418870147920222032907240528279439037814303133837435107319244194686754406432563881851382188218502438069999947733013005649884107791928741341929297200970481951993067993290969042784064731682041565926728632933630474670123316852983422152744517260835859654566319282835244787787799894310779783833699159288594555213714181128458251145584319223079897504395086859412457230891738946169368372321191373658977977723286698840356390251044443035457396733706583981055420456693824658413747607155981176573877626747665912387199931904006317334709003012790188175203447190250028061277777916798391090578584006464715943810511489154282775041174682194133952466682503431306181587829379004205392375072083366693241580002758391118854188641513168478436313080237596295773983001708984375E-308",
      2.2250738585072024e-308
    )
    1 must_== 1
  }

  "FastDoubleParser pass prepared data" in {
    val file = getClass.getResourceAsStream("/lemire-fast-double-parser.txt")
    val lines = scala.io.Source.fromInputStream(file).getLines()
    var count = 0L
    lines.foreach { line =>
      val spl = line.split(" ")
      val expected =
        if (spl(3).toLowerCase.contains("x")) Double.NaN
        else
          java.lang.Double.longBitsToDouble(
            java.lang.Long.parseLong(spl(2), 16)
          )

      val got = org.saddle.util.FastDoubleParser
        .parseFloatValue(spl(3).toCharArray(), 0, spl(3).length)
      assert((expected.isNaN && got.isNaN) || expected == got)
      count += 1
    }
    94313 must_== count
  }

  "FastDoubleParser pass on random doubles" in {

    0 until 10000000 foreach { _ =>
      val a = scala.util.Random.nextDouble()
      val str = a.toString
      val got = org.saddle.util.FastDoubleParser
        .parseFloatValue(str.toCharArray(), 0, str.length)

      assert((a.isNaN && got.isNaN) || a == got, s"$a != $got")

    }
    1 must_== 1
  }

}
