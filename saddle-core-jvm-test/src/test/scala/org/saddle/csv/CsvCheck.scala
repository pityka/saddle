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
package org.saddle.csv

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.saddle.{Index, Vec, Frame, na, ST, Series}
import org.saddle.order._

class CsvCheck extends Specification with ScalaCheck {
  val crlf = "\r\n"
  val lf = "\n"

  "csv string parsing works cols, empty" in {
    val data =
      s"""""""

    val src = ByteChannel(data)

    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, cols = List(1))

    frame must_== Left(
      "Unclosed quote after line 0 (not necessarily in that line)"
    )
  }
  "csv string parsing works cols" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val src = ByteChannel(data)

    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, cols = List(1))
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e")).colAt(Array(1))
    frame must_== expect
  }
  "csv string parsing works cols parseString" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val frame =
      CsvParser
        .parseString[String](data, bufferSize = 8, cols = List(1))
        .toOption
        .get
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e")).colAt(Array(1))
    frame must_== expect
  }
  "csv string parsing works cols 0,2" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val src = ByteChannel(data)

    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, cols = List(0, 2))
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e")).colAt(Array(0, 2))
    frame must_== expect
  }

  "csv with many empty fields" in {
    val data =
      s"""a,b,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,${crlf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o${crlf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o"""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4)
        .toOption
        .get
        ._1
    frame.colAt(0) must_== Series("a", "", "")
  }
  "csv with many empty fields LF" in {
    val data =
      s"""a,b,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,${lf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o${lf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o"""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4, recordSeparator = lf)
        .toOption
        .get
        ._1
    frame.colAt(0) must_== Series("a", "", "")
  }

  "csv write string NAs (represented as nulls) " in {
    val expect = Frame("abc" -> Series("a", null, "b"))

    val parsedBack = CsvParser
      .parseFromChannel[String](
        ByteChannel(
          new String(org.saddle.csv.CsvWriter.writeFrameToArray(expect))
        )
      )
      .toOption
      .get
      ._1
      .withColIndex(0)
      .withRowIndex(0)
      .mapRowIndex(_.toInt)

    parsedBack must_== expect.mapVec(_.fillNA(_ => "NA"))
  }

  "csv string parsing and writing works" in {

    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))

    val parsedBack = (CsvParser
      .parseFromChannel[String](
        ByteChannel(
          new String(org.saddle.csv.CsvWriter.writeFrameToArray(expect))
        )
      )
      .toOption
      .get
      ._1
      .withColIndex(0)
      .withRowIndex(0)
      .mapRowIndex(_.toInt))

    parsedBack must_== expect
  }
  "csv string parsing works, short buffer" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4)
        .toOption
    frame must_== None
  }
  "csv string parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val src = ByteChannel(data)

    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv int parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val src = ByteChannel(data)

    val (frame0, index) =
      CsvParser
        .parseFromChannel[Int](src, bufferSize = 8, header = true)
        .toOption
        .get

    val frame = frame0.resetRowIndex.setColIndex(index.get)
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e")).mapValues(_.toInt)
    frame must_== expect
  }
  "csv string parsing works, two lines" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, maxLines = 2)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1"),
      Vec("25"),
      Vec("36")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works, take first line" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, maxLines = 1)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec.empty[Double],
      Vec.empty[Double],
      Vec.empty[Double]
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works with LF line endings" in {
    val data =
      s"""a,"b,c,d",e${lf}1,25,36${lf}4,55,"6"${lf}5,9,38${lf}7,"8","9""""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 8, recordSeparator = lf)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec("25", "55", "9", "8"),
      Vec("36", "6", "38", "9")
    ).setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }
  "csv string parsing works with double quotes and quoted CRLF and unquoted CR" in {
    val data =
      s""","b,""c"",d",e${crlf}1,25${'\r'}1,${'\r'}${'\r'}${'\r'}36${crlf}4,5${'\r'}${'\r'}5,"6${crlf}1"${crlf}5,  ,38${crlf}7,"8${'\r'}1","""""

    val src = ByteChannel(data)

    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 14)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec(s"25${'\r'}1", s"5${'\r'}${'\r'}5", "  ", s"8${'\r'}1"),
      Vec(s"${'\r'}${'\r'}${'\r'}36", s"6${crlf}1", "38", "")
    ).setColIndex(Index("", """b,""c"",d""", "e"))
    frame must_== expect
  }
  "csv string parsing works with double quotes and quoted CRLF and unquoted CR with LF line endings" in {
    val data =
      s""","b,""c"",d",e${lf}1,25${'\r'}1,${'\r'}${'\r'}${'\r'}36${lf}4,5${'\r'}${'\r'}5,"6${lf}1"${lf}5,  ,38${lf}7,"8${'\r'}1","9""""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 12, recordSeparator = lf)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex

    val expect = Frame(
      Vec("1", "4", "5", "7"),
      Vec(s"25${'\r'}1", s"5${'\r'}${'\r'}5", "  ", s"8${'\r'}1"),
      Vec(s"${'\r'}${'\r'}${'\r'}36", s"6${lf}1", "38", "9")
    ).setColIndex(Index("", """b,""c"",d""", "e"))

    frame must_== expect
  }
  "quoted empty string" in {
    val data =
      s"""""${crlf}1"""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1")
    ).setColIndex(Index(""))
    frame must_== expect
  }
  "quoted empty string with LF line ending" in {
    val data =
      s"""""${lf}1"""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4, recordSeparator = lf)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("1")
    ).setColIndex(Index(""))
    frame must_== expect
  }
  "quoted empty string 2" in {
    val data =
      s"""1${crlf}"""""

    val src = ByteChannel(data)
    val frame =
      CsvParser
        .parseFromChannel[String](src, bufferSize = 4)
        .toOption
        .get
        ._1
        .withColIndex(0)
        .resetRowIndex
    val expect = Frame(
      Vec("")
    ).setColIndex(Index("1"))
    frame must_== expect
  }

  "csv int parsing works" in {
    val data =
      s"""a,"b,c,d",e${crlf}1,2,3${crlf}4,5,"test"${crlf}7,"8","9"""".stripMargin

    val src = ByteChannel(data)

    val frame = CsvParser
      .parseFromChannel[String](src, bufferSize = 8)
      .toOption
      .get
      ._1
      .withColIndex(0)
      .resetRowIndex
      .mapValues(s => implicitly[ST[Int]].parse(s.toCharArray, 0, s.length()))
    val expect = Frame(Vec(1, 4, 7), Vec(2, 5, 8), Vec(3, na[Int], 9))
      .setColIndex(Index("a", "b,c,d", "e"))
    frame must_== expect
  }

  "csv fails on irregular row (short)" in {
    val data =
      s"""a,"b,c,d",e${"\r\n"}1,2,3${"\r\n"}4,5${"\r\n"}7,"8","9",   """.stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src) must_== Left(
      "Too short line 3 (1-based). Expected 3 fields, got 2."
    )
  }
  "csv fails on irregular row (long)" in {
    val data =
      s"""a,"b,c,d",e${"\r\n"}1,2,3${"\r\n"}4,5,6${"\r\n"}7,"8","9",   """.stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src) must_== Left(
      "Too long line 4 (1-based). Expected 3 fields, got 4."
    )
  }

  "csv fails on too short header" in {
    val data =
      s"""a,a""".stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src, cols = List(2)) must_==
      Left("Header line to short for given locs: [2]. Header line: [a, a]")

  }
  "csv fails on too short header 2" in {
    val data =
      s"""a,a$crlf""".stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src, cols = List(2)) must_==
      Left("Header line to short for given locs: [2]. Header line: [a, a]")

  }

  "csv parsing still works when final field is empty" in {
    val data =
      """,""".stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src).toOption.get._1 must_== Frame(
      Vec(""),
      Vec("")
    )
  }
  "csv parsing works with consecutive empty fields" in {
    val data =
      s"""a,,c,d${crlf},,"",""".stripMargin

    val src = ByteChannel(data)

    CsvParser.parseFromChannel[String](src).toOption.get._1 must_==
      Frame(Vec("a", ""), Vec("", ""), Vec("c", ""), Vec("d", ""))

  }
}
