/** Copyright (c) 2023 Saddle Development Team
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
package org.saddle.io.csv

import org.scalatest.funsuite.AnyFunSuite
import java.nio.CharBuffer
import java.nio.ByteBuffer

class CSVSuite extends AnyFunSuite {

  test("single line") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,b,c,d,e"""

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("a", "b", "c", "d", "e"))

  }
  test("test1") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(
      buffer.toList == List(
        "a",
        "b,c,d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test1 tsv") {
    val crlf = "[]"
    val lf = "\n"
    val tab = "\t"
    val data =
      s"""a$tab"b${tab}c${tab}d"${tab}e${lf}1${tab}25${tab}36${tab}${lf}4${tab}55${tab}"6"${lf}5${tab}9${tab}38${lf}7${tab}"8"${tab}"9"${tab}   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf,
      fieldSeparator = '\t'
    )

    assert(
      buffer.toList == List(
        "a",
        s"b${tab}c${tab}d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test1 csv lf") {
    val crlf = "[]"
    val lf = "\n"
    val tab = ","
    val data =
      s"""a$tab"b${tab}c${tab}d"${tab}e${lf}1${tab}25${tab}36${tab}${lf}4${tab}55${tab}"6"${lf}5${tab}9${tab}38${lf}7${tab}"8"${tab}"9"${tab}   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf,
      fieldSeparator = ','
    )

    assert(
      buffer.toList == List(
        "a",
        s"b${tab}c${tab}d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test1 csv crlf") {
    val crlf = "\r\n"
    val lf = "\n"
    val tab = ","
    val data =
      s"""a$tab"b${tab}c${tab}d"${tab}e${crlf}1${tab}25${tab}36${tab}${crlf}4${tab}55${tab}"6"${crlf}5${tab}9${tab}38${crlf}7${tab}"8"${tab}"9"${tab}   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf,
      fieldSeparator = ','
    )

    assert(
      buffer.toList == List(
        "a",
        s"b${tab}c${tab}d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test1 - quote in quote") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,"b,""c"",d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 12,
      recordSeparator = crlf
    )

    assert(
      buffer.toList == List(
        "a",
        "b,\"\"c\"\",d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test2") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,"b,c,d",e${lf}1,25,36,${lf}4,55,"6"${lf}5,9,38${lf}7,"8","9",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf
    )

    assert(
      buffer.toList == List(
        "a",
        "b,c,d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9",
        "   "
      )
    )

  }
  test("test3") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,a""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf
    )

    assert(buffer.toList == List("a", "a"))

  }
  test("test4") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,,c,d${crlf},,"",""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("a", "", "c", "d", "", "", "", ""))

  }
  test("test4b") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,,c,d${crlf},,"",$crlf""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("a", "", "c", "d", "", "", "", ""))

  }
  test("test4 lf") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,,c,d${lf},,"",""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf
    )

    assert(buffer.toList == List("a", "", "c", "d", "", "", "", ""))

  }
  test("test4b lf") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,,c,d${lf},,"",$lf""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = lf
    )

    assert(buffer.toList == List("a", "", "c", "d", "", "", "", ""))

  }
  test("test5") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s""""a"a""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("a\"a"))

  }

  test("empty final") {
    val crlf = "[]"
    val lf = "]"
    val data =
      """,""".stripMargin

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("", ""))

  }

  test("quoted empty string") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""""${crlf}1"""

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(buffer.toList == List("", "1"))

  }
  test("many empty fields crlf") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,b,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,${crlf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o${crlf},,c,,,,d,e,f,g,h,i,j,k,l,,,,,,,,,,,m,,,o"""

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(
      buffer.toList == List(
        "a",
        "b",
        "c",
        "",
        "",
        "",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "k",
        "l",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "m",
        "",
        "",
        "",
        "",
        "",
        "c",
        "",
        "",
        "",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "k",
        "l",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "m",
        "",
        "",
        "o",
        "",
        "",
        "c",
        "",
        "",
        "",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "k",
        "l",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "m",
        "",
        "",
        "o"
      )
    )

  }
  test("string parsing with too short buffer") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    val res = org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 4,
      recordSeparator = crlf
    )

    assert(res == Some("Buffer too short for this data."))

  }
  test("string parsing ") {
    val crlf = "[]"
    val lf = "]"
    val data =
      s"""a,"b,c,d",e${crlf}1,25,36,${crlf}4,55,"6"${crlf}5,9,38${crlf}7,"8","9""""

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    val res = org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 8,
      recordSeparator = crlf
    )

    assert(
      buffer.toList == List(
        "a",
        "b,c,d",
        "e",
        "1",
        "25",
        "36",
        "",
        "4",
        "55",
        "6",
        "5",
        "9",
        "38",
        "7",
        "8",
        "9"
      )
    )

  }
  test(
    "csv string parsing works with double quotes and quoted CRLF and unquoted CR"
  ) {
    val crlf = "[]"
    val lf = "]"
    val cr = "["
    val data =
      s""","b,""c"",d",e${crlf}1,25${cr}1,${cr}${cr}${cr}36,${crlf}4,5${cr}${cr}5,"6${crlf}1"${crlf}5,  ,38${crlf}7,"8${cr}1","",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 12,
      recordSeparator = crlf
    )
    assert(
      buffer.toList == List(
        "",
        "b,\"\"c\"\",d",
        "e",
        "1",
        s"25${cr}1",
        s"${cr}${cr}${cr}36",
        "",
        "4",
        s"5${cr}${cr}5",
        s"6${crlf}1",
        "5",
        "  ",
        "38",
        "7",
        s"8${cr}1",
        "",
        "   "
      )
    )
  }
  test(
    "csv string parsing works with double quotes and quoted CRLF and unquoted CR with LF ending"
  ) {
    val crlf = "[]"
    val lf = "]"
    val cr = "["
    val data =
      s""","b,""c"",d",e${lf}1,25${cr}1,${cr}${cr}${cr}36,${lf}4,5${cr}${cr}5,"6${lf}1"${lf}5,  ,38${lf}7,"8${cr}1","",   """

    val src = ByteChannel(data)
    val buffer = new BufferCallback
    org.saddle.io.csv.parse(
      src,
      buffer,
      bufferSize = 12,
      recordSeparator = lf
    )
    assert(
      buffer.toList == List(
        "",
        "b,\"\"c\"\",d",
        "e",
        "1",
        s"25${cr}1",
        s"${cr}${cr}${cr}36",
        "",
        "4",
        s"5${cr}${cr}5",
        s"6${lf}1",
        "5",
        "  ",
        s"38",
        "7",
        s"8${cr}1",
        "",
        "   "
      )
    )
  }

}
