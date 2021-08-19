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
import java.nio.charset.Charset

class CsvCheck extends Specification with ScalaCheck {
  val crlf = "\r\n"
  val lf = "\n"
  "file reading works" in {
    val tmp = java.io.File.createTempFile("test", "test")
    val os = new java.io.FileOutputStream(tmp)
    val text = "abcdefgh"
    os.write(text.getBytes("US-ASCII"))
    os.close
    val text2 = CsvParser
      .readFile(tmp, bufferSize = 2, CsvParser.asciiSilentCharsetDecoder)
      .map(_.toString)
      .mkString
    text must_== text2
  }
  "file reading works, utf16" in {
    val tmp = java.io.File.createTempFile("test", "test")
    val os = new java.io.FileOutputStream(tmp)
    val text = "abcdefgh…πœ"
    os.write(text.getBytes("UTF-16"))
    os.close
    val text2 = CsvParser
      .readFile(tmp, bufferSize = 2, Charset.forName("UTF-16").newDecoder)
      .map(_.toString)
      .mkString
    text must_== text2
  }
}
