package org.saddle.npy

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
import org.specs2.mutable.Specification
import org.saddle._
class NpyTest extends Specification {
  "read vec" in {
    val read =
      Reader
        .readVecFromChannel[Double](
          java.nio.channels.Channels
            .newChannel(getClass.getResourceAsStream("/file.npy"))
        )
        .toOption
        .get

    read must_== ((Vec(1d, 0d, 0d, 0d, 1d, 0d, 0d, 0d, 1d), List(3, 3)))
  }
  "read vec 2" in {

    val read =
      Reader
        .readVecFromChannel[Double](
          java.nio.channels.Channels
            .newChannel(getClass.getResourceAsStream("/ones.npy"))
        )
        .toOption
        .get

    read._2 must_== List(3)
  }
  "read vec 3" in {
    val read =
      Reader
        .readVecFromChannel[Double](
          java.nio.channels.Channels
            .newChannel(getClass.getResourceAsStream("/ones2.npy"))
        )
        .toOption
        .get

    read._2 must_== List(3, 3, 3)
  }
  "read mat" in {
    val read =
      Reader
        .readMatFromChannel[Double](
          java.nio.channels.Channels
            .newChannel(getClass.getResourceAsStream("/file.npy"))
        )
        .toOption
        .get

    read must_== (Mat(Vec(1d, 0d, 0d), Vec(0d, 1d, 0d), Vec(0d, 0d, 1d)))
  }
}
