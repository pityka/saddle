[![Build status](https://github.com/pityka/saddle/workflows/CI/badge.svg)](https://github.com/pityka/saddle/actions)
[![codecov](https://codecov.io/gh/pityka/saddle/branch/master/graph/badge.svg)](https://codecov.io/gh/pityka/saddle)
[![doc](https://img.shields.io/badge/api-scaladoc-green)](https://pityka.github.io/saddle/api/org/saddle/Frame.html)
[![doc](https://img.shields.io/badge/docs-green)](https://pityka.github.io/saddle)
[![maven](https://img.shields.io/maven-central/v/io.github.pityka/saddle-core_2.12.svg)](https://repo1.maven.org/maven2/io/github/pityka/saddle-core_2.12/)


This repository is a fork of the original Saddle library which has seen no 
activity for some time.

Saddle: Scala Data Library
==========================

Introduction
============

Saddle is a data manipulation library for Scala that provides array-backed,
indexed, one- and two-dimensional data structures that are judiciously
specialized on JVM primitives to avoid the overhead of boxing and unboxing.

Saddle offers vectorized numerical calculations, automatic alignment of data
along indices, robustness to missing (N/A) values, and facilities for I/O.

Saddle draws inspiration from several sources, among them the R programming
language & statistical environment, the numpy and pandas Python libraries,
and the Scala collections library.

Documentation
=============

 - [Docs](https://pityka.github.io/saddle)
 - [scaladoc](https://pityka.github.io/saddle/api/org/saddle/Frame.html)

How to build the code
=====================
You need [sbt](https://www.scala-sbt.org/): `sbt test`

How to build the website
========================
You need [sbt](https://www.scala-sbt.org/) and [hugo](https://gohugo.io/): `sbt docs/mdoc docs/unidoc && cd website && hugo`

License
=======

Saddle is distributed under the Apache License Version 2.0 (see LICENSE file).

Copyright
=========

Copyright (c) 2013-2015 Novus Partners, Inc.

Copyright (c) 2013-2015 The Saddle Development Team

All rights reserved.

Saddle is subject to a shared copyright. Each contributor retains copyright to
his or her contributions to Saddle, and is free to annotate these contributions
via code repository commit messages. The copyright to the entirety of the code
base is shared among the Saddle Development Team, comprised of the developers
who have made such contributions.

The copyright and license of each file shall read as follows:

> Copyright (c) 2013-2015 Saddle Development Team
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
> http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.


Individual contributors may, if they so desire, append their names to
the CONTRIBUTORS file.

Code in saddle-core/src/main/scala/org/saddle/util/LongMap.scala has different copyright terms,
see its header.

Code in saddle-core/src/main/scala/org/saddle/Buffer.scala has different copyright terms,
see its header. 

Code in spire-prng has different copyright terms, see the spire-prng/COPYING.

About the Copyright Holders
===========================

Adam Klein began Saddle development in 2012 while an employee of 
[Novus Partners, Inc](http://www.novus.com "Novus"). The code was 
released by Novus under this license in 2013. Adam Klein is lead 
developer. Saddle was inspired by earlier prototypes developed by 
Chris Lewis, Cheng Peng, & David Cru. Saddle was also inspired by 
previous work with [pandas](http://pandas.pydata.org/ "pandas"), a 
data analysis library written in Python. 

Code in the saddle-linalg/ folder is contributed by Istvan Bartha.

