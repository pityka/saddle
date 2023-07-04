---
title: 'Getting started'
weight: 1
---

Add any of these lines to your build.sbt:
```scala
// The core library
libraryDependencies += "io.github.pityka" % "saddle-core" % "@VERSION@"
// Inlined binary operation instances for Mat and Vec
libraryDependencies += "io.github.pityka" % "saddle-ops-inlined" % "@VERSION@"
// Linear algebra interface to BLAS
libraryDependencies += "io.github.pityka" % "saddle-linalg" % "@VERSION@"
// Binary representation of data frames and matrices
libraryDependencies += "io.github.pityka" % "saddle-binary" % "@VERSION@"
// Circe Encoder and Decoder instances
libraryDependencies += "io.github.pityka" % "saddle-circe" % "@VERSION@"
// Interface to joda time (not maintained)
libraryDependencies += "io.github.pityka" % "saddle-time" % "@VERSION@"
// Interface to EJML (not maintained)
libraryDependencies += "io.github.pityka" % "saddle-stats" % "@VERSION@"
```

### Dependencies
The actively maintained artifacts have minimal dependencies:

- `saddle-io` has no dependency (not even saddle-core)
- `saddle-core` depends on [cats-kernel](https://github.com/typelevel/cats)
- `saddle-linalg` depends on a fork of [netlib-java](https://github.com/pityka/netlib-java)
- `saddle-binary` depends on [ujson](http://www.lihaoyi.com/upickle/)
- `saddle-circe` depends on [circe](https://github.com/circe/circe)
- `saddle-jsoniter` depends on [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala)

### Example: SVD on the Iris dataset
```scala mdoc
import scala.io.Source
import org.saddle._
val irisURL = "https://gist.githubusercontent.com/pityka/d05bb892541d71c2a06a0efb6933b323/raw/639388c2cbc2120a14dcf466e85730eb8be498bb/iris.csv"
val iris : Frame[Int,String,Double] = csv.CsvParser.parseInputStreamWithHeader[Double](
      inputStream = new java.net.URL(irisURL).openStream, 
      cols = List(0,1,2,3), 
      recordSeparator = "\n").toOption.get

import org.saddle.linalg._
val centered = iris.mapVec(_.demeaned)
val SVDResult(u, s, vt) = centered.toMat.svd(2)
val pca = u.mDiagFromRight(s).toFrame

val joined = iris.rconcat(pca.mapColIndex(i => s"PCA$i"))
val reducedByColumn = joined.reduce(_.values.mean)
val reducedByRow = joined.rreduce(_.values.max)
```