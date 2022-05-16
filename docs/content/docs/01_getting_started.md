---
title: 'Getting started'
weight: 1
---

## Saddle modules
Add the appropriate saddle modules to your build.sbt:
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

## Dependencies
The actively maintained artifacts have minimal dependencies:

- `saddle-core` depends on [cats-kernel](https://github.com/typelevel/cats)
- `saddle-linalg` depends on [netlib-java](https://github.com/fommil/netlib-java)
- `saddle-binary` depends on [ujson](http://www.lihaoyi.com/upickle/)
- `saddle-circe` depends on [circe](https://github.com/circe/circe)

## Imports
You most likely need the following two imports:
```scala
import org.saddle._
import org.saddle.order._
```

Note that `org.saddle.order._` imports `cats.kernel.Order[_]` typeclass instances into the scope. 
If you import cats instances an other way then you should not import `org.saddle.order._`. 

The `Order[Double]` and `Order[Float]` instances in `org.saddle.order` define a total ordering and 
order `NaN` above all other values, consistent with `java.lang.Double.compare`.

## Example: SVD on the Iris dataset
```scala mdoc:silent
import scala.io.Source
import org.saddle._
import org.saddle.linalg._
val irisURL = "https://gist.githubusercontent.com/pityka/d05bb892541d71c2a06a0efb6933b323/raw/639388c2cbc2120a14dcf466e85730eb8be498bb/iris.csv"
val iris = csv.CsvParser.parseSourceWithHeader[Double](
      source = Source.fromURL(irisURL), 
      cols = List(0,1,2,3), 
      recordSeparator = "\n").toOption.get
```
```scala mdoc
val centered = iris.mapVec(_.demeaned)
val SVDResult(u, s, vt) = centered.toMat.svd(2)
val pca = u.mDiagFromRight(s).toFrame
```