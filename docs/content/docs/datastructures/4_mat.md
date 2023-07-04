---
title: '2D array (matrix)'
weight: 4
---

# Matrix: Mat[T]

```scala
import org.saddle._
import org.saddle.order._
```

A `Mat[T]` represents a matrix of values. Internally it is stored as a single contiguous array in row-major order.

The row-major layout is compatible with EJML, a linear algebra library running on the JVM. 
If you want to use a BLAS, you can take a look in `saddle-linalg`.

## Factories

```scala mdoc
import org.saddle._
 Mat(2,2, Array(1,2,3,4))

// all same:
 Mat(Array(Array(1,3), Array(2,4)))
 Mat(Vec(1,3), Vec(2,4))
 Mat(Array(Vec(1,3), Vec(2,4)))

// identity matrix:
 mat.ident(2)

// empty matrix:
 Mat.empty[Double]

// zeros:
 Mat[Int](2, 2)
```

Again, sometimes we want to create instances filled with random observations. As to Vec, we can do the following:

```scala mdoc
 mat.rand(2,2)       
 mat.randp(2,2)      // random positive doubles
 mat.randn(2,2)      // random normally distributed doubles
 mat.randn2(2,2,3,12) // random normally distributed with mean=3, stdev=12
``` 
There are a few other factory methods available:

```scala mdoc
 mat.ones(2,2)
 mat.zeros(2,2)
 mat.diag(Vec(1,2))
``` 

## Binary operations
Let’s look at some basic operations with Mat. As with Vec, you may perform calculations on two Mat instances, or on a Mat and a scalar value.

```scala mdoc
import org.saddle.ops.BinOps._
// element-wise multiplication
 Mat(2,2,Array(1,2,3,4)) * Mat(2,2,Array(4,1,2,3))
```

## Linear algebra
Matrix multiplication, matrix-vector multiplication or other linear algebra operatoins need `saddle-linalg`, which depends on netlib-java. These work just on `Mat[Double]` and `Vec[Double]`. 

```scala mdoc
import org.saddle.linalg._
// matrix multiplication
 Mat(2,2,Array(1d,2d,3d,4d)) mm Mat(2,2,Array(4d,1d,2d,3d))


// matrix-vector multiplication
 Mat(2,2,Array(1d,2d,3d,4d)) mv Vec(2d,1d)

// as expected
 Mat(2,2,Array(1,2,3,4)) * 2
 Mat(2,2,Array(1,2,3,4)) + 2
 Mat(2,2,Array(1,2,3,4)) << 2
// etc...

// transpose
 Mat(2,2,Array(1,2,3,4)).T

// properties of Mat
val m = Mat(2,2,Array(1,2,3,4))
 m.numRows
 m.numCols
 m.isSquare
 m.isEmpty
``` 

## Operations 
There are a few ways to extract values from a Mat.
```scala mdoc
 m.at(0,1)

// be careful with this one!
 m.raw(0,1)

 m.takeRows(0)

 m.withoutRows(0)

 m.takeCols(0)

 m.col(0)
 m.row(0)
 m.rows
 m.cols
``` 
Some other interesting methods on Mat:

```scala mdoc
 val m2 = Mat(2,2,Array(1,2,na[Int],4))

 m2.rowsWithNA
 m2.dropRowsWithNA
 m2.reshape(1,4)
 mat.rand(2,2).roundTo(2)
``` 