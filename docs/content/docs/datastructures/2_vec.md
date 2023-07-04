---
title: 'Vector'
weight: 2
---


# 1D vector: Vec[T]

```scala
import org.saddle._
import org.saddle.order._
```

## Factories
```scala mdoc
import org.saddle._
Vec(1, 2, 3)  
Vec(1 to 3 : _*)    
Vec(Array(1,2,3))
Vec.empty[Double] 

vec.ones(2)
vec.zeros(3)
vec.rand(20)
```

## Operations
```scala mdoc:nest
import org.saddle.ops.BinOps._

Vec(1,2,3) + Vec(4,5,6)
Vec(1,2,3) * Vec(4,5,6)
Vec(1,2,3) ** Vec(4,5,6)
```
Note above, that you have to import that `BinOp` instances. 

An alternative set of instances are also available which inline the innermost in Mat and Vec operations:
```scala mdoc:reset
// The macros package contains instances where the innermost loop is inlined.
import org.saddle._
import org.saddle.macros.BinOps._ 
Vec(1,2,3) + Vec(4,5,6)
Vec(1,2,3) * Vec(4,5,6)
Vec(1,2,3) ** Vec(4,5,6)
 
```

## Slicing
```scala mdoc:reset
import org.saddle._
import org.saddle.ops.BinOps._
Vec(1,2,3).at(2) // Boxes and keeps NA
Vec(1,2,3).raw(2)  
Vec(1,2,3).apply(2) // same as raw
Vec(1,2,3).take(0,2)
Vec(1,2,3).take(1 -> *)
Vec(1,2,3).take(* -> 1)
```