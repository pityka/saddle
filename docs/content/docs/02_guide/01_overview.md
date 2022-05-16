---
title: 'Overview'
weight: 1
---

# Overview
This is a high level overview of saddle. It contains links to more detailed sections.

## Construction 

```scala mdoc
import org.saddle._
val vec = Vec(1, 2, 3)
Series("a" -> 1, "b" -> 2, "c" -> 3)
val series = Series(Vec(1,2,3), Index("a", "b", "c"))
val series1 = Series(Vec(4,5,6), Index("a", "b", "c"))
val frame = Frame("col_a" -> series, "col_b" -> series1)
```

## Missing data

A key aspect of saddle is how it deals with missing data, also known as NA. Values in vectors, series and frames can be missing, keys can't.

```scala mdoc
Vec[Int](1, na, 3)
Series(Vec[Int](1, 2, na), Index("a", "b", "c"))
Frame("col_a" -> Series(Vec[Int](1, 2, na), Index("a", "b", "c")))
```

This is necessary for supporting alignment by index and other 
[joins]({{< relref "#joins" >}}).

## Selection
Elements can be selected by position:
```scala mdoc
vec.at(2)
series.at(2)
frame.at(0, 1)
frame.colAt(1)
frame.rowAt(0)
```
Or sliced:
```scala mdoc
vec.slice(0, 2)
series.slice(0, 2)
frame.rowSlice(0, 2)
```

Elements of `Series` and `Frame`s can further be selected by key:
```scala mdoc
series.get("a")
series(* -> "b")
frame.first("b").get("col_a")
frame("b" -> "c", "col_b" -> *)
```

## Joins
TODO

## Element-wise operations
TODO

## Linear algebra





