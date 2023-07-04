---
title: 'Data frame'
weight: 5
---

# Homogeneous table with row and column index (data frame) : Frame[RX,CX,T]


A Frame combines a Mat with a row index and a column index which provides a way to index into the Mat. 

A Frame is represented internally as a sequence of column Vec instances all sharing the same row index.

## Factories

```scala mdoc
import org.saddle._
import org.saddle.order._
import org.saddle.ops.BinOps._
 val v = Vec(1, 2)                              // given the following
 val u = Vec(3, 4)
 val s = Series(Vec(1,3,2,4), Index("c", "b", "a", "b")).sortedIx
 val s2 = Series("a" -> 1, "b" -> 2)
 val t = Series("b" -> 3, "c" -> 4)

 Frame(v, u)                                    // two-column frame

 Frame("x" -> v, "y" -> u)                      // with column index

 Frame(s2, t)                                    // aligned along rows

 Frame("x" -> s2, "y" -> t)                      // with column index

 Frame(Seq(s2, t), Index("x", "y"))              // explicit column index

 Frame(Seq(v, u), Index(0, 1), Index("x", "y")) // row & col indexes specified explicitly

 Frame(Seq(v, u), Index("a", "b"))              // col index specified
```

The factory methods which construct a Frame from columns as Series come in two flavors 
regarding their behavior upon non-unique indices in the series (duplicate row index values).
- The Frame.apply methods create a full cross product of the respective indices. 
This means that for a given value of the row index all respective items will be paired with all other,
potentially leading to a combinatorial explosion of the number of rows of the resulting Frame in case of duplicated index values.
- The Frame.fromCols methods disambiguate the non-unique indices before joining. 
This avoids the combinatorial increase in the number of rows, at the cost of arbitrarily joining
items with the same index value.

An example for the difference between Frame.apply and Frame.fromCols, note the rows with 0 index:
```scala mdoc
  Frame.fromCols(
        Series(0 -> 1, 2 -> 2, 1 -> 3, 0 -> 4),
        Series(1 -> 1, 2 -> 2, 0 -> 3, 0 -> 4),
        Series(0 -> 1, 1 -> 2, 2 -> 3, 0 -> 4)
      )

  Frame.apply(
        Series(0 -> 1, 2 -> 2, 1 -> 3, 0 -> 4),
        Series(1 -> 1, 2 -> 2, 0 -> 3, 0 -> 4),
        Series(0 -> 1, 1 -> 2, 2 -> 3, 0 -> 4)
      )
```

## Operations

You’ll notice that if an index is not provided, a default int index is set where the index ranges between 0 and the length of the data.

If you want to set or reset the index, these methods are your friends:
```scala mdoc
 val f = Frame("x" -> s2, "y" -> t)

 f.setRowIndex(org.saddle.index.IndexIntRange(f.numRows))
 f.setColIndex(Index("p", "q"))
 f.resetRowIndex
 f.resetColIndex
```
(Note: frame f will carry through the next examples.)

You also have the following index transformation tools at hand:
```scala mdoc
f.mapRowIndex { case rx => rx+1 }
f.mapColIndex { case cx => cx+2 }
```
Let’s next look at how to extract data from the Frame.
```scala mdoc
 f.rowAt(2)    // extract row at offset 2, as Series
 f.rowAt(1,2)  // extract frame of rows 1 & 2
 f.rowAt(1->2) // extract frame of rows 1 & 2

 f.colAt(1)    // extract col at offset 1, as Series
 f.colAt(0,1)  // extract frame of cols 1 & 2
 f.colAt(0->1) // extract frame of cols 1 & 2
``` 
rowAt and colAt are used under the hood for the at extractor:
```scala mdoc
 f.at(1,1)              // Scalar value
 f.at(Array(1,2), 0)    // extract rows 1,2 of column 0
```
If you want more control over slicing, you can use these methods:

```scala mdoc
 f.colSlice(0,1)        // frame slice consisting of column 0
 f.rowSlice(0,3,2)      // row slice from 0 until 3, striding by 2
``` 
Of course, this is an bi-indexed data structure, so we can use its indexes to select out data using keys:
```scala mdoc
 f.row("a")             // row series 'a', with all columns
 f.col("x")             // col series 'x', with all rows
 f.row("a", "c")        // select two rows
 f.row("a"->"b")        // slice two rows (index must be sorted)
 f.row(Seq("a", "c"):_*)   // another way to select
``` 
A more explict way to slice with keys is as follows, and you can specify whether the right bound is inclusive or exclusive. Again, to slice, the index keys must be ordered.

```scala mdoc
 f.rowSliceBy("a", "b", inclusive=false)
 f.colSliceBy("x", "x", inclusive=true)
``` 
The row and col methods are used under the hood for the apply method:

```scala mdoc
 f("a", "x")             // extract a one-element frame by keys
 f("a"->"b", "x")        // two-row, one-column frame
 f(Vec("a", "c").toArray, "x")   // same as above, but extracting, not slicing
``` 
The methods of extracting multiple rows shown above can of course be done on columns as well.

You can also split up the Frame by key or index:

```scala mdoc
 f.colSplitAt(1)          // split into two frames at column 1
 f.colSplitBy("y")

 f.rowSplitAt(1)
 f.rowSplitBy("b")
``` 
You extract some number of rows or columns:
```scala mdoc
 f.head(2)                // operates on rows
 f.tail(2)
 f.headCol(1)             // operates on cols
 f.tailCol(1)
``` 
Or the first & last of some key (which is helpful when you’ve got a multi-key index):
```scala mdoc
 f.first("b")              // first row indexed by "b" key
 f.last("b")               // last row indexed by "b" key
 f.firstCol("x")
 f.lastCol("x")
``` 
There are a few other methods of extracting data:

```scala mdoc
import org.saddle.linalg._
 f.filter { case s => s.toVec.map(_.toDouble).mean2 > 2.0 }  // any column whose series satisfies predicate
 f.filterIx { case x => x == "x" }    // col where index matches key "x"
 f.where(Vec(false, true))            // extract second column
``` 
There are analogous methods to operate on rows rather then columns:
```scala
rfilter
rfilterIx
rwhere
```
etc... in general, methods operate on a column-wise basis, whereas the r-counterpart does so on a row-wise basis.

You can drop cols (rows) containing any NA values:
```scala mdoc
 f.dropNA
 f.rdropNA
``` 
Let’s take a look at some operations we can do with Frames. We can do all the normal binary math operations with Frames, with either a scalar value or with another Frame. When two frames are involved, they are reindexed along both axes to match the outer join of their indices, but any missing observation in either will carry through the calculations.

```scala mdoc
 f + 1
 f * f
 val g = Frame("y"->Series("b"->5, "d"->10))
 f + g                      // one non-NA entry, ("b", "y", 8)
``` 
You can effectively supply your own binary frame operation using joinMap, which lets you control the join style on rows and columns:
```scala mdoc
 f.joinMap(g, rhow=index.LeftJoin, chow=index.LeftJoin) { case (x, y) => x + y }
```
If you want simply to align one frame to another without performing an operation, use the following method:
```scala mdoc
 val (fNew, gNew) = f.align(g, rhow=index.LeftJoin, chow=index.OuterJoin)
```
If you want to treat a Frame as a matrix to use in linear algebraic fashion, call the `toMat` method.

We can sort a frame in various ways:

```scala mdoc
 f.sortedRIx                // sorted by row index
 f.sortedCIx                // sorted by col index
 f.sortedRows(0,1)          // sort rows by (primary) col 0 and (secondary) col 1
 f.sortedCols(1,0)          // sort cols by (primary) row 1 and (secondary) row 0
``` 
We can also sort by an ordering provided by the result of a function acting on rows or cols:

```scala mdoc
 f.sortedRowsBy { case r => r.at(0) }   // sort rows by first element of row
 f.sortedColsBy { case c => c.at(0) }   // sort cols by first element of col
``` 
There are several mapping functions:

```scala mdoc
 f.mapValues { case t => t + 1 }        // add one to each element of frame
 import org.saddle.linalg._
 f.mapVec { case v => v.map(_.toDouble).demeaned }      // map over each col vec of the frame
 f.reduce { case s => s.toVec.map(_.toDouble).mean2 }          // collapse each col series to a single value
 f.transform { case s => s.reversed }   // transform each series; outerjoin results
``` 
We can mask out values:
```scal mdoc
 f.mask(_ > 2)                          // mask out values > 2
 f.mask(Vec(false, true, true))         // mask out rows 1 & 2 (keep row 0)
``` 
Columns (rows) containing only NA values can be dropped as follows:
```scala mdoc
 f.mask(Vec(true, false, false)).rsqueeze   // drop rows containing NA values
 f.rmask(Vec(false, true)).squeeze          // takes "x" column
``` 
We can groupBy in order to combine or transform:
```scala mdoc
import org.saddle.linalg._
 f.groupBy(_ == "a").combine(_.count)       // # obs in each column that have/not row key "a"
 f.groupBy(_ == "a").transform(_.map(_.toDouble).demeaned)  // contrived, but you get the idea hopefully!
```
We can join against another frame, or against a series. 
```scala mdoc
 f.rconcat(g, how=index.LeftJoin)              
 f.concat(g, how=index.LeftJoin)              
 f.cbind(g, how=index.LeftJoin)              
 f.rbind(g, how=index.LeftJoin)              
``` 
Btw, to join a Frame to a series, the call looks like this:
```scala mdoc
 s.joinF(g, how=index.LeftJoin)
``` 
