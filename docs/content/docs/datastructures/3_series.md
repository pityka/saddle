---
title: 'Indexed vector'
weight: 3
---


# 1D vector with index: Series[K,V]

A Series combines a Vec with an Index that provides an ordered key-value mapping. We’ll talk more about the details of Index later. 

The key type of a must have a natural ordering (ie, an Ordering of that type within the implicit scope). However, the Series maintains the order in which its data was supplied unless ordered othewise.

Let’s look at a few constructions:

```scala mdoc
import org.saddle.order._
import org.saddle._
import org.saddle.ops.BinOps._
// we already know we can convert a Vec
Series(Vec(32, 12, 9))

// we can pass a pair of tuples
Series("a" -> 1, "b" -> 2, "c" -> 3)

// any series of tuples will work, eg:
Series(List("a" -> 1, "b" -> 2, "c" -> 3) : _*)

// can pass data and index separately:
Series(Vec(1,2,3), Index("a", "b", "c"))

// you can create an empty Series like so:
Series.empty[String, Int]

// supplied order is maintained:
Series(Vec(1,2,3), Index("c", "b", "a"))

// unlike map, multiple keys are entirely fine:
Series(Vec(1,2,3,4), Index("c", "b", "a", "b"))
```

With construction out of the way, let’s look at a few ways we can get data out of a Series.

```scala mdoc
 val q = Series(Vec(1,3,2,4), Index("c", "b", "a", "b"))

// get the values or index
 q.values
 q.index

// extract value by numerical offset
 q.at(2)

 q.at(2,3,1)

// or extract key
 q.keyAt(2)

 q.keyAt(2,3,1)

// sort by index ordering
 q.sortedIx

// sort by value ordering
 q.sorted

// extract elements matching the index
 q("b")

 q("a", "b")

// notice ordering subtleties:
 q("b", "a")

// get first or last values
 q.first
 q.last

// or key
 q.firstKey
 q.lastKey

// "reindex" to a new index:
 q.reindex(Index("a","c","d"))

// or just by a sequence of keys:
 q.reindex("a","c","d")

// notice that 'slicing' ignores unknown keys:
 q("a", "d")
```

```scala mdoc:crash
// we cannot reindex with "b", because it isn't unique.
// (the problem is, which "b" would we choose?)
 q.reindex("a", "b")
```

```scala mdoc
// we can "reset" the index to integer labels
 q.resetIndex

// or to a new index altogether
 q.setIndex(Index("w", "x", "y", "z"))

// to 'slice', we need a sorted index; slice is inclusive by default
 val s = q.sortedIx
 s.sliceBy("b", "c")

// syntactic sugar is provided:
 s.sliceBy("b" -> "c")
 s.sliceBy(* -> "b")

// where slice is by offset, exclusive by default, and the
// index doesn't have to be sorted:
 q.slice(0,2)

// there are head/tail methods:
 q.head(2)
 q.tail(2)
```

Aside from extracting values, there are many fun ways to compute with Series. Try the following:

```scala mdoc
q.mapValues(_ + 1)
q.mapIndex(_ + "x")
q.shift(1)
q.filter(_ > 2)
q.filterIx(_ != "b")
q.filterAt { case loc => loc != 1 && loc != 3 }
q.find(_ == 2)
q.findKey { case x => x == 2 || x == 3 }
q.findOneKey { case x => x == 2 || x == 3 }
q.minKey
q.contains("a")
q.scanLeft(0) { case (acc, v) => acc + v }
q.reversed

val ma = q.mask(q.values > 2)
ma.hasNA
ma.dropNA

q.rolling(2, _.minKey)
q.splitAt(2)
q.sortedIx.splitBy("b")
```

We can of course convert to a Vec or a Seq if we need to. The Series.toSeq method yields a sequence of key/value tuples.

```scala mdoc
 q.toVec
 q.toSeq
```

We can also group by key in order to transform or combine the groupings, which themselves are Series. For example:

```scala mdoc
q.groupBy.combine(_.sum)

q.groupBy.transform(s => s - s.mean)
```
You can also group by another index, or by a transformation of the current index, by passing an argument into groupBy. See the Saddle API for more info.

The expressive nature of working with Series becomes apparent when you need to align data:

```scala mdoc
val a = Series(Vec(1,4,2,3), Index("a","b","c","d"))
val b = Series(Vec(5,2,1,8,7), Index("b","c","d","e","f"))

a + b
```
You see that the indexes have been aligned prior to operation being performed. Because there is a missing observation in each label of a, e, and f, the summation is not done and instead an NA value is inserted into the result.

Generally, a full-outer join is performed. So, for instance:
```scala mdoc
val c = Series(Vec(1,4,2), Index("a","b","b"))
val d = Series(Vec(5,2,1), Index("b","b","d"))

c + d
```
Most basic math and boolean operations are supported between two Series, as well as between a Series and a scalar value.

We mentioned joins. Let’s look at a few join operations; the result is a Frame, which we will touch on a bit later. These are similar in nature to SQL joins.

```scala mdoc
 a.join(b, how=index.LeftJoin)

 a.join(b, how=index.RightJoin)

 a.join(b, how=index.InnerJoin)

 a.join(b, how=index.OuterJoin)
``` 
