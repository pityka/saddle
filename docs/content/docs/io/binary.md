---
title: 'Binary'
weight: 4
---

The `saddle-binary` module offers a saddle specific fast serialization format.
Its main use is as a temporary format which can be parsed very efficiently. 
It can serialize Vecs, Mats, Series and Frames.

The entry point of these APIs are the org.saddle.binary.Writer and Reader objects.

The scaladoc documents the serialization format.

An example:

```scala mdoc
import org.saddle._
import org.saddle.order._

val frame = Frame(
  Mat(Vec(1d, 2d), Vec(3d, 4d), Vec(5d, 6d)),
  Index("r1", "r2"),
  Index("c1", "c2", "c3")
)
val binaryFrame = org.saddle.binary.Writer.writeFrameIntoArray(frame)
   
  
```