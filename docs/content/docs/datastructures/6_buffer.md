---
title: 'Mutable buffer'
weight: 6
---

Buffer is a mutable data structure with two operations:
 1. append an element 
 2. convert the buffer to an array or arrays. 
 
- Internally it is stored as a growable sequence of arrays. It never recopies data internally during an append, but it may allocate during append.
- It is specialized for primitive types.
- It can store more elements than Int.MaxValue, however in this case toArray will throw (use toArrays).

```scala mdoc
    val b = org.saddle.Buffer.empty[Int]
    b += 3
    b.toArray
```  