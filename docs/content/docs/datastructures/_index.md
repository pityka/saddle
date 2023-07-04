---
title: 'Data structures'
weight: 3
---

Saddle is focused around the following five data structures:
- a 1D array (Vec)
- a 2D array (Mat)
- an index (Index)
- a 1D array with an index (Series)
- a 2D array with an index for each dimensions (Frame)

Vec and Mat are stored as a contiguous array. 
Index is stored as a specialized hash map.
Series is the product type of Index and Vec.
Frame is stored as two Index and a sequence of Vec-s (column-wise).