---
title: 'Linear algebra'
weight: 5
---

Saddle exposes the most often used linear algebra operations through a binding to BLAS and LAPACK, thereby linear algebra on saddle is fast and hardware efficient. Data is not copied to an off heap memory segment for this to work. 

These operations are available only for Vec[Double] and Mat[Double] instances.
Linear algebra needs the `saddle-linalg` dependency.

The linear algebra API is exposed as a set of extension methods.
See the [scaladoc](/saddle/api/org/saddle/linalg/MatPimp.html) for the list of supported operations.