---
title: 'Imports'
weight: 2
---

### Imports
You most likely need the following two imports:
```scala
import org.saddle._
import org.saddle.order._
```

Note that `org.saddle.order._` imports `cats.kernel.Order[_]` typeclass instances into the scope. 
If you import cats instances an other way then you should not import `org.saddle.order._`. 

The `Order[Double]` and `Order[Float]` instances in `org.saddle.order` define a total ordering and 
order `NaN` above all other values, consistent with `java.lang.Double.compare`.