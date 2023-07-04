---
title: 'CSV'
weight: 1
---


## CSV Parser 

Saddle features a very fast CSV parser. 
It thrives to allocate as little as possible and make as few branching as possible during parsing. E.g. it can parse numeric tables without ever allocating a String (except for the header).

The CSV parsing logic itself is published in the `saddle-io` module which is a dependency free module. The saddle specific parts are in the `saddle-core` module.

```scala mdoc
import scala.io.Source
import org.saddle._
val irisURL = "https://gist.githubusercontent.com/pityka/d05bb892541d71c2a06a0efb6933b323/raw/639388c2cbc2120a14dcf466e85730eb8be498bb/iris.csv"
val iris : Frame[Int,String,Double] = csv.CsvParser.parseInputStreamWithHeader[Double](
      inputStream = new java.net.URL(irisURL).openStream, 
      cols = List(0,1,2,3), 
      recordSeparator = "\n").toOption.get
```

### Limitations

- recordSeparator must be a String of length one or two.
- fieldSeparator must be a single Char
- doubled quoted quotes are not turned back into a single quote. The csv RFC states that quoted quotes (" hi "quote" ") must be doubled (" hi ""quote"" "). These are returned as doubled.
- The csv parser is tuned for fast parsing of trusted (conforming) input. Parsing arbitrarily broken CSV files is not in scope.

## CSV Writer

There is a simple csv writer provided in the `org.saddle.csv.CsvWriter` object. 

