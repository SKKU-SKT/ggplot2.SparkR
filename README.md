# ggplot2.SparkR

ggplot2.SparkR is an R package for scalable visualization of big data represented 
in Spark DataFrame. It is an extension to the original ggplot2 package and can seamlessly 
handle both R data.frame and Spark DataFrame with no modifications to the original API.

ggplot2.SparkR requires no additional training for existing R users who are already 
familiar with ggplot2 and allows them to benefit from powerful distributed processing 
capabilities of Spark for efficient visualization of big data. 

Until now, 5 graph types (bar, bin2d, boxplot, histogram, stat-sum graphs) and 8 options 
(facet-grid, facet-wrap, coord-flip, scale-x-log10, scale-y-log10, position-fill, 
position-stack, position-dodge) are supported. We plans to further extend it in the future.

Find out more at <http://skku-skt.github.io/ggplot2.SparkR/>

## Installation

Get the development version from github: 

```R
# install.packages("devtools")
devtools::install_github("SKKU-SKT/ggplot2.SparkR")
```

## Mailing list

Your are welcome to ask ggplot2.SparkR questions or bugs on [ggplot2.SparkR](https://groups.google.com/forum/?fromgroups#!forum/ggplot2-sparkr)
or send an email to ggplot2-sparkr@googlegroups.com.
Anyone can read the archived discussion that you post messages.

## Other Resources
* [ggplot2](http://ggplot2.org): Plotting system for R by Hadley Wickham
* [Apache Spark](http://spark.apache.org): Large-scale data processing engine.
