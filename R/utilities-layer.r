bindIDs <- function(data) {
  pair <- dtypes(data)[[1]]
  data <- SparkR::arrange(data, data[[1]])

  rdd1 <- SparkR:::coalesce(SparkR:::map(SparkR:::toRDD(data),
    function(rdd) { toString(rdd) }), 1)
  rdd2 <- SparkR:::coalesce(SparkR:::map(SparkR:::parallelize(sc, 1:nrow(data)),
    function(rdd) { toString(rdd) }), 1)
  rdd <- SparkR:::zipRDD(rdd1, rdd2)
  df <- createDataFrame(sqlContext, rdd)

  df <- withColumnRenamed(df, "_1", paste0(pair[1], "id"))
  df <- withColumnRenamed(df, "_2", "id")
  df$id <- cast(df$id, "integer")

  df
}
