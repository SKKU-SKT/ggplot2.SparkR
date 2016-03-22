# Facet specification: a single panel.
#

#' @export
facet_train_layout_SparkR.null <- function(facet, data) {
  createDataFrame(
    sqlContext,
    data.frame(PANEL = 1, ROW = 1, COL = 1,
      SCALE_X = 1, SCALE_Y = 1))
}

#' @export
facet_map_layout_SparkR.null <- function(facet, data, layout) {
  withColumn(data, "PANEL", lit(1))
}
