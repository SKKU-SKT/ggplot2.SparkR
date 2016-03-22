# Wrap a 1d ribbon of panels into 2d.

#' @export
facet_train_layout_SparkR.wrap <- function(facet, data) {
  panels <- layout_wrap_SparkR(data, facet$facets, facet$nrow, facet$ncol,
    facet$as.table, facet$drop, facet$dir)

  n <- nrow(panels)
  nrow <- collect(select(panels, max(panels$ROW)))
  nrow <- nrow[, 1]

  panels <- SparkR::mutate(panels, SCALE_X = lit(1), SCALE_Y = lit(1), MAX = lit(nrow))
  panels <- SparkR::mutate(panels, AXIS_X = (panels$ROW == panels$MAX),
    AXIS_Y = (panels$COL == 1)) 

  panels
}

#' @export
facet_map_layout_SparkR.wrap <- function(facet, data, layout) {
  locate_wrap_SparkR(data, layout, facet$facets)
}
