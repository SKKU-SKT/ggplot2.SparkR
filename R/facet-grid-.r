# Layout panels in a grid

#' @export
facet_train_layout_SparkR.grid <- function(facet, data) {
  layout <- layout_grid_SparkR(data, facet$rows, facet$cols, facet$margins,
    drop = facet$drop, as.table = facet$as.table)

  # Relax constraints, if necessary
  layout <- SparkR::mutate(layout, SCALE_X = lit(1), SCALE_Y = lit(1))

  layout
}

#' @export
facet_map_layout_SparkR.grid <- function(facet, data, layout) {
  locate_grid_SparkR(data, layout, facet$rows, facet$cols, facet$margins)
}
