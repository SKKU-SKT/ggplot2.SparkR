# Bars, rectangles with bases on x-axis
#
#' @export
geom_bar <- function(mapping = NULL, data = NULL, stat = "count",
                     position = "stack", width = NULL, binwidth = NULL, ...,
	             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      ...
    )
  )

  layer2 <- layer_SparkR(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBar_SparkR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
GeomBar_SparkR <- ggproto("GeomBar_SparkR", GeomBar,
  setup_data = function(data, params) {
    SparkR::mutate(data, ymin = lit(0), ymax = data$y,
      xmin = data$x - (data$width / 2), xmax = data$x + (data$width / 2))
  }
)
