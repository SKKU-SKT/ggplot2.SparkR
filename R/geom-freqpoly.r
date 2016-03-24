#' Freqpoly polygons
#'
#' Uses the same aesthetics as geom_line
#'
#' @param mapping Set of aesthetic mapping created by aes or aes_.
#' @param data The data to be displayed in this layer.
#' @param stat Use override the default connection between geom_histogram, geom_freqpoly and stat_bin
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param na.rm Not used in ggplot2.SparkR
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather ehan combining with them.
#' @param ... other arguments passed on to layer.
#' @export
geom_freqpoly <- function(mapping = NULL, data = NULL, stat = "bin",
			  position = "identity", na.rm = FALSE,
			  show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

  layer2 <- layer_SparkR(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath_SparkR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}
