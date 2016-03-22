# @rdname geom_histogram
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
