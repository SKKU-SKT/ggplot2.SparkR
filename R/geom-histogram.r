# Histograms and frequency polygons.
#
# Display a 1d distribution by dividing into bins and counting the number
# of observations in each bin. Histograms use bars; frequency polygons use
# lines.
#
#' @export
geom_histogram <- function(mapping = NULL, data = NULL, stat = "bin",
                          binwidth = NULL, bins = NULL, origin = NULL,
                          right = FALSE, position = "stack", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      origin = origin,
      right = right,
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
      binwidth = binwidth,
      bins = bins,
      origin = origin,
      right = right,
      na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}
