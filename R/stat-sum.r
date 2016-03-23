#' Count two dimensional variables and present size of point
#'
#' @inheritParams layer_SparkR
#' @inheritParams geom_bar
#' @export
stat_sum <- function(mapping = NULL, data = NULL, geom = "point",
                     position = "identity", na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = StatSum,
    geom = geom,
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
    stat = StatSum_SparkR,
    geom = geom,
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

#' @rdname stat_sum
#' @format NULL
#' @usage NULL
#' @export
StatSum_SparkR <- ggproto("StatSum_SparkR", Stat_SparkR,
  default_aes = StatSum$default_aes,
  required_aes = StatSum$required_aes,

  compute_group = function(data, scales) {
    counts <- SparkR::count(groupBy(data, "PANEL", "x", "y"))
    counts <- SparkR::rename(counts, n = counts$count)
    counts <- withColumn(counts, "prop", counts$n^(-1))
    counts <- select(counts, "PANEL", "x", "y", "n", "prop")

    counts
  }
)
