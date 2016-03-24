#' @rdname geom_freqpoly
#' @format NULL
#' @usage NULL
#' @export
GeomPath_SparkR <- ggproto("GeomPath_SparkR", GeomPath,
  handle_na = function(data, params) {
    data
  }
)
