# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
GeomPath_SparkR <- ggproto("GeomPath_SparkR", ggplot2::GeomPath,
  handle_na = function(data, params) {
    data
  }
)
