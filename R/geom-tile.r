# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
GeomTile_SparkR <- ggproto("GeomTile_SparkR", ggplot2::GeomTile,
  setup_data = function(data, params) {
    data
  }
)
