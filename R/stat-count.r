# @rdname geom_bar
#' @export
stat_count <- function(mapping = NULL, data = NULL, geom = "bar",
		     position = "stack", width = NULL, ...,
		     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = StatCount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      ...
    )
  )

  layer2 <- layer_SparkR(
    data = data,
    mapping = mapping,
    stat = StatCount_SparkR,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      ...
    )
  )

  return(list(layer1, layer2))
}

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
#' @include stat-.r
StatCount_SparkR <- ggproto("StatCount_SparkR", Stat_SparkR,
  required_aes = "x",
  default_aes = aes(y = ..count..),

  setup_params = function(data, params) {
    params
  },

  compute_group = function(self, data, scales, width = NULL) {
    res_x <- collect(select(data, "x"))
    width <- if(!is.null(width)) width else resolution(res_x$x) * 0.9
    
    if(length(grep("fill", columns(data)))) {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "fill", "group"))
    } else if(length(grep("colour", columns(data)))) {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "colour", "group"))
    } else {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "group"))
    }

    SparkR::mutate(data, width = lit(width))
  }
)
