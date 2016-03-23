#' Add heatmap of 2d bin counts.
#'
#' @export
#' @inheritParams layer_SparkR
#' @param stat Use override the default connection between geom_bin2d and stat_bin2d
#' @param na.rm If FALSE, remove missing values with a warning (not used in ggplot2.SparkR)
#' @param ... other arguments passed on to layer.
#' @examples
#' \dontrun{
#' ggplot(diamonds, aes(x, y)) + geom_bin2d()
#'
#' df <- createDataFrame(sqlContext, diamonds)
#' ggplot(df, aes(x, y)) + geom_bin2d()
#' ggplot(df, aes(x, y)) + geom_bin2d(binwidth = c(0.5, 0.5))
#' }
geom_bin2d <- function(mapping = NULL, data = NULL, stat = "bin2d",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTile,
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
    geom = GeomTile_SparkR,
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
