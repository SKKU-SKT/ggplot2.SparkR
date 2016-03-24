#' Histograms.
#'
#' Display a 1d distribution by dividing into bins and counting the number
#' of observations in each bin. Histograms use bars.
#'
#' @export
#' @param mapping Set of aesthetic mapping created by aes or aes_.
#' @param data The data to be displayed in this layer.
#' @param stat Use override the default connection between geom_histogram, geom_freqpoly and stat_bin
#' @param binwidth ???
#' @param bins Does not use in ggplot2.SparkR
#' @param origin Does not use in ggplot2.SparkR
#' @param right Does not use in ggplot2.SparkR
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param na.rm Not used in ggplot2.SparkR
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather ehan combining with them.
#' @param ... other arguments passed on to layer.
#' @examples
#' \dontrun{
#' ggplot(faithful, aes(eruptions)) + geom_histogram()
#'
#' df <- createDataFrame(sqlContext, faithful)
#' ggplot(df, aes(eruptions)) + geom_histogram()
#' ggplot(df, aes(eruptions)) + geom_histogram(binwidth = 5)
#'
#' ggplot(df, aes(eruptions, fill = waiting)) + geom_histogram()
#' }
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
