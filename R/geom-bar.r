#' Bars, rectangles with bases on x-axisi
#'
#' There are two types of bar charts, determined by what is mapped to bar
#' height. By default, geom_bar uses stat_count which makes the
#' height of the bar proportion to the number of cases. 
#'
#' A bar chart maps the height of the bar to a variable, and so the base of the
#' bar must always be shown to produce a valid visual comparison. Naomi Robbins
#' has a nice
#' \href{http://www.b-eye-network.com/view/index.php?cid=2468}{article on this
#' topic}. This is why it doesn't make sense to use a log-scaled y axis with a
#' bar chart.
#'
#' @export
#' @param width Bar width.
#' @param binwidth geom_bar no longer has a binwidth arhument
#' @param stat Override the default connection between geom_bar and stat_count
#' @param na.rm If FALSE, removes missing values with a warning (not used in ggplot2.SparkR).
#' @param ... other arguments passed on to layer.
#' @examples
#' \dontrun{
#' ggplot(faithful, aes(x = eruptions)) + geom_bar()
#'
#' df <- createDataFrame(sqlContext, faithful)
#' ggplot(df, aes(x = eruptions)) + geom_bar()
#'
#' df2 <- createDataFrame(sqlContext, diamonds)
#' ggplot(df2, aes(x = cut, fill = color)) + geom_bar(position = "dodge")
#' ggplot(df2, aes(cut)) + geom_bar() + facet_grid(. ~ clarity)
#' }
geom_bar <- function(mapping = NULL, data = NULL, stat = "count",
                     position = "stack", width = NULL, binwidth = NULL, ...,
	             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
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
      width = width,
      na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}

#' @rdname geom_bar
#' @format NULL
#' @usage NULL
#' @export
GeomBar_SparkR <- ggproto("GeomBar_SparkR", GeomBar,
  setup_data = function(data, params) {
    SparkR::mutate(data, ymin = lit(0), ymax = data$y,
      xmin = data$x - (data$width / 2), xmax = data$x + (data$width / 2))
  }
)
