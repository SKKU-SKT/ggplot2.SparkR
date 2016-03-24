#' Box and whiskers plot.
#'
#' The lower and upper "hinges" correspond to the first and third quartiles
#' (the 25th and 75th percentiles). This differs slightly from the method used
#' by the boxplot function, and may be apparent with small samples.
#'
#' The upper whisker extends from the hinge to the highest value that is within
#' 1.5 * IQR of the hinge, where IQR is the inter-quartile range, or distance
#' between the first and third quartiles. The lower whisker extends from the
#' hinge to the lowest value within 1.5 * IQR of the hinge. Data beyond the
#' end of the whiskers are outliers and plotted as points (as specified by Tukey).
#'
#' In a notched box plot, the notches extend \code{1.58 * IQR / sqrt(n)}.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#' @export
#' @inheritParams geom_bar
#' @param mapping Set of aesthetic mapping created by aes or aes_.
#' @param data The data to be displayed in this layer.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather ehan combining with them.
#' @param stat Use to override the default connection between geom_boxplot and stat_boxplot
#' @param outlier.colour,outlier.shape,outlier.size,outlier.stroke
#'   Default aesthetics for outliers.
#' @param notch if FALSE(default) make a standard box plot. 
#' @param notchwidth for a notched box plot, width of the notch relative to
#'   the body (default 0.5)
#' @param varwidth if FALSE(default) make a standard box plot.
#' @examples
#' \dontrun{
#' ggplot(diamonds, aes(cut, price)) + geom_boxplot()
#'
#' df <- createDataFrame(sqlContext, diamonds)
#' ggplot(df, aes(cut, price)) + geom_bar()
#' ggplot(df, aes(cut, price, fill = color)) + geom_boxplot()
#' }
geom_boxplot <- function(mapping = NULL, data = NULL, stat = "boxplot",
                         position = "dodge", outlier.colour = NULL,
                         outlier.shape = 19, outlier.size = 1.5,
                         outlier.stroke = 0.5, notch = FALSE, notchwidth = 0.5,
                         varwidth = FALSE, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )

  layer2 <- layer_SparkR(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot_SparkR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.colour,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
   
  return(list(layer1, layer2))
}

#' @rdname geom_boxplot
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplot_SparkR <- ggproto("GeomBoxplot_SparkR", GeomBoxplot,
  setup_data = function(data, params) {
    # if `varwidth` not requested or not available, don't use it
    if(is.null(params) || is.null(params$varwidth) ||
      !params$varwidth || length(grep("relvarwidth", columns(data))) == 0) {
      SparkR::mutate(data, xmin = data$x - data$width / 2, xmax = data$x + data$width / 2)
    } else {
      # make `relvarwidth` relative to the size of the largest group
      max_relvarwidth <- collect(select(data, max(data$relvarwidth)))[[1]]
      SparkR::mutate(data, xmin = data$x - (data$relvarwidth * data$width) / (2 * max_relvarwidth),
      		     xmax = data$x + (data$relvarwidth * data$width) / (2 * max_relvarwidth))
    }
  }
)
