# Box and whiskers plot.
#
#' @export
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

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
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
