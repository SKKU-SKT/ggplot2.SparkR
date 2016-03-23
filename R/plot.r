#' Create a new ggplot plot.
#'
#' This function is same as ggplot2::ggplot. In ggplot2.SparkR, 
#' we define a new method for Spark DataFrame
#'
#' @export
#' @param data Default dataset to use for plot. If not already a data.frame,
#'   will be converted to one by fortify. If not specified,
#'   must be suppled in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, must be suppled in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment If an variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which ggplot() is called.
ggplot <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggplot")
}

#' @export
#' @rdname ggplot
#' @usage NULL
ggplot.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  ggplot.data.frame(fortify(data, ...), mapping, environment = environment)
}

#' @export
#' @rdname ggplot
#' @usage NULL
ggplot.data.frame <- function(data, mapping = aes(), ...,
                              environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes() or `aes_()`.", call. = FALSE)
  }

  p <- structure(list(
    data = data,
    layers = list(),
    scales = scales_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot"))
  
  p$labels <- make_labels(mapping)
  
  set_last_plot(p)
  p
}

#' @export
#' @rdname ggplot
#' @usage NULL
#' @method ggplot DataFrame
ggplot.DataFrame <- function(data, mapping = aes(), ...,
			     environment = getNamespace("ggplot2.SparkR")) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with aes or aes_string")
  }

  p <- structure(list(
    data = data,
    layers = list(),
    scales = scales_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot_SparkR", "ggplot"))
 
  p$labels <- make_labels(mapping)

  set_last_plot(p)
  p
}

#' @export
#' @method print ggplot_SparkR
print.ggplot_SparkR <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
  if(newpage) grid.newpage()

  grDevices::recordGraphics(
    requireNamespace("ggplot2.SparkR", quietly = TRUE),
    list(),
    getNamespace("ggplot2.SparkR")
  )

  data <- ggplot_build_SparkR(x)
  
  gtable <- ggplot_gtable(data)
  if(is.null(vp)) {
    grid.draw(gtable)
  } else {
    if(is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }

  invisible(data)
}

#' @method plot ggplot_SparkR
#' @export
plot.ggplot_SparkR <- print.ggplot_SparkR

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}
