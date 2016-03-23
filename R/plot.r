#' @export
#' @method ggplot DataFrame
ggplot.DataFrame <- function(data, mapping = aes(), ...,
			     environment = getNamespace("ggplot2.SparkR")) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with aes or aes_string")
  }

  p <- structure(list(
    data = data,
    layers = list(),
    scales = ggplot2:::scales_list(),
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
