# Count number of observation in rectangular bins.
#
# @rdname geom_bin2d
#' @export
stat_bin_2d <- function(mapping = NULL, data = NULL, geom = "tile",
		        position = "identity", bins = 30, binwidth = NULL,
			deop = TRUE, na.rm = FALSE,
			show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    stat = StatBin2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )

  layer2 <- layer_SparkR(
    data = data,
    stat = StatBin2d_SparkR,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}

# @rdname geom_bin2d
# @usage NULL
#' @export
stat_bin2d <- stat_bin_2d

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
StatBin2d_SparkR <- ggproto("StatBin2d_SparkR", Stat_SparkR,
  required_aes = c("x", "y"),
  default_aes = aes(fill = ..count..),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
  			   breaks = NULL, origin = NULL, drop = TRUE) {

    origin <- dual_param(origin, list(NULL, NULL))
    binwidth <- dual_param(binwidth, list(NULL, NULL))
    breaks <- dual_param(breaks, list(NULL, NULL))
    bins <- dual_param(bins, list(x = 30, y = 30))

    xbreaks <- bin_breaks(select(data, "x"), scales$x, breaks$x, origin$x, binwidth$x, bins$x)
    ybreaks <- bin_breaks(select(data, "y"), scales$y, breaks$y, origin$y, binwidth$y, bins$y)

    breaks_df_x <- createDataFrame(sqlContext,
      data.frame(xmin = xbreaks[-length(xbreaks)], xmax = xbreaks[-1])
    )

    breaks_df_y <- createDataFrame(sqlContext,
      data.frame(ymin = ybreaks[-length(ybreaks)], ymax = ybreaks[-1])
    )

    data <- SparkR::join(data, breaks_df_x, data$x >= breaks_df_x$xmin & data$x < breaks_df_x$xmax, "inner")
    data <- SparkR::join(data, breaks_df_y, data$y >= breaks_df_y$ymin & data$y < breaks_df_y$ymax, "inner")

    if(length(grep("fill", columns(data)))) {
      data <- SparkR::count(groupBy(data, "PANEL", "group", "xmin", "xmax", "ymin", "ymax", "fill"))
    } else if(length(grep("colour", columns(data)))) {
      data <- SparkR::count(groupBy(data, "PANEL", "group", "xmin", "xmax", "ymin", "ymax", "colour"))
    } else {
      data <- SparkR::count(groupBy(data, "PANEL", "group", "xmin", "xmax", "ymin", "ymax"))
    }
   
    sum_count <- collect(select(data, sum(data$count)))
    width <- diff(xbreaks)[1]
    height <- diff(ybreaks)[1]
    
    data <- SparkR::mutate(data, density = data$count / sum_count[1, ],
                           width = lit(width), height = lit(height))
    data
  }
)

dual_param <- function(x, default = list(x = NULL, y = NULL)) {
  if(is.null(x)) {
    default
  } else if(length(x) == 2) {
    if(is.list(x) && !is.null(names(x))) {
      x
    } else {
      list(x = x[[1]], y = x[[2]])
    }
  } else {
    list(x = x, y = x)
  }
}

bin_breaks <- function(data, scale, breaks = NULL, origin = NULL, binwidth = NULL, 
                       bins = 30, right = TRUE) {
  if(scale$is_discrete()) {
    return(-0.5 + seq_len(nrow(distinct(data)) + 1))
  }

  range <- as.numeric(collect(select(data, min(data[[1]]), max(data[[1]]))))
  
  if(is.null(binwidth) || identical(binwidth, NA)) {
    binwidth <- diff(range[1:2]) / bins
  }
  stopifnot(is.numeric(binwidth), length(binwidth) == 1)
  
  if(is.null(origin) || identical(origin, NA)) {
    origin <- plyr::round_any(range[1], binwidth, floor)
  }
  stopifnot(is.numeric(origin), length(origin) == 1)
  
  if(is.null(breaks)) {
    breaks <- seq(origin, range[2] + binwidth, binwidth)
  } 
}

