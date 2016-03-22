# @rdname geom_boxplot
#' @export
stat_boxplot <- function(mapping = NULL, data = NULL, geom = "boxplot",
                         position = "dodge", coef = 1.5, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
  
  layer2 <- layer_SparkR(
    data = data,
    mapping = mapping,
    stat = StatBoxplot_SparkR,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )

  return(list(layer1, layer2))
}

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
StatBoxplot_SparkR <- ggproto("StatBoxplot_SparkR", Stat_SparkR,
  required_aes = StatBoxplot$required_aes,
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$width <- if(!is.null(params$width)) params@width else 1 * 0.75

    params
  },

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
    qs <- c(0, 0.25, 0.5, 0.75, 1)
    column_fill <- length(grep("fill", columns(data)))

    if(column_fill) {
      distinct_data <- collect(distinct(select(data, "x", "PANEL", "fill", "group")))
    } else {
      distinct_data <- collect(distinct(select(data, "x", "PANEL", "group")))
    }

    for(i in 1:nrow(distinct_data)) {
      if(column_fill) {
        y <- SparkR::filter(data, data$x == distinct_data$x[i] &
			data$PANEL == distinct_data$PANEL[i] &
			data$fill == distinct_data$fill[i] &
			data$group == distinct_data$group[i])
      } else {
        y <- SparkR::filter(data, data$x == distinct_data$x[i] & 
			data$PANEL == distinct_data$PANEL[i] &
			data$group == distinct_data$group[i])
      }
      y <- collect(SparkR::arrange(select(y, "y"), "y"))$y
      stats <- as.numeric(quantile(y, qs))
      names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
      iqr <- diff(stats[c(2, 4)])
      outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)

      if(any(outliers)) {
        stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm = TRUE)
      }
      
      df <- as.data.frame(as.list(stats))
      df$outliers <- I(list(y[outliers]))
      n <- sum(!is.na(y))
      df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
      df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
      df$relvarwidth <- sqrt(n)
      
      distinct <- cbind(distinct_data[i, ], df)
      if(i > 1) box <- rbind(box, distinct) else box <- distinct
    }
    
    outliers <- box[c("x", "outliers")]
    box$outliers <- NULL
    data <- createDataFrame(sqlContext, box)
    data <- SparkR::mutate(data, width = lit(width), weight = lit(1))
    
    list(outliers, data)
  }
)
