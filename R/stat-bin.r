#' \code{stat_bin} is suitable only for continuous x data. If your x data is
#'   discrete, you probably want to use \code{\link{stat_count}}.
#'
#' @usage NULL
#' @rdname geom_histogram
#' @export
stat_bin <- function(mapping = NULL, data = NULL, geom = "bar",
                     position = "stack", width = 0.9, drop = FALSE,
                     right = FALSE, binwidth = NULL, bins = NULL, origin = NULL,
                     breaks = NULL, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, ...) {
  layer1 <- layer(
    data = data, mapping = mapping, stat = StatBin,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width, drop = drop, right = right,
      bins = bins, binwidth = binwidth, origin = origin,
      breaks = breaks, na.rm = na.rm,
      ...
    )
  )
  
  layer2 <- layer_SparkR(
    data = data, mapping = mapping, stat = StatBin_SparkR,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width, drop = drop, right = right,
      bins = bins, binwidth = binwidth, origin = origin,
      breaks = breaks, na.rm = na.rm,
      ...
    )
  )

  return(list(layer1, layer2))
}

#' @rdname geom_histogram
#' @format NULL
#' @usage NULL
#' @export
StatBin_SparkR <- ggproto("StatBin_SparkR", Stat_SparkR,
  required_aes = c("x"),
  default_aes = aes(y = ..count..),
  
  setup_params = function(data, params) {
    if(length(check_type(data, "x", "integer")) != 0) {
      stop('StatBin requires a continuous x variable the x variable is discrete. Perhaps you want stat="count"?',
        call. = FALSE)
    }
    
    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
    }
    
    params
  },
  
  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           origin = NULL, breaks = NULL, width = 0.9, drop = FALSE,
                           right = FALSE) {
    
    bin(data, binwidth = binwidth, bins = bins,
        origin = origin, breaks = breaks, width = width,
        drop = drop, right = right)
  }
)

bin <- function(data, binwidth=NULL, bins=NULL, origin=NULL, breaks=NULL,
                width=0.9, drop = FALSE, right = TRUE) {
  sqlContext <- get("sqlContext", envir = globalenv())
  if(length(check_type(data, "x", "int")) != 0) {
    remained <- NULL
    if(length(grep("fill", names(data)))) {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "fill", "group"))
    } else if(length(grep("colour", names(data)))) {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "colour", "group"))
    } else {
      data <- SparkR::count(groupBy(data, "x", "PANEL", "group"))
    }
  } else if(length(check_type(data, "x", "double")) != 0) {
    range <- as.numeric(collect(select(data, min(data$x), max(data$x))))

    if(is.null(binwidth)) binwidth <- diff(range) / 30
    if(is.null(breaks)) {
      if(is.null(origin)) {
        breaks <- fullseq(range, binwidth, pad = TRUE)
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }
    
    diddle <- 1e-07 * stats::median(diff(breaks))
    if(right) {
      fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
    } else {
      fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
    }
    
    fuzzybreaks <- sort(breaks) + fuzz
    width <- diff(breaks)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    zero_filter <- c()
    
    for(index in 1:length(left)) {
      filter_df <- filter(data, data$x >= left[index] & data$x < right[index])
      if(nrow(filter_df) == 0) {
        zero_filter <- append(zero_filter, index)
      }
      
      filter_df <- SparkR::rename(filter_df, x_bin = filter_df$x)
      filter_df <- withColumn(filter_df, "x", lit((left[index] + right[index]) / 2))	
      
      if(index == 1) unioned <- filter_df
      else unioned <- SparkR::rbind(unioned, filter_df)
    }
    
    if(length(grep("fill", columns(data)))) {
      data <- SparkR::count(groupBy(unioned, "group", "PANEL", "fill", "x"))
    } else if(length(grep("colour", columns(data)))){
      data <- SparkR::count(groupBy(unioned, "group", "PANEL", "colour", "x"))
    } else {
      data <- SparkR::count(groupBy(unioned, "group", "PANEL", "x"))
    }
   
    remained <- data.frame(group = -1, PANEL = 1,
    			   x = (left[zero_filter] + right[zero_filter]) / 2,
                           count = 0, density = 0, ncount = 0, width = width[1],
                           ndensity = 0)
    remained <- createDataFrame(sqlContext, remained)
  }
  
  max_sum_count <- collect(select(data, data$count))
  max_count <- max(max_sum_count$count)
  sum_count <- sum(max_sum_count$count)
    
  data <- SparkR::mutate(data, density = data$count / width[1] / sum_count,
                         ncount = data$count / max_count,
                         width = lit(width[1]))
    
  max_density <- collect(select(data, data$density))
  max_density <- max(max_density$density)
    
  data <- withColumn(data, "ndensity", data$density / max_density)
 
  if(!is.null(remained))  data <- SparkR::rbind(data, remained)
  
  data <- SparkR::arrange(data, "x")
  if (drop) data <- filter(data, data$count > 0)
  
  data
}
