# Learn the layout of panels within a plot.
#
# This is determined by the facet, which returns a data frame, than
# when joined to the data to be plotted tells us which panel it should
# appear in, where that panel appears in the grid, and what scales it
# uses.
#
# As well as the layout info, this function also adds empty lists in which
# to house the x and y scales.
#
# @param the panel object to train
# @param the facetting specification
# @param data a list of data frames (one for each layer), and one for the plot
# @param plot_data the default data frame
# @return an updated panel object
train_layout_SparkR <- function(panel, facet, data, plot_data) {
  layout <- facet_train_layout_SparkR(facet, c(list(plot_data), data))
  panel$layout <- layout
  panel$shrink <- facet$shrink

  panel
}

# Map data to find out where it belongs in the plot.
#
# Layout map ensures that all layer data has extra copies of data for margins
# and missing facetting variables, and has a PANEL variable that tells that
# so it know what panel it belongs to. This is a change from the previous
# design which added facetting variables directly to the data frame and
# caused problems when they had names of aesthetics (like colour or group).
#
# @param panel a trained panel object
# @param the facetting specification
# @param data list of data frames (one for each layer)
# @param plot_data default plot data frame
map_layout_SparkR <- function(panel, facet, data, plot_data) {
  lapply(data, function(data) {
    if(inherits(data, "waiver")) data <- plot_data
    facet_map_layout_SparkR(facet, data, panel$layout)
  })
}

# Train position scales with data
#
# If panel-specific scales are not already present, will clone from
# the scales provided in the parameter
#
# @param panel the panel object to train
# @param data a list of data frames (one for each layer)
# @param x_scale x scale for the plot
# @param y_scale y scale for the plot
train_position_SparkR <- function(panel, data, x_scale, y_scale) {
  layer_data <- data[[1]]

  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- plyr::rlply(1, x_scale$clone())
  }
  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- plyr::rlply(1, y_scale$clone())
  }

  if(!is.null(x_scale) && length(grep("x", columns(layer_data))) != 0 &&
    is.null(panel$x_scales[[1]]$range$range)) {
    if(panel$x_scales[[1]]$scale_name == "position_d") {
      panel$x_scales[[1]]$range$range <- distinct(select(layer_data, layer_data$x))
    }
  }
  if(!is.null(y_scale) && length(grep("y", columns(layer_data))) != 0 &&
    is.null(panel$y_scales[[1]]$range$range)) {
    if(panel$y_scales[[1]]$scale_name == "position_d") {
      panel$y_scales[[1]]$range$range <- distinct(select(layer_data, layer_data$y))
    }
  }

  panel
}

check_type <- function(layer_data, axis, type) {
  index <- unlist(lapply(dtypes(layer_data), function(df) {
    if(df[1] == axis & df[2] == type) TRUE else FALSE
  }))

  grep(TRUE, index)
}

# Map data with scales.
#
# This operation must be idempotent because it is applied twice: both before
# and after statistical transformation.
#
# @param data a list of Spark DataFrames (one for each layer)
map_position_SparkR <- function(data, x_scale, y_scale) {
  lapply(data, function(layer_data) {
    if(length(check_type(layer_data, "x", "string")) != 0) {
      if(!is.null(x_scale$limits)) {
	layer_data <- SparkR::filter(layer_data, eval(parse(
	  text = paste0('layer_data$x == "', x_scale$limits, '"', collapse = " & "))))
      }
      disc_x <- bindIDs(SparkR::unique(select(layer_data, "x")))
     
      colname <- names(layer_data)[names(layer_data) != "x"]
      layer_data <- SparkR::join(layer_data, disc_x, layer_data$x == disc_x$xid, "inner")
      layer_data <- select(layer_data, append(as.list(colname), "id"))
      layer_data <- withColumnRenamed(layer_data, "id", "x")
    } else {
      layer_data$x <- cast(layer_data$x, "double")
    }

    index <- check_type(layer_data, "y", "string")
    if(length(index) != 0) {
      if(!is.null(y_scale$limits)) {
	layer_data <- SparkR::filter(layer_data, eval(parse(
	  text = paste0('layer_data$y == "', y_scale$limits, '"', collapse = " & "))))
      }
      disc_y <- bindIDs(SparkR::unique(select(layer_data, "y")))
     
      colname <- names(layer_data)[names(layer_data) != "y"]
      layer_data <- SparkR::join(layer_data, disc_y, layer_data$y == disc_y$yid, "inner")
      layer_data <- select(layer_data, append(as.list(colname), "id"))
      layer_data <- withColumnRenamed(layer_data, "id", "y")
    }

    layer_data
  })
}

# Compute ranges and dimensions of each panel, using the coord.
train_ranges_SparkR <- function(panel, data, plot) {
  x_scale_name <- panel$x_scales[[1]]$scale_name
  y_scale_name <- panel$y_scales[[1]]$scale_name
  
  if(x_scale_name == "position_d") {
    panel$x_scales[[1]]$range$range <- collect(SparkR::arrange(panel$x_scales[[1]]$range$range, "x"))[[1]]
  } else {
    if(!is.null(data[[1]]$xmin)) x_min <- min(data[[1]]$xmin) else x_min <- min(data[[1]]$x)
    if(!is.null(data[[1]]$xmax)) x_max <- max(data[[1]]$xmax) else x_max <- max(data[[1]]$x)
    panel$x_scales[[1]]$range$range <- c(x_min, x_max)
  }
  
  if(y_scale_name == "position_d") {
    panel$y_scales[[1]]$range$range <- collect(SparkR::arrange(panel$y_scales[[1]]$range$range, "y"))[[1]]
  } else {
    if(!is.null(data[[1]]$ymin)) y_min <- min(data[[1]]$ymin) else y_min <- min(data[[1]]$y)
    if(!is.null(data[[1]]$ymax)) y_max <- max(data[[1]]$ymax) else y_max <- max(data[[1]]$y)
    panel$y_scales[[1]]$range$range <- c(y_min, y_max)
  }
  
  panel <- train_ranges(panel, plot$coordinates)

  panel
}

reset_scales <- function(panel) {
  if(!panel$shrink) return()
  lapply(panel$x_scales, function(s) s$reset())
  lapply(panel$y_scales, function(s) s$reset())
  invisible()
}

# Compute ranges and dimensions of each panel, using the coord.
train_ranges <- function(panel, coord) {
  compute_range <- function(ix, iy) {
  # TODO: change coord_train method to take individual x and y scales
    coord$train(list(x = panel$x_scales[[ix]], y = panel$y_scales[[iy]]))
  }

  panel$ranges <- Map(compute_range,
    panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}
