#' Build ggplot for rendering.
#'
#' @usage NULL
#' @export
ggplot_build_SparkR <- function(plot) {
  plot <- plot_clone(plot)
  if(length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  outliers <- NULL
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)

  scales <- plot$scales
  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
 
  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  panel <- structure(list(), class = "panel")
  panel <- train_layout_SparkR(panel, plot$facet, layer_data, plot$data)
  data <- map_layout_SparkR(panel, plot$facet, layer_data, plot$data)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics_SparkR(d, plot))

  # Transform all scales
  data <- lapply(data, scales_transform_df_SparkR, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  panel <- train_position_SparkR(panel, data, scale_x(), scale_y())
  data <- map_position_SparkR(data, scale_x(), scale_y())

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic_SparkR(d, panel))
  if(length(data[[1]]) == 2) {
    outliers <- data[[1]][[1]]
    data[[1]] <- data[[1]][[2]]
  }
  data <- by_layer(function(l, d) l$map_statistic_SparkR(d, plot))

  # Make sure missing (but required) aesthetics are added
  scales_add_missing_SparkR(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  # Re-train and map.  This ensures that facets have control
  # over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  reset_scales(panel)
  panel <- train_position_SparkR(panel, data, scale_x(), scale_y())
  data <- map_position_SparkR(data)
  data <- lapply(data, function(data) collect(SparkR::arrange(data, "group", "x")))

  # (TODO) Need to delete this if function
  if(!is.null(outliers)) {
    outliers <- plyr::arrange(outliers, x)
    data[[1]] <- cbind(data[[1]], outliers = outliers$outliers)
  }

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Apply position to adjustments
  panel$layout <- collect(panel$layout)
  data <- by_layer(function(l, d) l$compute_position(d, panel))

  # Train coordinate system
  panel <- train_ranges_SparkR(panel, data, plot)

  data <- by_layer(function(l, d) l$compute_geom_2(d))
  levels(data[[1]]$PANEL) <- as.character(panel$layout$PANEL)
  data[[1]]$PANEL <- as.factor(data[[1]]$PANEL)

  list(data = data, panel = panel, plot = plot)
}

