# Create a new layer
#
#' @export
layer_SparkR <- function(geom = NULL, stat = NULL,
			 data = NULL, mapping = NULL,
			 position = NULL, params = list(),
			 inherit.aes = TRUE, subset = NULL, show.legend = NA) {
  # (TODO) Need to delete pre-processing part from here {
  if (is.null(geom))
    stop("Attempted to create layer with no geom.", call. = FALSE)
  if (is.null(stat))
    stop("Attempted to create layer with no stat.", call. = FALSE)
  if (is.null(position))
    stop("Attempted to create layer with no position.", call. = FALSE)

  # Handle show_guide/show.legend
  if (!is.null(params$show_guide)) {
    warning("`show_guide` has been deprecated. Please use `show.legend` instead.",
      call. = FALSE)
    show.legend <- params$show_guide
    params$show_guide <- NULL
  }
  if (!is.logical(show.legend) || length(show.legend) != 1) {
    warning("`show.legend` must be a logical vector of length 1.", call. = FALSE)
    show.legend <- FALSE
  }
  
  data <- fortify(data)
  if (!is.null(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping must be created by `aes()` or `aes_()`", call. = FALSE)
  }
 
  if (is.character(geom))
    geom <- find_subclass("Geom", geom)
  if (is.character(stat))
    stat <- find_subclass_SparkR("Stat", stat)
  if (is.character(position))
    position <- find_subclass("Position", position)
   
  # Split up params between aesthetics, geom, and stat
  params <- rename_aes(params)

  aes_params  <- params[intersect(names(params), geom$aesthetics())]
  geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
  stat_params <- params[intersect(names(params), stat$parameters(TRUE))]

  all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics())
  extra <- setdiff(names(params), all)
  if (length(extra) > 0) {
    stop("Unknown parameters: ", paste(extra, collapse = ", "), call. = FALSE)
  }
  # } to here

  ggproto("LayerInstance", Layer_SparkR,
    geom = geom,
    geom_params = geom_params,
    stat = stat,
    stat_params = stat_params,
    data = data,
    mapping = mapping,
    aes_params = aes_params,
    subset = subset,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

Layer_SparkR <- ggproto("Layer", ggplot2:::Layer,
  geom = NULL,
  geom_params = NULL,
  stat = NULL,
  stat_params = NULL,
  data = NULL,
  aes_params = NULL,
  mapping = NULL,
  position = NULL,
  inherit.aes = FALSE,

  compute_aesthetics_SparkR = function(self, data, plot) {
    if(self$inherit.aes) {
      aesthetics <- defaults(self$mapping, plot$mapping)
    } else {
      aesthetics <- self$mapping
    }

    values <- as.character(unlist(aesthetics))
    keys <- names(aesthetics)
    data <- select(data, append(as.list(values), "PANEL"))

    for(index in 1:length(keys)) {
      data <- withColumnRenamed(data, values[index], keys[index])
    }

    if(!is.null(self$geom_params$group)) {
      aesthetics["group"] <- self$aes_params$group
    }
    scales_add_defaults_SparkR(plot$scales, data, aesthetics, plot$plot_env)

    data <- add_group_SparkR(data)
    data
  },

  compute_statistic_SparkR = function(self, data, panel) {
    empty <- function(data) {
      is.null(data) || nrow(data) == 0 || ncol(data) == 0
    }
    if(empty(data)) return(NULL)

    params <- self$stat$setup_params(data, self$stat_params)
    data <- self$stat$setup_data(data, params)
    self$stat$compute_layer(data, params, panel)
  },

  map_statistic_SparkR = function(self, data, plot) {
    aesthetics <- self$mapping
    if(self$inherit.aes) {
      aesthetics <- defaults(aesthetics, plot$mapping)
    }
    aesthetics <- defaults(aesthetics, self$stat$default_aes)
    aesthetics <- compact(aesthetics)

    new <- ggplot2:::strip_dots(aesthetics[ggplot2:::is_calculated_aes(aesthetics)])
    if(length(new) == 0) return(data)

    data <- withColumn(data, names(new), data[[as.character(new)]])
    scales_add_defaults_SparkR(plot$scales, data, new, plot$plot_env)

    if(self$stat$retransform) {
      data <- scales_transform_df_SparkR(plot$scales, data)
    }
    data
  },

  compute_geom_1 = function(self, data) {
    data <- self$geom$setup_data(data, c(self$geom_params, self$aes_params))

    data
  },

  compute_position = function(self, data, panel) {
    params <- self$position$setup_params(data)
    data <- self$position$setup_data(data, params)

    self$position$compute_layer(data, params, panel)
  },

  compute_geom_2 = function(self, data) {
    self$geom$use_defaults(data, self$aes_params)
  },

  draw_geom = function(self, data, panel, coord) {
    data <- self$geom$handle_na(data, self$geom_params)
    self$geom$draw_layer(data, self$geom_params, panel, coord)
  }
)

find_subclass_SparkR <- function(super, class) {
  name <- paste0(super, camelize(class, first = TRUE), "_SparkR")

  obj <- get(name)
  if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }
  obj
}

find_subclass <- function(super, class) {
  name <- paste0(super, camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }

  obj
}
