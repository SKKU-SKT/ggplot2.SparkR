# Transform values to cardinal representation
scales_transform_df_SparkR <- function(scales, data) {
  for(index in 1:length(scales$scales)) {
    scale_type <- scales$scales[[index]]$trans$name
    done <- scales$scales[[index]]$trans$done
    scale_aes <- scales$scales[[index]]$aesthetics[1]

    for(col_list in columns(data)) {
      if(scale_aes == col_list && !is.null(scale_type) && is.null(done)) {
        scale_aes_old <- paste0(scale_aes, "_OLD")
	if(scale_type == "log-10") {
	  data <- withColumnRenamed(data, scale_aes, scale_aes_old)
	  data <- withColumn(data, scale_aes, log10(data[[scale_aes_old]])) 
	}
	
	scales$scales[[index]]$trans$done <- TRUE
      }
    }
  }

  data
}

# @param aesthetics A list of aesthetic-variable mappings. The name of each
#   item is the aesthetic, and the value of each item is the variable in data.
scales_add_defaults_SparkR <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))
  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()
  
  datacols <- new_aesthetics
  type_arr <- unlist(dtypes(data))
  
  for(aes in datacols) {
    type_col <- type_arr[grep(aes, type_arr) + 1]
    if(type_col == "string" || type_col == "boolean") {
      type <- "discrete"
    } else {
      type <- "continuous"
    }
    
    scale_name <- paste("scale", aes, type, sep = "_")
    scale_f <- find_global(scale_name, env, mode = "function")
    
    if (is.null(scale_f)) next
    
    scales$add(scale_f())
  }
}

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing_SparkR <- function(plot, aesthetics, env) {

  # Keep only aesthetics that aren't already in plot$scales
  aesthetics <- setdiff(aesthetics, plot$scales$input())

  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")
    
    scale_f <- find_global(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }
}

# Train scale from a data frame
scales_train_df <- function(scales, df, drop = FALSE) {
  if (empty(df) || length(scales$scales) == 0) return()

  lapply(scales$scales, function(scale) scale$train_df(df = df))
}

# Map values from a data.frame. Returns data.frame
scales_map_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  mapped <- unlist(lapply(scales$scales, function(scale) scale$map_df(df = df)), recursive = FALSE)

  plyr::quickdf(c(mapped, df[setdiff(names(df), names(mapped))]))
}
