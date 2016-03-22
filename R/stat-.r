# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
#' @export
Stat_SparkR <- ggproto("Stat_SparkR", Stat,
  retransform = TRUE,

  compute_layer = function(self, data, params, panels) {
    missing_aes <- setdiff(self$stat$required_aes, c(names(data), names(params)))
    if(length(missing_aes) != 0) {
      stop("requires the following missing aesthetics: ",
          paste(missing_aes), collapse = ", ", call. = FALSE)
    }

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]
    scales <- list(
      x = panels$x_scales[[1]],
      y = panels$y_scales[[1]]
    )

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    do.call(self$compute_group, args)
  }
)
