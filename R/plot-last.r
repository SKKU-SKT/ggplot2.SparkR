.plot_store <- function() {
  .last_plot <- NULL

  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

# Set last plot created or modified
set_last_plot <- function(value) .store$set(value)
