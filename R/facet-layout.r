# Layout panels in a 2d grid.
#
# @params data list of Spark DataFrames, one for each layer
# @params rows variables that form the rows
# @params cols variables that form the columns
# @return a Spark DataFrame with columns \code{PANEL}, \code{ROW} and \code{COL},
#   that match the facetting variable values up with their position in the
#   grid
layout_grid_SparkR <- function(data, rows = NULL, cols = NULL, margins = NULL,
    drop = TRUE, as.table = TRUE) {    
  if(length(rows) == 0 && length(cols) == 0) return(layout_null())
  
  rows_char <- as.character(rows)
  cols_char <- as.character(cols)
  rows_is_null <- length(rows_char) == 0
  cols_is_null <- length(cols_char) == 0

  data <- data[[1]]
  
  # Set unique number for ROW grid
  if(!rows_is_null) {
    rows <- distinct(select(data, eval(rows_char)))
    rows <- bindIDs(rows)
    rows <- withColumnRenamed(rows, paste0(rows_char, "id"), eval(rows_char))
    rows <- withColumnRenamed(rows, "id", "ROW")
  }
  
  # Set unique number for COL grid
  if(!cols_is_null) {
    cols <- distinct(select(data, eval(cols_char)))
    cols <- bindIDs(cols)
    cols <- withColumnRenamed(cols, paste0(cols_char, "id"), eval(cols_char))
    cols <- withColumnRenamed(cols, "id", "COL")
  }
  
  # Create PANEL info dataset
  if(!rows_is_null && !cols_is_null) {
    rows_cols <- SparkR::join(rows, cols)
    rows_cols <- cbind(collect(rows_cols), PANEL = 1:nrow(rows_cols))
    panels <- createDataFrame(sqlContext, rows_cols)
  } else if(!rows_is_null) {
    panels <- SparkR::mutate(rows, PANEL = rows$ROW, COL = lit(1))
  } else if(!cols_is_null) {
    panels <- SparkR::mutate(cols, PANEL = cols$COL, ROW = lit(1))
  }

  panels
}

# Layout out panels in a 1d ribbon.
#
# @params drop should missing combinations be excluded from the plot?
# @keywords internal
layout_wrap_SparkR <- function(data, vars = NULL, nrow = NULL, ncol = NULL,
    as.table = TRUE, drop = TRUE, dir = "h") {
  vars <- as.character(unlist(vars))
  data <- data[[1]]

  panels <- distinct(select(data, eval(vars)), eval(vars))
  panels <- bindIDs(panels)
  panels <- withColumnRenamed(panels, paste0(vars, "id"), vars)
  panels <- withColumnRenamed(panels, "id", "PANEL")

  dims <- ggplot2:::wrap_dims(nrow(panels), nrow, ncol)
  panels <- SparkR::mutate(panels, ROW = floor((panels$PANEL - 1) / dims[2] + 1),
    COL = pmod(panels$PANEL - 1, lit(dims[2])) + 1)

  select(panels, vars, "PANEL", "ROW", "COL")
}
