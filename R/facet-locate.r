# Take single layer of data and combine it with panel information to split
# data into different panels. Adds in extra data for missing facetting
# levels and for margins.
#
# @params data a data frame
locate_grid_SparkR <- function(data, panels, rows = NULL, cols = NULL, margins = FALSE) {
  rows_char <- as.character(rows)
  cols_char <- as.character(cols)
  rows_is_null <- length(rows_char) == 0
  cols_is_null <- length(cols_char) == 0
  
  colnames <- names(data)
  
  # Add PANEL variable 
  if(!rows_is_null && !cols_is_null) {
    panels <- SparkR::rename(panels, row_old = panels[[rows_char]], col_old = panels[[cols_char]])
    data <- SparkR::join(panels, data, 
      panels$row_old == data[[rows_char]] &
      panels$col_old == data[[cols_char]], "inner")
  } else if(!rows_is_null) {
    panels <- withColumnRenamed(panels, eval(rows_char), "row_old")
    data <- SparkR::join(panels, data, panels$row_old == data[[rows_char]], "inner")
  } else if(!cols_is_null) {
    panels <- withColumnRenamed(panels, eval(cols_char), "col_old")
    data <- SparkR::join(panels, data, panels$col_old == data[[cols_char]], "inner")
  }
  
  # Return with unnessary columns (col_old, row_old, COL, ROW
  select(data, append(as.list(colnames), "PANEL"))
}

locate_wrap_SparkR <- function(data, panels, vars) {
  vars <- as.character(unlist(vars))
  colname <- names(data)

  panels <- withColumnRenamed(panels, vars, "id")
  data <- SparkR::join(data, panels, data[[vars]] == panels$id, "inner")

  select(data, append(as.list(colname), "PANEL"))
}
