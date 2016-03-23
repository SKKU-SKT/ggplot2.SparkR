is.discrete_SparkR <- function(data) {
  unlist(lapply(dtypes(data), function(df) {
    if(df[2] == "string" | df[2] == "boolean")  return(df[1])
  }))
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}
