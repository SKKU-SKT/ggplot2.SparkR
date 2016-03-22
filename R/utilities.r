is.discrete_SparkR <- function(data) {
  unlist(lapply(dtypes(data), function(df) {
    if(df[2] == "string" | df[2] == "boolean")  return(df[1])
  }))
}
