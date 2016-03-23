# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding \code{label}. The special value \code{NO_GROUP}
# is used for all observations if no discrete variables exist.
add_group_SparkR <- function(data) {
  sqlContext <- get("sqlContext", envir = globalenv())
  if(is.null(data)) return(data)
  
  cols <- names(data)
  if(!any(cols == "group")) {
    disc <- is.discrete_SparkR(data)
    disc <- disc[disc != "PANEL"]

    if(any(as.logical(length(disc)))) {
      group <- SparkR::unique(select(data, as.list(disc)))

      if(as.logical(length(disc[disc == "fill"]))) group <- SparkR::arrange(group, "fill")
      if(as.logical(length(disc[disc == "y"]))) group <- SparkR::arrange(group, "y")
      if(as.logical(length(disc[disc == "x"]))) group <- SparkR::arrange(group, "x")

      group <- collect(group)
      names(group) <- paste0(names(group), "id")
      group <- createDataFrame(sqlContext, cbind(group, group = 1:nrow(group)))

      cols2 <- names(group)
      cmd <- paste0("data$", disc[disc != "PANEL"], " == group$",
        cols2[cols2 != "group"], collapse = " & ")

      data <- SparkR::join(data, group, eval(parse(text = cmd)), "inner")
      data <- select(data, append(as.list(cols), "group"))
    } else {
      data <- withColumn(data, "group", lit(-1))
    }
  } else {
    cols <- cols[cols != "group"]
    group <- SparkR::unique(select(data, "group"))
    group <- bindIDs(group)
    
    data <- SparkR::join(data, group, data$group == group$groupid, "inner")
    data <- select(data, append(as.list(cols), "id"))
    data <- withColumnRenamed(data, "id", "group")
  }

  data
}
