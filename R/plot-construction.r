# Modify a ggplot or theme object by adding on new components.
#
# This operator allows you to add objects to a ggplot or theme object.
#
# If the first object is an object of class \code{ggplot}, you can add
# the following types of objects, and it will return a modified ggplot
# object.
#
# @rdname gg-add
#' @method + gg
#' @export
"+.gg" <- function(e1, e2) {
  if(length(e2) == 2) {
    if(class(e1$data)[1] == "DataFrame") {
      ggplot2:::`+.gg`(e1, e2[[2]])
    } else {
      ggplot2:::`+.gg`(e1, e2[[1]])
    }
  } else {
    ggplot2:::`+.gg`(e1, e2)
  }
}

# @rdname gg-add
#' @export
"%+%" <- `+.gg`
