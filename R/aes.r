.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
  "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
  "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

# Look up the scale that should be used for given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

  var
}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]

  plyr::rename(x, .base_to_ggplot, warn_missing = FALSE)
}
