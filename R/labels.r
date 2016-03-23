# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  remove_dots <- function(x) {
    gsub(match_calculated_aes, "\\1", x)
  }

  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess"))
    if (is.character(mapping)) {
      aesthetic
    } else {
      remove_dots(deparse(mapping))
    }
  }
  Map(default_label, names(mapping), mapping)
}
