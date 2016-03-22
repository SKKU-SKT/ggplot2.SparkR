# Figure out layout from data from plot and all layers
# for SparkR.
#
# This creates the layout data frame which maps from data values to
# panel coordinates: ROW, COL and PANEL. It also records the panels that
# contribute to each x and y scale.
#
# @param data a list of Spark DataFrames (one for the plot and one for each
#   layer)
facet_train_layout_SparkR <- function(facet, data) {
  UseMethod("facet_train_layout_SparkR")
}

facet_map_layout_SparkR <- function(facet, data, layout) {
  UseMethod("facet_map_layout_SparkR")
}
