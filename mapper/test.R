# General script testing the mapper functionality
# This script creates a Mapper graph for the Reaven-Miller diabetes dataset (~1973)
# It is taken from Singh et al. 2007 (the Mapper introductory paper)
library(rrcov) # Reaven-Miller Diabetes dataset
library(igraph)
library(RColorBrewer)
library(ks)
source("utils.R")

# source functionality
sourceDir("mapper", exclude = "test.R")

# data setup
data(diabetes)
raw_data <- diabetes[, 1:5]
# center = FALSE to keep spatial information
raw_scaled <- data.frame(scale(raw_data[, 1:5], center = FALSE))

d_matrix <- dist(raw_scaled, method = "euclidean")

density_est <- density_estimation(d_matrix, epsilon = NULL)
filter_values <- density_est$values

num_intervals <- 4
percent_overlap <- 50

intervals <- create_fixed_intervals(
  filter_values,
  num_intervals,
  percent_overlap
)

mapper <- compute_mapper(
  distances = d_matrix,
  filter_values = filter_values,
  intervals = intervals,
  pruning = FALSE,
  clustering = cluster_gap_heuristic,
  k_bins = 5
)

graph <- create_mapper_graph(
  mapper,
  colourisation = filter_values,
  diabetes,
  groups = list(
    cat_var = "group",
    cat_colour = brewer.pal(length(unique(diabetes$group)), "Set1")
  )
)
plot_mapper_graph(
  graph,
  graph_layout = layout_with_kk,
  colourisation = filter_values,
  legend = TRUE,
  legend_title = "KDE"
)
