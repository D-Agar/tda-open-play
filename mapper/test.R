# General script testing the mapper functionality
library(rrcov) # Reaven-Miller Diabetes dataset
library(igraph)
library(RColorBrewer)

# source functionality
mapper_files <- list.files(file.path("mapper"), full.names = TRUE)
mapper_files <- mapper_files[!mapper_files %in% c("mapper/test.R", "mapper/test.R.tmp.R")]
for (file in mapper_files) {
  source(file)
}

# data setup
data(diabetes)
raw_data <- diabetes[, 1:5]
raw_scaled <- data.frame(scale(raw_data))

d_matrix <- dist(raw_scaled, method = "euclidean")

filter_values <- density_estimation(d_matrix, epsilon = NULL)$values

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
  k_bins = 6
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

