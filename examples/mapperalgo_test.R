library(MapperAlgo)
# one-dimensional example
num_points <- 5000

inputs <- seq(0, 2 * pi, length.out = num_points)

f_x <- function(x) sin(x) * 10 + rnorm(num_points, 0, 0.1)
f_y <- function(x) 10 * sin(x) * (cos(x)^2) + rnorm(num_points, 0, 0.1)
f_z <- function(x) 10 * (sin(x)^2) * cos(x) + rnorm(num_points, 0, 0.1)

data <- data.frame(
  X = f_x(inputs),
  Y = f_y(inputs),
  Z = f_z(inputs)
)

distances <- dist(data)

# filter function: projection to x-coordinate
# cover: cover of the data up to the extremes
#   10 equally spaced intervals
#   25% overlap
# clustering: single-linkage hierarchical clustering
filtered <- data$X

num_bins <- 10
percent_overlap <- 25

cover <- create_width_balanced_cover(
  min(filtered), max(filtered),
  num_bins, percent_overlap
)

mapper <- MapperAlgo(
  data,
  filter_values = filtered,
  percent_overlap = percent_overlap,
  methods = "hierarchical",
  method_params = list(method = "single", num_bins_when_clustering = num_bins),
  cover_type = "extension",
  intervals = num_bins,
  interval_width = NULL
)

MapperPlotter(mapper, data, label = filtered, avg = FALSE, use_embedding = FALSE)
