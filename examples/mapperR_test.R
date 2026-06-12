library(mappeR)
library(igraph)

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

num_bins <- 5
percent_overlap <- 25

cover <- create_width_balanced_cover(
  min(filtered), max(filtered),
  num_bins, percent_overlap
)

# mappeR mapper objects require a list of membership test functions
# these return true/false for a cover element membership test
check_in_interval <- function(endpoints) {
  return(function(x) (endpoints[1] <= x) & (x <= endpoints[2]))
}
cover_checks <- apply(cover, 1, check_in_interval)

# build the mapper object
# cut_height defaults to 5% above the merge point (5% more distance)
mapper <- create_mapper_object(
  data = data,
  dists = distances,
  lens = filtered,
  cover_element_tests = cover_checks,
  clusterer = global_hierarchical_clusterer("single", distances)
)

# visualise the mapper graph (using igraph)
# mapper returns a list of two data frames: nodes and edges
mapper_g <- graph_from_data_frame(mapper[[2]], directed = FALSE, vertices = mapper[[1]])
V(mapper_g)$filter_vals <- filtered
plot(mapper_g)
