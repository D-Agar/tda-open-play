get_dimension_intervals <- function(values, num_intervals, overlap_frac) {
  interval_min <- min(values)
  interval_max <- max(values)

  # interval length given the percentage overlap
  # L = range / (num_intervals - ((num_intervals - 1) * overlap))
  interval_length <- (interval_max - interval_min) /
    (num_intervals - ((num_intervals - 1) * overlap_frac))
  step_size <- interval_length * (1 - overlap_frac)

  # dimension's intervals
  intervals <- list()
  for (i in seq_len(num_intervals)) {
    left <- interval_min + ((i - 1) * step_size)
    right <- left + interval_length
    intervals[[i]] <- c(left, right)
  }
  return(intervals)
}

create_interval_grid <- function(intervals, method = "rectangle") {
  # build the grid of interval combinations
  grid_indices <- expand.grid(lapply(intervals, seq_along))
  num_dims <- length(intervals)

  # map the grid index combinations back to actual numeric ranges
  hyperboxes <- lapply(seq_len(nrow(grid_indices)), function(row_idx) {
    box_coordinate_bounds <- lapply(seq_len(num_dims), function(d) {
      interval_idx <- grid_indices[row_idx, d]
      return(intervals[[d]][[interval_idx]])
    })
    # returns a list of length 'num_dims' containing c(min, max)
    return(box_coordinate_bounds)
  })

  return(hyperboxes)
}

create_fixed_intervals <- function(filter_values, num_intervals, percent_overlap, method = "rectangle") {
  # convert to matrices
  filter_values <- as.matrix(filter_values)
  n_dims <- ncol(filter_values)

  # parameter dimension matching
  if (length(num_intervals) == 1) num_intervals <- rep(num_intervals, n_dims)
  if (length(percent_overlap) == 1) percent_overlap <- rep(percent_overlap, n_dims)

  intervals <- lapply(seq_len(n_dims), function(dim) {
    get_dimension_intervals(
      filter_values[, dim],
      num_intervals[dim],
      percent_overlap[dim] / 100
    )
  })

  interval_grid <- create_interval_grid(intervals = intervals, method = method)
  return(interval_grid)
}

create_equalised_intervals <- function(filter_values, num_intervals, percent_overlap, method = "rectangle") {
  filter_values <- as.matrix(filter_values)
  n_dims <- ncol(filter_values)

  # parameter dimension matching
  if (length(num_intervals) == 1) num_intervals <- rep(num_intervals, n_dims)
  if (length(percent_overlap) == 1) percent_overlap <- rep(percent_overlap, n_dims)

  # Loop through each dimension independently to find marginal percentiles
  intervals <- lapply(seq_len(n_dims), function(dim) {
    m <- num_intervals[dim]
    overlap_frac <- percent_overlap[dim] / 100

    # Get interval length and step size as percentiles for this specific dimension
    interval_length <- 1 / ((m - 1) * (1 - overlap_frac) + 1)
    step_size <- interval_length * (1 - overlap_frac)

    dim_intervals <- list()
    for (i in seq_len(m)) {
      # Percentile start/end for interval i
      start <- (i - 1) * step_size
      end <- start + interval_length

      # Map percentiles strictly back to data points within column 'dim'
      dim_intervals[[i]] <- quantile(filter_values[, dim], probs = c(start, end), na.rm = TRUE)
    }
    return(dim_intervals)
  })

  # Construct the final hyperbox grid combinations
  interval_grid <- create_interval_grid(intervals = intervals, method = method)
  return(interval_grid)
}
# check if a coordinate lies in a hyperbox
is_point_in_interval <- function(filter_values, interval) {
  in_interval <- sapply(seq_along(interval), function(dim) {
    dim_bounds <- interval[[dim]]
    return(filter_values[dim] >= dim_bounds[1] & filter_values[dim] <= dim_bounds[2])
  })
  return(all(in_interval))
}

# identify all rows of data that lie in the interval
get_points_in_interval <- function(filter_values, interval) {
  in_interval <- apply(filter_values, 1, is_point_in_interval, interval = interval)
  interval_points <- which(in_interval)
  return(unname(interval_points))
}

# cluster interval into graph nodes
cluster_region <- function(points, d_matrix, clustering, ...) {
  # edge case: singleton forms its own node
  if (length(points) == 1) {
    return(list(points))
  }

  # normal case: cluster data
  sub_d_matrix <- d_matrix[points, points, drop = FALSE]
  interval_clusters <- clustering(distances = sub_d_matrix, ...)
  num_clusters <- max(interval_clusters)

  # cluster assignments -> data rows
  clusters <- lapply(seq_len(num_clusters), function(cluster) {
    return(points[interval_clusters == cluster])
  })
  return(clusters)
}

# prune mapper graph based on a minimum node size
prune_mapper_nodes <- function(nodes, interval_idx, min_node_size) {
  if (is.null(min_node_size) || min_node_size < 1) {
    stop("min_node_size must be a postive integer for pruning")
  }

  valid_nodes <- sapply(nodes, length) >= min_node_size
  return(list(
    nodes = nodes[valid_nodes],
    interval_ids = interval_idx[valid_nodes]
  ))
}

# create adjacency matrix of the mapper graph
compute_graph_edges <- function(nodes) {
  num_nodes <- length(nodes)
  adj_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)

  # one node or none
  if (num_nodes <= 1) {
    return(adj_matrix)
  }

  # intersections tracked
  for (u in seq_len(num_nodes - 1)) {
    for (v in seq(u + 1, num_nodes)) {
      if (length(intersect(nodes[[u]], nodes[[v]])) > 0) {
        adj_matrix[u, v] <- 1
        adj_matrix[v, u] <- 1
      }
    }
  }

  return(adj_matrix)
}

compute_mapper <- function(
  distances, filter_values, intervals, pruning = FALSE, min_node_size = 3, clustering, ...
) {
  dist_matrix <- as.matrix(distances)
  filter_values <- as.matrix(filter_values)

  # find points in intervals and cluster them
  nodes <- list()
  interval_ids <- c()
  for (i in seq_along(intervals)) {
    interval_points <- get_points_in_interval(filter_values, intervals[[i]])

    if (length(interval_points) == 0) next # skip empty hyperbox
    interval_nodes <- cluster_region(interval_points, dist_matrix, clustering, ...)

    # append clustering to the main tracker
    nodes <- c(nodes, interval_nodes)
    interval_ids <- c(interval_ids, rep(i, length(interval_nodes)))
  }

  # pruning if requested
  if (pruning) {
    pruned_data <- prune_mapper_nodes(nodes, interval_ids, min_node_size)
    nodes <- pruned_data$nodes
    interval_ids <- pruned_data$interval_ids
  }

  # compute the edges of the mapper construction
  adj_matrix <- compute_graph_edges(nodes)

  return(list(
    nodes = nodes,
    adjacency = adj_matrix,
    interval_ids = interval_ids
  ))
}
