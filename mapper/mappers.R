create_fixed_intervals <- function(filter_values, num_intervals, percent_overlap) {
  # interval endpoints
  f_min <- min(filter_values)
  f_max <- max(filter_values)

  # interval length given the percentage overlap
  # L = range / (num_intervals - (num_intervals - 1) * overlap)
  overlap_frac <- percent_overlap / 100
  interval_length <- (f_max - f_min) / (num_intervals - (num_intervals - 1) * overlap_frac)
  step_size <- interval_length * (1 - overlap_frac)

  intervals <- list()
  for (i in 1:num_intervals) {
    left <- f_min + (i - 1) * step_size
    right <- left + interval_length
    intervals[[i]] <- c(left, right)
  }
  return(intervals)
}

compute_mapper_1d <- function(
    distances, filter_values, intervals, pruning = FALSE, min_node_size = 3, clustering, ...
  ) {
  # find points in intervals and cluster them
  nodes <- list()
  node_interval_id <- c()
  for (i in seq_along(intervals)) {
    # pullback
    points_in_interval <- which(
      filter_values >= intervals[[i]][1] & filter_values <= intervals[[i]][2]
    )
    # remove the names (confusing output)
    points_in_interval <- unname(points_in_interval)

    if (length(points_in_interval) > 0) {
      if (length(points_in_interval) == 1) {
        # single point forms its own node
        nodes[[length(nodes) + 1]] <- points_in_interval
        node_interval_id <- c(node_interval_id, i)
      } else {
        # cluster the points (use distance matrix, precomputed, symmetrically)
        interval_clusters <- clustering(data = d_matrix[points_in_interval, points_in_interval], ...)
        num_clusters <- max(interval_clusters)

        for (cl in 1:num_clusters) {
          nodes[[length(nodes) + 1]] <- points_in_interval[interval_clusters == cl]
          node_interval_id <- c(node_interval_id, i)
        }
      }
    }
  }

  # pruning if requested
  if (pruning) {
    if (is.null(min_node_size) || min_node_size < 1) {
      stop("min_node_size must be a positive integer for pruning.")
    }
    valid_nodes <- sapply(nodes, length) >= min_node_size
    nodes <- nodes[valid_nodes]
    node_interval_id <- node_interval_id[valid_nodes]
  }

  # compute the edges of the mapper construction
  num_nodes <- length(nodes)
  adj_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)

  if (num_nodes > 1) {
    for (u in 1:(num_nodes - 1)) {
      for (v in (u + 1):num_nodes) {
        # intersections create an edge
        if (length(intersect(nodes[[u]], nodes[[v]])) > 0) {
          adj_matrix[u, v] <- 1
          adj_matrix[v, u] <- 1
        }
      }
    }
  }

  return(list(nodes = nodes, adjacency = adj_matrix, interval_id = node_interval_id))
}
