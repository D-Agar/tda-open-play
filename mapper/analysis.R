library(tidyverse)
library(igraph)

get_node_mapping <- function(mapper_obj, original_data) {
  actual_ids <- original_data$index
  tibble(
    node = seq_along(mapper_obj$points_in_vertex),
    index = purrr:::map(mapper_obj$points_in_vertex, function(mapper_indices) {
      return(actual_ids[mapper_indices])
    })
  ) |>
    # flatten for multiple rows of indices
    unnest(index) |>
    # nodes become lists
    group_by(index) |>
    summarise(node = list(as.numeric(node)), .groups = "drop")
}

find_isolated_components <- function(mapper_obj, original_data) {
  m_graph <- graph_from_adjacency_matrix(mapper_obj$adjacency, mode = "undirected")
  graph_components <- components(m_graph)

  component_sizes <- graph_components$csize
  main_trunk_id <- which.max(component_sizes)
  message(graph_components$no, " number of components")

  isolated_component_ids <- setdiff(seq_along(component_sizes), main_trunk_id)

  if (length(isolated_component_ids) == 0) {
    message("No isolated components found. The entire graph is completely connected.")
    return(NULL)
  }

  message(
    "Found ", length(isolated_component_ids),
    " isolated component(s) detached from the main trunk."
  )

  # row ID -> component ID
  node_to_component <- data.frame(
    node      = seq_along(graph_components$membership),
    component = graph_components$membership
  )
  return(node_to_component)
}

# mapper node data
get_node_details <- function(mapper_obj, original_data, node_id) {
  if (node_id > mapper_obj$num_vertices || node_id < 1) {
    stop("Invalid node_id requested")
  }

  point_indices <- mapper_obj$points_in_vertex[[node_id]]

  node_data <- original_data[point_indices, ]
  return(node_data)
}

# summary statistics of the mapper nodes
summarise_graph <- function(mapper_obj, original_data) {
  original_data_size <- nrow(original_data)
  numeric_cols <- colnames(original_data)[sapply(original_data, is.numeric)]
  categoric_cols <- setdiff(colnames(original_data), numeric_cols)

  metrics <- lapply(seq_along(mapper_obj$points_in_vertex), function(node) {
    node_data <- get_node_details(mapper_obj, original_data, node)
    node_size <- length(mapper_obj$points_in_vertex[[node]])
    size_pct <- round((node_size / original_data_size) * 100, 4)

    node_metrics <- data.frame(
      size = node_size,
      size_pct = size_pct
    )
    if (node_size > 0) {
      if (length(numeric_cols) > 0) {
        means <- sapply(node_data[, numeric_cols, drop = FALSE], mean, na.rm = TRUE)
        names(means) <- paste0("mean_", numeric_cols)

        medians <- sapply(node_data[, numeric_cols, drop = FALSE], median, na.rm = TRUE)
        names(medians) <- paste0("median_", numeric_cols)

        numeric_values <- cbind(as.data.frame(t(means)), as.data.frame(t(medians)))
        node_metrics <- cbind(node_metrics, numeric_values)
      }
      # if (length(categoric_cols) > 0) {
      #   modes = sapply(node_data[, categoric_cols, drop = FALSE], find_mode)
      #   names(modes) <- paste0("mode_", categoric_cols)
      #   categoric_values <- as.data.frame(t(modes))
      #   node_metrics <- cbind(node_metrics, categoric_values)
      # }
    }
    return(node_metrics)
  })

  return(do.call(rbind, metrics))
}
