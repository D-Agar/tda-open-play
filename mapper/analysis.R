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

  message("Found ", length(isolated_component_ids),
    " isolated component(s) detached from the main trunk."
  )

  # row ID -> component ID
  node_to_component <- data.frame(
    node      = seq_along(graph_components$membership),
    component = graph_components$membership
  )
  return(node_to_component)
}

# get the flares of the mapper graph, 
get_flares <- function(mapper_obj, original_data, attr, id) {

}