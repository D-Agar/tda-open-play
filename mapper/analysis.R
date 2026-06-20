get_node_mapping <- function(mapper_obj) {
  do.call(
    rbind,
    sapply(
        seq_along(mapper_obj$points_in_vertex), function(vertex) {
            points <- mapper_obj$points_in_vertex[[vertex]]
            node_rep <- replicate(length(points), vertex)
            df <- data.frame(
                node = node_rep,
                index = points
            )
            return(df)
        },
        simplify = FALSE
    )
  )
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
