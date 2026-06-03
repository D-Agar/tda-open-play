library(igraph)
library(visNetwork)
library(htmltools)
library(htmlwidgets)

# construction
create_mapper_graph <- function(mapper_obj, lens, original_data, groups = NULL) {
  # create a mapper graph, with node sizes, colours, and the option for nodes to be pie charts
  g <- graph_from_adjacency_matrix(mapper_obj$adjacency, mode = "undirected")
  
  # node size is the cluster size
  V(g)$size <- sapply(mapper_obj$nodes, length)
  
  # node colour is the lens value (by default)
  mean_lens_vals <- sapply(mapper_obj$nodes, function(indices)
    return(mean(lens[indices])))
  val_range <- max(mean_lens_vals) - min(mean_lens_vals)
  
  # avoid division by zero if all mean lens values are the same (range = 0)
  if (val_range == 0) {
    scaled_vals <- rep(0.5, length(mean_lens_vals)) # no variance
  } else {
    scaled_vals <- (mean_lens_vals - min(mean_lens_vals)) / val_range
  }
  
  # colour gradient from blue to yellow to red
  colour_palette_fn <- colorRampPalette(c("blue", "yellow", "red"))
  V(g)$color <- colour_palette_fn(100)[round(scaled_vals * 99) + 1]
  
  # make node pie charts (optional)
  if (!is.null(groups)) {
    # groups is a list (cat_var, cat_colour)
    categories <- unique(original_data[[groups$cat_var]])
    
    # proportions of each node
    pie_data <- lapply(mapper_obj$nodes, function(indices) {
      counts <- table(factor(original_data[indices, groups$cat_var], levels = categories))
      return(as.numeric(counts))
    })
    
    # igraph allows list of numeric vectors for the pie attribute
    V(g)$pie <- pie_data
    V(g)$pie.color <- list(groups$cat_colour)
  }
  return(g)
}

add_continuous_legend <- function(network_obj,
                                  title = "Continuous Filter",
                                  colors = c("blue", "yellow", "red"),
                                  labels = c("Low", "High")) {
  # css gradient string
  gradient_string <- paste(colors, collapse = ", ")
  gradient_css <- sprintf("linear-gradient(to right, %s)", gradient_string)
  
  # html widget to go in visualisation
  color_bar <- tags$div(
    style = "position: absolute; bottom: 30px; left: 30px; z-index: 1000; background: rgba(255,255,255,0.9); padding: 12px; border: 1px solid #ccc; border-radius: 5px; font-family: sans-serif; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
    tags$b(title),
    tags$div(
      style = paste0(
        "width: 200px; height: 15px; background: ",
        gradient_css,
        "; margin-top: 8px; margin-bottom: 5px; border-radius: 3px;"
      )
    ),
    tags$div(style = "display: flex; justify-content: space-between; font-size: 12px; color: #333; font-weight: bold;", tags$span(labels[1]), tags$span(labels[2]))
  )
  
  # add to visualisation
  return(htmlwidgets::appendContent(network_obj, color_bar))
}

create_vis_graph <- function(graph, mapper_obj, original_data) {
  vis_data <- toVisNetworkData(graph)
  # numeric column names
  num_cols <- names(original_data)[sapply(original_data, is.numeric)]
  
  info_strings <- sapply(1:nrow(vis_data$nodes), function(i) {
    # node index exists if mapper was pruned
    if (i <= length(mapper_obj$nodes)) {
      indices <- mapper_obj$nodes[[i]]
      
      html <- paste0(
        "<div style='min-width: 150px; font-family: sans-serif;'>",
        "<h4 style='margin-top:0;'>Node ID: ",
        i,
        "</h4>",
        "<b>Size:</b> ",
        length(indices),
        "<hr style='margin: 5px 0;'>"
      )
      
      # add average of every numeric column for the cluster
      for (col in num_cols) {
        avg_val <- mean(original_data[[col]][indices], na.rm = TRUE)
        html <- paste0(html, "<b>Avg ", col, ":</b> ", round(avg_val, 2), "<br>")
      }
      html <- paste0(html, "</div>")
    } else {
      html <- paste0("<div style='font-family: sans-serif;'><b>Empty Node</b> (Pruned)</div>")
    }
    return(html)
  })
  
  vis_data$nodes$title <- info_strings
  
  # pie chart rendering (if it exists)
  if (!is.null(V(graph)$pie)) {
    # 'dot' shape keeps size and scaling constant
    vis_data$nodes$shape <- "dot"
  }
  
  # build graph/network
  network <- visNetwork(vis_data$nodes,
                        vis_data$edges,
                        width = "100%",
                        height = "800px") |>
    visNodes(
      borderWidth = 2,
      borderWidthSelected = 4,
      shadow = list(enabled = TRUE, size = 10)
    ) |>
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01
      ),
      stabilization = list(enabled = TRUE, iterations = 200)
    ) |>
    visOptions(
      # hover highlighting
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE,
      # double-click cluster to collapse
      collapse = TRUE
    ) |>
    visInteraction(navigationButtons = TRUE, tooltipDelay = 200)
  return(network)
}

save_vis_graph <- function(vis_graph, filename) {
  saveWidget(
    vis_graph %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE,
        autoResize = TRUE,
        clickToUse = TRUE,
      ),
    file = filename
  )
}
