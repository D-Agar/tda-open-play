library(igraph)
library(fields)
library(visNetwork)
library(htmltools)
library(htmlwidgets)
source("utils.R")

# construction
create_mapper_graph <- function(
  mapper_obj, colourisation, original_data, groups = NULL,
  graph_layout = layout_with_kk, legend = TRUE,
  legend_title = NULL, legend_tick_num = 5
) {
  # create a mapper graph, with node sizes, colours, and the option for nodes to be pie charts
  g <- graph_from_adjacency_matrix(mapper_obj$adjacency, mode = "undirected")

  # detect 0-indexed JSON (from previous fix)
  all_indices <- unlist(mapper_obj$points_in_vertex)
  is_zero_indexed <- length(all_indices) > 0 && min(all_indices, na.rm = TRUE) == 0

  pts_list <- lapply(mapper_obj$points_in_vertex, function(idx) {
    if (is_zero_indexed) idx + 1 else idx
  })

  # node size is a constant + sqrt(node size)
  node_counts <- sapply(pts_list, length)
  V(g)$size <- 2 + sqrt(node_counts)

  is_colourisation_categorical <- is.character(colourisation) || is.factor(colourisation)

  if (is_colourisation_categorical) {
    colourisation <- as.factor(colourisation)
    cat_levels <- levels(colourisation)

    mode_colours <- sapply(pts_list, function(indices) {
      indices <- indices[!is.na(indices) & indices > 0]
      if (length(indices) == 0) {
        return(NA_character_)
      }
      vals <- colourisation[indices]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        return(NA_character_)
      }
      u <- unique(vals)
      as.character(u[which.max(tabulate(match(vals, u)))])
    })

    # categoric colour palette
    cat_palette <- hcl.colors(length(cat_levels), palette = "Set 2")
    names(cat_palette) <- cat_levels
    V(g)$color <- cat_palette[mode_colours]
  } else {
    # node colour is the lens value (by default)
    colourisation <- as.matrix(colourisation)
    # Euclidean distance magnitude from the center across dimensions
    mean_colours <- sapply(pts_list, function(indices) {
      indices <- indices[!is.na(indices) & indices > 0]
      if (length(indices) == 0) {
        return(0)
      }
      sub_mat <- colourisation[indices, , drop = FALSE]
      dim_means <- colMeans(sub_mat, na.rm = TRUE)
      dim_means[is.nan(dim_means) | is.na(dim_means)] <- 0
      return(sqrt(sum(dim_means^2)))
    })

    min_val <- min(mean_colours, na.rm = TRUE)
    max_val <- max(mean_colours, na.rm = TRUE)
    val_range <- max(mean_colours) - min(mean_colours)
    # avoid division by zero if all mean lens values are the same (range = 0)
    if (is.na(val_range) || is.nan(val_range) || val_range == 0) {
      scaled_vals <- rep(0.5, length(mean_colours))
    } else {
      scaled_vals <- (mean_colours - min_val) / val_range
    }

    # colour gradient from blue to yellow to red
    colour_palette_fn <- colorRampPalette(c("blue", "yellow", "red"))
    V(g)$color <- colour_palette_fn(100)[round(scaled_vals * 99) + 1]
  }

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

  par(mar = c(1, 1, 1, 4))
  plot(g, layout = graph_layout(g))

  if (legend) {
    if (is_colourisation_categorical) {
      legend(
        "right",
        legend = cat_levels,
        fill = cat_palette,
        title = legend_title,
        xpd = TRUE,
        bty = "n"
      )
    } else {
      z_limits <- c(min(colourisation), max(colourisation))
      legend <- setupLegend(
        horizontal = FALSE,
        legend.shrink = 0.5,
        legend.width = 1.0,
        legend.mar = 4.1
      )
      ticks <- seq(z_limits[1], z_limits[2], length.out = legend_tick_num)

      addLegend(
        legend,
        zlim = z_limits,
        col = colour_palette_fn(100),
        legend.args = list(
          text = legend_title,
          side = 2,
          line = 0.5
        ),
        # axis.args to control the tick lines
        axis.args = list(
          at = ticks,
          labels = round(ticks, 2)
        )
      )
    }
  }
  return(g)
}

add_continuous_legend <- function(
  network_obj,
  title = "Continuous Filter",
  colours = c("blue", "yellow", "red"),
  labels = c("Low", "High")
) {
  # css gradient string
  gradient_string <- paste(colours, collapse = ", ")
  gradient_css <- sprintf("linear-gradient(to right, %s)", gradient_string)

  # html widget to go in visualisation
  colour_bar <- tags$div(
    style = "position: absolute; top: 30px; z-index: 1000; background: rgba(255,255,255,0.9); padding: 12px; border: 1px solid #ccc; border-radius: 5px; font-family: sans-serif; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);",
    tags$b(title),
    tags$div(
      style = paste0(
        "width: 200px; height: 15px; background: ",
        gradient_css,
        "; margin-top: 8px; margin-bottom: 5px; border-radius: 3px;"
      )
    ),
    tags$div(
      style = "display: flex; justify-content: space-between; font-size: 12px; color: #333; font-weight: bold;",
      tags$span(labels[1]),
      tags$span(labels[2])
    )
  )

  # add to visualisation
  # relative positions to avoid locked legend in previews
  combined_html <- tags$div(
    style = "position: relative; width: 100%; height: 800px; border: 1px solid #f0f0f0;",
    network_obj,
    colour_bar
  )
  # render correctly
  return(browsable(combined_html))
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
    height = "800px"
  ) |>
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
      collapse = TRUE,
      autoResize = TRUE,
      clickToUse = TRUE
    ) |>
    visInteraction(navigationButtons = TRUE, tooltipDelay = 200)
  return(network)
}

save_vis_graph <- function(vis_graph, filename) {
  save_html(vis_graph, file = filename)
}
