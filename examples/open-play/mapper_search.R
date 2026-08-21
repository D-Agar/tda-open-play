# Mapper construction grid search
# Environment setup
library(tidyverse)
library(MapperAlgo)
library(igraph)
library(jsonlite)
library(Rtsne)
library(uwot)
library(dbscan)

# Script parameters
data_size <- "more_telem"
trial_name <- "pca12_final"
scaler <- "zscale"
lenses <- c("PC1", "PC2")
dir_name <- paste0(data_size, "_", trial_name, "_", tolower(scaler))

# Grid search parameters
cover_types <- c("extension")
intervals_grid <- c(10)
overlaps_grid <- c(40, 50)
widths_grid <- list(NULL)

# options(scipen = 10)
# hist(
#   dist(mapper_scaled),
#   main = "Min-Max Mapper distances",
#   xlab = "Distance",
#   ylab = "Frequency"
# )
# pdf("knn_distplot_minmax.pdf")
# List of clustering methods and their respective hyperparameter sets
# kNNdistplot(x = mapper_scaled, k = 34) # shows k~0.5 is good
# dev.off()

clustering_configs <- list(
  # list(method = "kmeans", params = list(max_kmeans_clusters = 2)),
  # list(method = "dbscan", params = list(eps = 0.05, minPts = 3))
  # list(method = "dbscan", params = list(eps = 0.05, minPts = 10)),
  # list(method = "dbscan", params = list(eps = 2.5, minPts = 5))
  list(method = "dbscan", params = list(eps = 3, minPts = 5))
  # list(method = "dbscan", params = list(eps = 0.4, minPts = 5)) # calculated version
  # list(method = "dbscan", params = list(eps = 0.1, minPts = 10))
  # list(method = "dbscan", params = list(eps = 0.2, minPts = 10)),
  # list(method = "dbscan", params = list(eps = 0.4, minPts = 15)),
  # list(method = "dbscan", params = list(eps = 0.5, minPts = 15)),
  # list(method = "dbscan", params = list(eps = 0.5, minPts = 8)),
  # list(method = "dbscan", params = list(eps = 0.1, minPts = 5)),
  # list(method = "dbscan", params = list(eps = 0.1, minPts = 3))
  # list(method = "hierarchical", params = list(method = "single", num_bins_when_clustering = 5)),
  # list(method = "hierarchical", params = list(method = "single", num_bins_when_clustering = 10)),
  # list(method = "hierarchical", params = list(method = "single", num_bins_when_clustering = 15)),
  # list(method = "hierarchical", params = list(method = "single", num_bins_when_clustering = 20))
)

# Generate parameter grid for iteration
param_grid <- tidyr::crossing(
  cover = cover_types,
  intervals = intervals_grid,
  overlap = overlaps_grid,
  width = widths_grid,
  config_idx = seq_along(clustering_configs)
)
total_runs <- nrow(param_grid)

message(paste0("Performing mapper grid search for: ", dir_name))
message(paste0("Total grid search iterations to process: ", total_runs))

mapper_trials_dir <- file.path("examples", "open-play", "grid_searches", dir_name)
if (!dir.exists(mapper_trials_dir)) {
  dir.create(mapper_trials_dir, recursive = TRUE)
}

message("Loading base mapper data...")
if (!exists("biweekly_mapper_data")) {
  source("examples/open-play/mapper_data.R")
}

make_mapper_filename <- function(
  mapper_name = "mapper",
  cover_type = NULL,
  intervals = NULL,
  overlap = NULL,
  width = NULL,
  method = NULL,
  method_params = NULL,
  ext = "json"
) {
  parts <- c()

  if (!is.null(mapper_name) && nzchar(mapper_name)) {
    parts <- c(parts, mapper_name)
  } else {
    parts <- c(parts, "mapper")
  }
  if (!is.null(cover_type) && !is.na(cover_type) && nzchar(cover_type)) {
    parts <- c(parts, paste0("cov", cover_type))
  }
  if (!is.null(intervals) && length(intervals) > 0 && !any(is.na(intervals))) {
    parts <- c(parts, paste0("int", paste(intervals, collapse = "x")))
  }
  if (!is.null(overlap) && !is.na(overlap)) {
    parts <- c(parts, paste0("ov", overlap))
  }
  if (!is.null(width) && !is.na(width)) {
    parts <- c(parts, paste0("w", width))
  }
  if (!is.null(method) && !is.na(method) && nzchar(method)) {
    parts <- c(parts, method)
  }
  if (!is.null(method_params) && length(method_params) > 0) {
    valid_params <- method_params[!sapply(method_params, function(x) is.null(x) || is.na(x))]
    if (length(valid_params) > 0) {
      param_names <- names(valid_params)
      if (is.null(param_names)) param_names <- rep("", length(valid_params))
      param_str <- paste(param_names, unlist(valid_params), sep = "", collapse = "_")
      parts <- c(parts, param_str)
    }
  }
  ext <- sub("^\\.", "", ext)
  paste0(paste(parts, collapse = "_"), ".", ext)
}

message("Preparing data and lenses...")
if (data_size == "shorter") {
  # Shortlisted, unscaled data
  mapper_data <- biweekly_mapper_data |>
    mutate(
      wemwbs_total = rowSums(across(starts_with("wemwbs_")), na.rm = TRUE),
      bangs_auto_sat = rowSums(across(c(bangs_1, bangs_2, bangs_3)), na.rm = TRUE),
      bangs_auto_frus = rowSums(across(c(bangs_4, bangs_5, bangs_6)), na.rm = TRUE),
      bangs_comp_sat = rowSums(across(c(bangs_7, bangs_8, bangs_9)), na.rm = TRUE),
      bangs_comp_frus = rowSums(across(c(bangs_10, bangs_11, bangs_12)), na.rm = TRUE),
      bangs_rel_sat = rowSums(across(c(bangs_13, bangs_14, bangs_15)), na.rm = TRUE),
      bangs_rel_frus = rowSums(across(c(bangs_16, bangs_17, bangs_18)), na.rm = TRUE),
      gaming_value_sum = rowSums(across(starts_with("gaming_value_")), na.rm = TRUE)
      # log_total_wave_minutes = log1p(total_wave_minutes)
    ) |>
    select(
      wave,
      aff_val = affective_valence,
      wemwbs_total,
      bangs_auto_sat, bangs_auto_frus,
      bangs_comp_sat, bangs_comp_frus,
      bangs_rel_sat, bangs_rel_frus,
      gaming_value_sum,
      total_wave_minutes,
      max_binge_minutes,
      atten_score_mean = score_mean
    )
} else if (data_size == "more_telem") {
  mapper_data <- biweekly_mapper_data |>
    mutate(
      wemwbs_total = rowSums(across(starts_with("wemwbs_")), na.rm = TRUE),
      bangs_auto_sat = rowSums(across(c(bangs_1, bangs_2, bangs_3)), na.rm = TRUE),
      bangs_auto_frus = rowSums(across(c(bangs_4, bangs_5, bangs_6)), na.rm = TRUE),
      bangs_comp_sat = rowSums(across(c(bangs_7, bangs_8, bangs_9)), na.rm = TRUE),
      bangs_comp_frus = rowSums(across(c(bangs_10, bangs_11, bangs_12)), na.rm = TRUE),
      bangs_rel_sat = rowSums(across(c(bangs_13, bangs_14, bangs_15)), na.rm = TRUE),
      bangs_rel_frus = rowSums(across(c(bangs_16, bangs_17, bangs_18)), na.rm = TRUE),
      gaming_value_sum = rowSums(across(starts_with("gaming_value_")), na.rm = TRUE)
    ) |>
    select(
      aff_val = affective_valence,
      wemwbs_total,
      bangs_auto_sat, bangs_auto_frus,
      bangs_comp_sat, bangs_comp_frus,
      bangs_rel_sat, bangs_rel_frus,
      gaming_value_sum,
      days_played,
      total_wave_minutes,
      total_steam_minutes,
      total_xbox_minutes,
      total_mobile_minutes,
      total_nintendo_minutes,
      max_binge_minutes,
      atten_score_mean = score_mean
    )
} else if (data_size == "wellbeing") {
  mapper_data <- biweekly_mapper_data |>
    select(
      all_of(starts_with("bangs_")),
      all_of(starts_with("wemwbs_"))
    )
} else {
  # Longer, unscaled data
  mapper_data <- biweekly_mapper_data |>
    mutate(
      wemwbs_total = rowSums(across(starts_with("wemwbs_")), na.rm = TRUE),
      bangs_auto_sat = rowSums(across(c(bangs_1, bangs_2, bangs_3)), na.rm = TRUE),
      bangs_auto_frus = rowSums(across(c(bangs_4, bangs_5, bangs_6)), na.rm = TRUE),
      bangs_comp_sat = rowSums(across(c(bangs_7, bangs_8, bangs_9)), na.rm = TRUE),
      bangs_comp_frus = rowSums(across(c(bangs_10, bangs_11, bangs_12)), na.rm = TRUE),
      bangs_rel_sat = rowSums(across(c(bangs_13, bangs_14, bangs_15)), na.rm = TRUE),
      bangs_rel_frus = rowSums(across(c(bangs_16, bangs_17, bangs_18)), na.rm = TRUE),
      gaming_value_sum = rowSums(across(starts_with("gaming_value_")), na.rm = TRUE)
    ) |>
    rename(atten_score_mean = score_mean) |>
    select(where(is.numeric))
}

# mapper_data |>
#   add_column(biweekly_mapper_data$adhd) |>
#   rename(adhd = `biweekly_mapper_data$adhd`) |>
#   ggpairs(aes(colour = adhd, alpha = 0.5))

# ggpairs(
#   biweekly_mapper_filters,
#   columns = 2:ncol(biweekly_mapper_filters),
#   aes(colour = adhd, alpha = 0.5)
# )

# Create reference data for post-construction analysis
mapper_full_data <- mapper_data |>
  add_column(biweekly_mapper_data$pid) |>
  rename(pid = `biweekly_mapper_data$pid`) |>
  left_join(select(intake_participants, pid, adhd), by = "pid") |>
  mutate(any_adhd = if_else(adhd == "Neurotypical", 0, 1))

# Data scaling

# Min-Max Normalisation (commonly used in Kepler Mapper)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

if (scaler == "minmax") {
  mapper_scaled <- mapper_data |>
    mutate(across(everything(), min_max_norm))
} else if (scaler == "zscale") {
  mapper_scaled <- mapper_data |>
    mutate(across(everything(), ~ scale(.)[, 1]))
} else if (scaler == "none") {
  mapper_scaled <- mapper_data
} else {
  stop("No valid scaler selected")
}


# Lens configuration

# Dist mean
dist_mean <- function(data) {
  scaled_col_means <- colMeans(data)
  # the euclidean distance between two single points is just the absolute difference
  A_dist <- sweep(data, 2, scaled_col_means, "-")
  rowSums(A_dist^2)
}

# Create lens/filter functions
mapper_pca <- prcomp(mapper_data, center = TRUE, scale = TRUE)
mapper_tsne <- Rtsne(
  mapper_data,
  dims = 1,
  pca_center = TRUE,
  pca_scale = TRUE
)
set.seed(30)
mapper_umap <- umap2(
  mapper_data,
  n_components = 2,
  scale = TRUE,
  n_neighbors = 15, # default
  min_dist = 0.001
)

# round(cor(cbind(mapper_scaled, pc1 = mapper_pca$x[, 1], pc2 = mapper_pca$x[ , 2]))[, c("pc1", "pc2")], 2)

tsne_vec <- as.vector(mapper_tsne$Y)
lens_values <- matrix(nrow = nrow(mapper_data), ncol = length(lenses))
lens_idx <- 0
for (lens in lenses) {
  lens_idx <- lens_idx + 1
  # PCA
  if (startsWith(lens, "PC")) {
    component <- as.integer(sub("^PC", "", lens))
    lens_values[, lens_idx] <- mapper_pca$x[, component]
  } else if (startsWith(lens, "umap")) {
    umap_dim <- as.integer(sub("^umap", "", lens))
    lens_values[, lens_idx] <- mapper_umap[, umap_dim]
  } else if (lens == "tsne") {
    lens_values[, lens_idx] <- as.vector(mapper_tsne$Y)
  } else if (lens == "adhd_binary") {
    lens_values[, lens_idx] <- ifelse(biweekly_mapper_data$adhd == "Diagnosed ADHD", 1, 0)
  } else if (lens == "eccentricity") {
    # distances on scaled data
    mapper_dists <- as.matrix(dist(mapper_scaled, method = "euclidean"))
    lens_values[, lens_idx] <- apply(mapper_dists, 1, max)
  } else if (lens == "xgb") {
    lens_values[, lens_idx] <- biweekly_mapper_filters$xgb_adhd_pred
  } else if (lens == "dist_mean") {
    lens_values[, lens_idx] <- dist_mean(mapper_scaled)
  } else if (lens == "density_estimation") {
    lens_values[, lens_idx] <- as.vector(density_estimation(dist(mapper_scaled))$values)
  } else if (startsWith(lens, "col_")) {
    column <- sub("^col_", "", lens)
    lens_values[, lens_idx] <- mapper_data[[column]]
  }
}

for (i in seq_len(total_runs)) {
  # Extract current iteration parameters
  row_params <- param_grid[i, ]
  cov <- row_params$cover
  int_val <- row_params$intervals
  ov <- row_params$overlap
  w_val <- row_params$width[[1]]
  config <- clustering_configs[[row_params$config_idx]]
  cur_method <- config$method
  cur_params <- config$params

  # Build destination filename and path
  filename <- make_mapper_filename(
    mapper_name = paste0(data_size, "_", trial_name),
    cover_type = cov,
    intervals = int_val,
    overlap = ov,
    width = w_val,
    method = cur_method,
    method_params = cur_params,
    ext = "json"
  )
  target_file <- here::here(mapper_trials_dir, filename)

  # Skip already computed runs
  if (file.exists(here::here(target_file))) {
    message(sprintf(
      "[%d/%d] Skipping existing run: %s",
      i,
      total_runs,
      filename
    ))
    next
  }

  message(sprintf("[%d/%d] Running Mapper: %s", i, total_runs, filename))

  # Catch bad parameters so run can continue
  tryCatch(
    {
      mapper_out <- MapperAlgo(
        original_data = mapper_scaled,
        filter_values = lens_values,
        percent_overlap = ov,
        methods = cur_method,
        method_params = cur_params,
        cover_type = cov,
        intervals = int_val,
        interval_width = w_val,
        num_cores = 12
      )

      export_data <- list(
        adjacency = mapper_out$adjacency,
        num_vertices = mapper_out$num_vertices,
        level_of_vertex = mapper_out$level_of_vertex,
        # Subtract 1 from every point index: MAJOR BUG IN MAPPERALGO
        # points_in_vertex = lapply(mapper_out$points_in_vertex, function(x) x - 1),
        points_in_vertex = mapper_out$points_in_vertex,
        original_data = mapper_full_data
      )

      write(
        toJSON(export_data, auto_unbox = TRUE),
        file = target_file
      )
    },
    error = function(e) {
      warning(sprintf(
        "[%d/%d] ERROR on %s: %s",
        i,
        total_runs,
        filename,
        e$message
      ))
    }
  )
}
message("Grid search completed successfully!")
