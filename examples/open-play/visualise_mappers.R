# R script to create all visualisations inside a specified directory
# We keep the same directory structure for consistency
library(jsonlite)
library(igraph)

if (!exists("mapper_full_data")) {
  source(here::here("examples/open-play/mapper_data.R"))
}
source(here::here("utils.R"))
sourceDir(here::here("mapper/"), exclude = "test.R")

get_directory_file_map <- function(root_dir = ".") {
  # get all (sub)directories
  all_dirs <- list.dirs(path = root_dir, full.names = TRUE, recursive = TRUE)

  # keys are relative paths
  keys <- sub("^\\./", "", all_dirs)
  keys[keys == all_dirs[1]] <- "."

  # map each directory to its immediate files only (ignoring subfolders)
  file_map <- setNames(
    lapply(all_dirs, function(d) {
      items <- list.files(d, full.names = FALSE, recursive = FALSE, no.. = TRUE)
      items[!dir.exists(file.path(d, items))]
    }),
    keys
  )

  return(file_map)
}

generate_mapper_figures <- function(source_dir = ".", output_dir = NULL) {
  # directory-file mapping
  clean_root <- sub("/+$", "", source_dir)
  dir_map <- get_directory_file_map(clean_root)

  base_out <- if (!is.null(output_dir)) {
    if (output_dir %in% c("", ".")) "./" else sub("/+$", "", output_dir)
  } else {
    file.path(clean_root, "visualisations")
  }

  # iterate through every folder
  for (dir_path in names(dir_map)) {
    dir_modified <- FALSE
    dir_created <- FALSE

    files <- dir_map[[dir_path]]

    rel_path <- sub(clean_root, "", dir_path, fixed = TRUE)
    rel_path <- sub("^/+", "", rel_path)

    # get output directory
    target_dir <- if (rel_path %in% c("", ".")) {
      base_out
    } else {
      file.path(base_out, rel_path)
    }

    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE, showWarnings = TRUE)
      dir_created <- TRUE
    }

    for (file in files) {
      if (!endsWith(file, ".json")) next

      graph_filename <- sub("\\.json$", ".pdf", file)
      output_pdf_path <- file.path(target_dir, graph_filename)

      if (file.exists(output_pdf_path)) {
        next
      }
      dir_modified <- TRUE
      json_path <- file.path(dir_path, file)
      mapper_json <- fromJSON(json_path)

      pdf(output_pdf_path)
      # create graph with adhd proportions as colour scale
      set.seed(2)
      create_mapper_graph(
        mapper_obj = mapper_json,
        colourisation = mapper_full_data$any_adhd,
        original_data = mapper_data,
        graph_layout = layout_with_fr,
        legend = TRUE,
        legend_title = "Proportion of ADHD Participants",
        legend_tick_num = 5
      )
      dev.off()
    }
    if (dir_created == TRUE) {
      message(paste0("[INFO] Created '", target_dir, "'"))
    } else if (dir_modified == TRUE) {
      message(paste0("[INFO] Modified '", target_dir, "'"))
    } else {
      message(paste0("[INFO] Directory '", target_dir, "' skipped"))
    }
  }
}
