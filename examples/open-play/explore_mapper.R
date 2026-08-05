source("utils.R")
sourceDir("mapper", exclude = "test.R")

dirname <- "shorter_pca12_min-max"
filename <- "shorter_pca12_covstride_int5_ov30_dbscan_eps0.1_minPts5.json"

file_location <- file.path(
  "examples", "open-play", "grid_searches", dirname, filename
)
mapper_json <- fromJSON(here::here(file_location))

nodes <- c(1, 6)
nodes_data <- list()

for (node_id in nodes) {
  temp_data <- get_node_details(
    mapper_json,
    mapper_json$original_data,
    node_id
  )
  nodes_data[[node_id]] <- temp_data
}
