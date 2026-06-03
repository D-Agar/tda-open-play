cosine_distance <- function(data_matrix) {
  X <- as.matrix(data_matrix)
  magnitudes <- sqrt(rowSums(X^2))
  
  # prevent division by zero for any rows that are all zeros
  magnitudes[magnitudes == 0] <- 1e-10 
  X_normalized <- X / magnitudes
  
  # cosine similarity matrix with matrix multiplication
  # Multiplying the normalized matrix by its transpose gets all dot products
  sim_matrix <- X_normalized %*% t(X_normalized)
  
  # similarity to distance (1 - similarity)
  # cap values to strictly between 0 and 1
  dist_matrix <- 1 - sim_matrix
  dist_matrix[dist_matrix < 0] <- 0
  
  # convert to 'dist' object (for hclust and Mapper)
  return(as.dist(dist_matrix))
}

pearson_distance <- function(data_matrix) {
  X <- as.matrix(data_matrix)
  
  # pearson correlation between rows
  pearson_cor <- cor(t(X), method = "pearson")
  
  # r ranges from -1 (perfect opposite) to 1 (perfect identical)
  dist_matrix <- 1 - pearson_cor
  
  # floating point errors (e.g., -0.000000001)
  dist_matrix[dist_matrix < 0] <- 0
  return(as.dist(dist_matrix))
}
