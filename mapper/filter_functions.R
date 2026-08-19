density_estimation <- function(d_matrix, epsilon = NULL) {
  d_matrix <- as.matrix(d_matrix)
  n <- nrow(d_matrix)

  # squared distances
  d_matrix_sq <- d_matrix^2

  # dimension-adjusted scale, from kde function
  if (is.null(epsilon)) {
    # Number of features/dimensions
    d <- ncol(d_matrix)
    med_dist <- median(d_matrix[upper.tri(d_matrix)])

    # Approximate a dimension-adjusted bandwidth, then square it for your formula
    bandwidth <- med_dist * (4 / (d + 2))^(1 / (d + 4))
    epsilon <- 2 * (bandwidth^2)
  }

  # exponential kernel component (Gaussian)
  kernel_matrix <- exp(-d_matrix_sq / epsilon)

  # sum over each point
  sum_y <- rowSums(kernel_matrix)

  # normalisation constant C_epsilon
  c_eps <- 1 / n

  estimation <- list()
  estimation$values <- c_eps * sum_y
  estimation$epsilon <- epsilon

  return(estimation)
}

calculate_eccentricity <- function(d_matrix, exponent = 2) {
  # row-wise vector norms
  row_norm <- (abs(d_matrix)^exponent)^(1 / exponent)
  norm_sums <- rowSums(row_norm)

  # keep dimensions
  return(matrix(norm_sums))
}
