density_estimation <- function(d_matrix, epsilon = NULL) {
  d_matrix <- as.matrix(d_matrix)
  d_matrix_sq <- d_matrix^2

  # if epsilon is not provided, use the median heuristic
  if (is.null(epsilon)) {
    # median of the upper triangle to avoid the 0s on the diagonal
    epsilon <- median(d_matrix_sq[upper.tri(d_matrix_sq)])
  }

  kernel_matrix <- exp(-d_matrix_sq / epsilon)
  # maybe need to calculate the constant for correct integral
  density_estimates <- rowSums(kernel_matrix)

  estimation <- list()
  estimation$lens <- density_estimates / nrow(d_matrix) # normalise
  estimation$epsilon <- epsilon

  return(estimation)
}
