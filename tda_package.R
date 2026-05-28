# NOTE: ensure you have the required dependency `gmp` installed in your system.
# For Fedora, I had to perform `sudo dnf install gmp-devel mpfr-devel`
library("TDA")

# Uniform sample on manifolds (topological space appearing flat when zoomed in)
circleSample <- circleUnif(n = 20, r = 1)
plot(circleSample, xlab = "", ylab = "", pch = 20)

# Distance functions and Density estimators
# Set of points sampled from a distribution P
X <- circleUnif(n = 400, r = 1)
lim <- c(-1.7, 1.7)
by <- 0.05
margin <- seq(from = lim[1], to = lim[2], by = by)
Grid <- expand.grid(margin, margin)

# Standard distance function: Euclidean distance
distance <- distFct(X = X, Grid = Grid)

# Distance To Measure (DTM): smoothed version of distance function
# computed for each point of the Grid
m0 <- 0.1 # smoothing parameter
DTM <- dtm(X = X, Grid = Grid, m0 = m0)

# kNN Density Estimator
k <- 60
kNN <- knnDE(X = X, Grid = Grid, k = k)

# Gaussian Kernel Density Estimator (KDE)
h <- 0.3 # smoothing parameter
KDE <- kde(X = X, Grid = Grid, h = h)

# Kernel distance estimator
Kdist <- kernelDist(X = X, Grid = Grid, h = h)

# Visualise these
par(mfrow = c(2, 3))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)

# Euclidean distance
persp(
  x = margin,
  y = margin,
  z = matrix(distance, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "",
  theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
  main = "Euclidean Distance"
)
# DTM
persp(
  x = margin,
  y = margin,
  z = matrix(DTM, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "",
  theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
  main = "DTM"
)
# kNN
persp(
  x = margin,
  y = margin,
  z = matrix(kNN, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "",
  theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
  main = "kNN DE"
)
# KDE
persp(
  x = margin,
  y = margin,
  z = matrix(KDE, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "",
  theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
  main = "Gaus KDE"
)
# kDist
persp(
  x = margin,
  y = margin,
  z = matrix(Kdist, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "",
  theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
  main = "Kdist"
)

# Persistent Homology
# Over a grid
# `gridDiag` computes the persistent homology of sublevel sets of functions
# Evaluates a real valued function over a triangulated grid
# Constructs a filtration of simplices using the values of the function
# Computes the persistent homology of the filtration

# Persistent Homology of the superlevel sets of the KDE
DiagGrid <- gridDiag(
  X = X, FUN = kde, lim = cbind(lim, lim), by = by,
  sublevel = FALSE, library = "Dionysus", printProgress = TRUE, h = 0.3
)

# Plot persistance diagram for KDE
par(mfrow = c(1, 3))
plot(X, main = "Sample X", pch = 20)
persp(
  x = margin, y = margin, z = matrix(KDE, nrow = length(margin), ncol = length(margin)),
  xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
  expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.9, main = "KDE"
)
plot(x = DiagGrid[["diagram"]], main = "KDE Persistence")

# Rips Persistent Homology
DiagRips <- ripsDiag(
  X = X,
  maxdimension = 1,
  maxscale = 0.5,
  library = c("GUDHI", "Dionysus"),
  location = TRUE
)

# plotting persistence
par(mfrow = c(1, 2))
plot(X, xlab = "", ylab = "", main = "Sample X", pch = 20)
plot(x = DiagRips[["diagram"]], main = "Rips Diagram")
