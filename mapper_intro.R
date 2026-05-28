# Noisy circle example
n_samples <- 1000
theta <- seq(from = 0, to = 2 * pi, length.out = n_samples)
noise_x <- rnorm(n_samples, mean = 0, sd = 0.1)
noise_y <- rnorm(n_samples, mean = 0, sd = 0.1)
x <- cos(theta) + noise_x
y <- sin(theta) + noise_y
circle <- cbind(x, y)

# lens function: dimension reduction or some stratification
# here, we reduce to the x coordinate
lens <- circle[, 1]

# cover: family of subsets whose union is all of X
# fixed-width interval cover
overlap_fraction <- 0.5
# cover interval width
d <- diff(range(lens)) * overlap_fraction
# cover interval centres
c <- seq(from = min(lens), to = max(lens), by = d / 2)
# create cover intervals
cover_intervals <- cbind(start_point = c - d / 2, end_point = c + d / 2)

# create the pullback cover sets
pullback <- apply(cover_intervals, 1, function(interval) which(interval[1] <= lens & lens < interval[2]))
names(pullback) <- paste0(seq(pullback), ":")
print("Cover intervals:")
print(lapply(pullback, head))

# cluster data based on the pullback cover sets
pullback_clustering <- function(cover) {
  clustering <- hclust(dist(circle[cover, , drop = FALSE]), method = "single")
  clusters <- cutree(clustering, h = 0.4)
  lapply(unique(clusters), function(v_ij) cover[which(clusters == v_ij)])
}
vertices <- unlist(lapply(pullback, pullback_clustering), recursive = FALSE)
print(lapply(vertices, head))

# matrix of pairwise overlaps
b <- matrix(NA, nrow = 0, ncol = 2)
for (ij in seq(length(vertices) - 1)) {
  i <- as.integer(gsub(":.*$", "", names(vertices)[[ij]]))
  i1js <- grep(paste0("^", i + 1, ":"), names(vertices))
  for (i1j in i1js) {
    if (length(intersect(vertices[[ij]], vertices[[i1j]])) > 0) {
      b <- rbind(b, c(ij, i1j))
    }
  }
}
print(b)

# Visualise the mapper
plot.new()
plot.window(c(-1.5, 1.5), c(-1.5, 1.5), asp = 1)
# point cloud
points(circle, pch = 19, cex = .5)
# lens
lines(x = c(-2, 2), y = c(0, 0), lty = 1)
rug(lens, pos = 0)
# cover
u_cols <- RColorBrewer::brewer.pal(nrow(cover_intervals), "Set1")
segments(
  x0 = cover_intervals[, 1] + .015, x1 = cover_intervals[, 2] - .015,
  y0 = c(-.1, -.2), col = u_cols, lwd = 3
)
l_nudge <- rep_len(c(TRUE, FALSE), length.out = nrow(cover_intervals))
# pullback cover
rect(
  xleft = cover_intervals[, 1] + .015, xright = cover_intervals[, 2] - .015,
  ybottom = .15 + .2 * l_nudge, ytop = 2.65 + .2 * l_nudge,
  col = paste0(u_cols, "77"), border = NA
)
# nerve
n_lay <- t(sapply(seq_along(vertices), function(i) {
  apply(circle[vertices[[i]], , drop = FALSE], 2, mean)
}))
for (i in seq(nrow(b))) lines(x = n_lay[b[i, ], 1], y = n_lay[b[i, ], 2])
points(
  x = n_lay[, 1], y = n_lay[, 2],
  pch = 21, cex = 2, lwd = 2, bg = "white",
  col = u_cols[as.integer(gsub("^([0-9]+)\\:.*$", "\\1", names(vertices)))]
)
