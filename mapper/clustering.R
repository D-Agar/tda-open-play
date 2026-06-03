cluster_single_linkage <- function(data_subset, height_threshold = NULL) {
  # handle edge cases where clustering isn't possible
  if (nrow(data_subset) <= 1) {
    return(rep(1, nrow(data_subset)))
  }
  sub_dist <- dist(data_subset)
  hc <- hclust(sub_dist, method = "single")
  
  # dynamic threshold
  if (is.null(height_threshold)) {
    # Cut at 70% of the maximum dendrogram height
    height_threshold <- max(hc$height) * 0.70
  }
  return(cutree(hc, h = height_threshold))
}

cluster_hierarchical_ward <- function(data_subset, height_threshold = NULL) {
  # handle edge cases where clustering isn't possible
  if (nrow(data_subset) <= 1) {
    return(rep(1, nrow(data_subset)))
  }
  sub_dist <- dist(data_subset)
  # ward.D2 squares the distances (Ward's criterion)
  hc <- hclust(sub_dist, method = "ward.D2")
  
  # dynamic threshold
  if (is.null(height_threshold)) {
    # if the max tree height is near 0, treat the entire subset as 1 cluster
    if (max(hc$height) < 0.01) {
      return(rep(1, nrow(data_subset)))
    }
    # cut at 70% of the maximum tree height
    height_threshold <- max(hc$height) * 0.70
  }
  return(cutree(hc, h = height_threshold))
}

cluster_gap_heuristic <- function(data_subset, k_bins = NULL) {
  # histogram gap heuristic from Singh et al. 2007
  single_linkage_clustering <- hclust(dist(data_subset), method = "single")

  # merge heights (edge lengths at each step)
  merge_heights <- single_linkage_clustering$height

  # automatic bin choice
  if (is.null(k_bins)) {
    hist_heights <- hist(merge_heights, breaks = "Sturges", plot = FALSE)
  } else {
    hist_heights <- hist(merge_heights, breaks = k_bins, plot = FALSE)
  }
  # get first empty interval
  empty_bins <- which(hist_heights$counts == 0)

  if (length(empty_bins) > 0) {
    # cutoff is the lower bound of first empty bin
    threshold <- hist_heights$breaks[empty_bins[1]]
  } else {
    # no gap found: highest threshold
    threshold <- max(merge_heights)
  }

  # cluster assignments based on dynamic threshold
  return(cutree(single_linkage_clustering, h = threshold))
}
