sourceDir <- function(path, exclude = NULL, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if (!is.null(exclude) & nm %in% exclude) next
    if(trace) cat(nm,":\n")
    source(file.path(path, nm), ...)
    if(trace) cat("-------------\n")
    options(op)
  }
}

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
