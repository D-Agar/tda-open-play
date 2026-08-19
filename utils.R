sourceDir <- function(path, exclude = NULL, trace = TRUE, ...) {
  op <- options()
  on.exit(options(op)) # to reset after each
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if (!is.null(exclude) & nm %in% exclude) next
    if (trace) cat(nm, ":\n")
    source(file.path(path, nm), ...)
    if (trace) cat("-------------\n")
    options(op)
  }
}

find_mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

# show a table in the web browser for better viewing
show_table <- function(data, ...) {
  temp_html <- tempfile(pattern = "dt_", fileext = ".html")
  htmlwidgets::saveWidget(
    widget = DT::datatable(data, ...),
    file = temp_html,
    selfcontained = TRUE
  )
  utils::browseURL(temp_html)
}
