#' @export
plot.stem_profile <- function(x, tree_id = NULL, ...) {
  ids <- if (is.null(tree_id))
    head(unique(x$tree_id), 1) else tree_id
  if (anyNA(ids) || any(!ids %in% x$tree_id))
    stop("Unknown tree_id.", call. = FALSE)
  for (id in ids) {
    data <- x[x$tree_id == id, ]
    finite <- is.finite(data$h) & is.finite(data$dib)
    if (sum(finite) < 2) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Stem profile unavailable. Review status.")
      next
    }
    defaults <- list(
      x = data$dib, y = data$h, type = "l", lwd = 2, xlab = "Diameter (inches)",
      ylab = "Height above ground (feet)", main = paste("Tree", id)
    )
    do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
    graphics::lines(data$dob, data$h, lty = 2)
    graphics::legend("topright", c("Inside bark", "Outside bark"), lty = c(1, 2), bty = "n")
  }
  invisible(x)
}

#' @export
plot.height_fit <- function(x, ...) {
  for (spcd in unique(x$data$spcd)) {
    data <- x$data[x$data$spcd == spcd, ]
    defaults <- list(
      x = data$dbh, y = data$ht, pch = 16, xlab = "Diameter at breast height (inches)",
      ylab = "Measured height (feet)", main = paste("Species", spcd)
    )
    do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
    grid <- seq(min(data$dbh), max(data$dbh), length.out = 100)
    graphics::lines(grid, predict_height(x, grid, spcd, re_form = "population"), lwd = 2)
  }
  invisible(x)
}
