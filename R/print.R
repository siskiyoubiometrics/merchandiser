.mc_print_rows <- function(x, n = 10L) {
  shown <- utils::head(x, n)
  print.data.frame(shown, row.names = FALSE)
  if (nrow(x) > nrow(shown))
    cat("... ", nrow(x) - nrow(shown), " more rows\n", sep = "")
  invisible(x)
}

#' @export
print.merch_products <- function(x, ..., n = 10L) {
  cat("<products> ", nrow(x), " products in cutting priority order\n", sep = "")
  .mc_print_rows(as.data.frame(x), n)
  invisible(x)
}

#' @export
print.merch_defects <- function(x, ..., n = 10L) {
  cat("<merch_defects> ", nrow(x), " records\n", sep = "")
  .mc_print_rows(as.data.frame(x), n)
  invisible(x)
}

#' @export
print.merch_result <- function(x, ...) {
  cat("<merch_result> ", x$call$strategy, ": ", length(x$call$tree_id), " trees\n",
    sep = ""
  )
  cat(nrow(x$logs), " logs, ", nrow(x$residuals), " residual pieces, ", nrow(
    x$status
  ), " trees with status, ",
  nrow(x$assumptions), " recorded assumptions\n",
  sep = ""
  )
  if (nrow(x$logs)) {
    for (product in unique(x$logs$product)) {
      selected <- x$logs$product == product
      cat(product, ": ", sum(x$logs$scale[selected]), " ", x$logs$volume_unit[which(
        selected
      )[1]],
      "\n",
      sep = ""
      )
    }
  }
  if (nrow(x$status))
    .mc_print_rows(x$status)
  invisible(x)
}
