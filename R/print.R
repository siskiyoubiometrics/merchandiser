.mc_print_rows <- function(x, n = 10L) {
  shown <- utils::head(x, n)
  print.data.frame(shown, row.names = FALSE)
  if (nrow(x) > nrow(shown)) {
    cat("... ", nrow(x) - nrow(shown), " more row(s)\n", sep = "")
  }
  invisible(x)
}

#' @export
print.merch_products <- function(x, ..., n = 10L) {
  cat("<products> ", nrow(x), " product(s)\n", sep = "")
  if (!nrow(x)) return(invisible(x))
  display_fields <- c(
    "product", "priority", "species", "lengths", "min_length",
    "max_length", "length_step", "min_sed", "volume_unit", "inside_bark", "split_scale", "round"
  )
  if (!all(display_fields %in% names(x))) {
    .mc_print_rows(as.data.frame(x), n)
    return(invisible(x))
  }
  lengths <- vapply(seq_len(nrow(x)), function(row) {
    value <- x$lengths[[row]]
    if (!is.null(value)) return(paste(value, collapse = ", "))
    upper <- if (is.na(x$max_length[row])) "top" else x$max_length[row]
    paste0(x$min_length[row], " to ", upper, " by ", x$length_step[row])
  }, character(1L))
  shown <- data.frame(
    product = x$product, priority = x$priority, species = x$species,
    lengths = lengths, min_sed = x$min_sed,
    volume_unit = x$volume_unit, inside_bark = x$inside_bark,
    split_scale = x$split_scale, round = x$round,
    stringsAsFactors = FALSE
  )
  if ("specification_source" %in% names(x) &&
        any(grepl("^illustrative_", x$specification_source))) {
    cat("Illustrative preset, not a sourced specification\n")
  }
  .mc_print_rows(shown, n)
  invisible(x)
}

#' @export
print.merch_defects <- function(x, ..., n = 10L) {
  cat("<merch_defects> ", nrow(x), " defect(s)\n", sep = "")
  if (!nrow(x)) return(invisible(x))
  columns <- intersect(
    c("id", "defect_id", "effect", "from", "to", "percent", "category", "status"),
    names(x)
  )
  .mc_print_rows(as.data.frame(x[columns]), n)
  invisible(x)
}

#' @export
print.merch_result <- function(x, ...) {
  algorithm <- x$run_metadata$algorithm
  algorithm <- switch(algorithm, cascade = "bucked", dp = "optimized", algorithm)
  units <- x$run_metadata$units
  volume_unit <- if (identical(units, "metric")) "m3" else "ft3"
  cat(
    "<merch_result> ", algorithm, ": ", nrow(x$trees), " tree(s), ",
    nrow(x$logs), " log(s)\n", sep = ""
  )
  recorded <- x$run_metadata$assumptions
  if (!is.null(recorded)) {
    mapping <- recorded[recorded$assumption == "species_model", c(
      "spcd", "species", "model", "source"
    ), drop = FALSE]
    if (nrow(mapping)) {
      cat("Species-to-equation assumptions:\n")
      .mc_print_rows(mapping)
    }
    cat(
      "Other recorded assumptions: ",
      sum(recorded$assumption != "species_model"), "\n", sep = ""
    )
  }
  if (!nrow(x$trees)) return(invisible(x))
  has_status <- "status" %in% names(x$trees)
  status <- if (has_status) x$trees$status else rep(0L, nrow(x$trees))
  if (has_status) {
    cat(
      "Valid trees: ", sum(status %in% c(0L, 410L)),
      "   Failed trees: ", sum(!status %in% c(0L, 410L)),
      "   Zero-log trees: ", sum(status == 410L), "\n", sep = ""
    )
  } else {
    cat("Status detail omitted. Rerun with status = TRUE for counts.\n")
  }
  volume <- sum(
    x$trees$log_net_cubic_ib[status %in% c(0L, 410L)], na.rm = TRUE
  )
  cat("Net inside-bark product volume: ", format(volume, digits = 7L),
      " ", volume_unit, "\n", sep = "")
  if (nrow(x$logs)) {
    products <- unique(x$logs$product)
    cat("Products: ", paste(products, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.bucking_price_comparison <- function(x, ...) {
  cat("<bucking_price_comparison> ", nrow(x), " scenario(s)\n", sep = "")
  .mc_print_rows(as.data.frame(x), nrow(x))
  invisible(x)
}
