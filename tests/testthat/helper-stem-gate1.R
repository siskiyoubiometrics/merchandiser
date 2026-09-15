.gate1_metrics <- new.env(parent = emptyenv())
.gate1_metrics$records <- list()

.with_treevolume_compat <- function(compat, code) {
  old <- options(merchandiser.compat = compat)
  on.exit(options(old), add = TRUE)
  force(code)
}

.gate1_exclusions <- function(masks) {
  if (!length(masks))
    return("none")
  counts <- vapply(masks, function(mask) sum(mask, na.rm = TRUE), numeric(1L))
  counts <- counts[counts > 0]
  if (!length(counts))
    return("none")
  paste(paste(names(counts), format(counts, scientific = FALSE)), collapse = ", ")
}

.gate1_record_values <- function(
  family, output, precision, actual, expected, include, tolerance,
  masks = list(), denominator_floor = 1e-12, compat = "port"
) {
  if (!nzchar(Sys.getenv("TREEVOLUME_GATE1_RESULTS", unset = ""))) {
    return(invisible(NULL))
  }
  include <- include & !is.na(include)
  relative <- abs(actual - expected) / pmax(abs(expected), denominator_floor)
  compared <- sum(include)
  within <- sum(include & is.finite(relative) & relative <= tolerance)
  maximum <- if (compared)
    max(relative[include], na.rm = TRUE) else NA_real_
  .gate1_metrics$records[[length(.gate1_metrics$records) + 1L]] <- data.frame(
    family = family,
    output = output, precision = precision, compat = compat,
    rows_compared = as.numeric(compared),
    rows_within_tolerance = as.numeric(within), max_relative_difference = maximum,
    tolerance = tolerance,
    exclusions = .gate1_exclusions(masks), stringsAsFactors = FALSE
  )
  invisible(NULL)
}

.gate1_record_summary <- function(
  family, output, precision, compared, within, maximum, tolerance,
  masks = list(), compat = "port"
) {
  if (!nzchar(Sys.getenv("TREEVOLUME_GATE1_RESULTS", unset = ""))) {
    return(invisible(NULL))
  }
  .gate1_metrics$records[[length(.gate1_metrics$records) + 1L]] <- data.frame(
    family = family,
    output = output, precision = precision, compat = compat,
    rows_compared = as.numeric(compared),
    rows_within_tolerance = as.numeric(within), max_relative_difference = maximum,
    tolerance = tolerance,
    exclusions = .gate1_exclusions(masks), stringsAsFactors = FALSE
  )
  invisible(NULL)
}

.gate1_write <- function() {
  path <- Sys.getenv("TREEVOLUME_GATE1_RESULTS", unset = "")
  if (!nzchar(path))
    return(invisible(NULL))
  if (!length(.gate1_metrics$records)) {
    stop("no gate 1 oracle metrics were recorded")
  }
  output <- do.call(rbind, .gate1_metrics$records)
  utils::write.csv(output, path, row.names = FALSE, na = "")
  invisible(output)
}
