.tv_status_table <- data.frame(
  code = c(0L, 1:8, 50:54, 100:103, 300L),
  name = c(
    "ok", "na_input", "dbh_nonpositive", "ht_nonpositive", "h_out_of_range",
    "diameter_nonpositive", "empty_bounds", "unknown_species",
    "outside_divisions", "unknown_model",
    "missing_input", "species_out_of_scope", "capability_missing", "kernel_error",
    "above_tip", "below_stump", "not_unique", "no_convergence",
    "library_error_base"
  ),
  category = c(
    "ok", rep("input", 8L), rep("model", 5L), rep("numeric", 4L), "library"
  ),
  description = c(
    "result is defined",
    "a required input is missing or non-finite",
    "dbh is not positive",
    "total height is not positive",
    "height is below zero or above total height",
    "diameter is not positive",
    "the resolved lower bound is not below the upper bound",
    "the species code is not recognized",
    "the coordinates are outside the packaged ecological divisions",
    "the model id is not registered",
    "a required auxiliary input is missing",
    "the species is outside the model scope",
    "the model lacks the requested capability",
    "the model kernel raised a condition or returned an invalid value",
    "the target diameter is smaller than the tip diameter",
    "the target diameter is larger than the diameter at the stump",
    "the profile has another crossing below the returned height",
    "the numerical method did not converge",
    "base offset added to a positive NVEL or NSVB library error flag"
  ),
  stringsAsFactors = FALSE
)

.status_name <- function(code) {
  match <- match(code, .tv_status_table$code)
  ifelse(is.na(match), paste0("library_error_", code - 300L),
    .tv_status_table$name[match]
  )
}

.validate_status <- function(status) {
  if (!is.logical(status) || length(status) != 1L || is.na(status)) {
    stop("status must be TRUE or FALSE.", call. = FALSE)
  }
  status
}

.validate_units <- function(units) {
  .scalar_character(units, "units", c("imperial", "metric"))
}

.status_warning <- function(function_name, status, details = NULL, size = length(status),
                            tree = seq_along(status)) {
  warn_codes <- sort(unique(status[!status %in% c(0L, 1L)]))
  if (!length(warn_codes)) {
    return(invisible(NULL))
  }
  if (is.null(details)) {
    details <- rep(NA_character_, length(status))
  }
  for (code in warn_codes) {
    code_details <- sort(unique(stats::na.omit(details[status == code])))
    count <- length(unique(tree[status == code]))
    retained <- code %in% c(52L, 102L)
    suffix <- if (retained) "" else " (NA returned)"
    extra <- if (length(code_details)) {
      paste0(": ", paste(code_details, collapse = " | "))
    } else {
      ""
    }
    warning(
      function_name, "(): ", .status_name(code), " for ", count, " of ", size,
      " trees", suffix, extra,
      call. = FALSE
    )
  }
  invisible(NULL)
}

.status_result <- function(value, status, include_status, details = NULL,
                           function_name = "merchandiser") {
  if (include_status) {
    return(data.frame(value = value, status = as.integer(status)))
  }
  .status_warning(function_name, status, details)
  value
}
