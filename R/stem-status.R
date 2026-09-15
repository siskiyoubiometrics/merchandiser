.tv_status_table <- data.frame(
  code = c(0L, 1:8, 50:55, 100:102), name = c(
    "ok", "missing_input",
    "invalid_dbh", "invalid_height", "height_out_of_range", "invalid_diameter",
    "empty_stem_section",
    "unknown_species", "outside_divisions", "unknown_model",
    "missing_tree_measurement", "species_out_of_scope",
    "measurement_unavailable", "model_failed", "auxiliary_out_of_domain",
    "diameter_above_tip", "diameter_below_stump",
    "multiple_heights"
  ), category = c("ok", rep("input", 8), rep("model", 6), rep("height", 3)),
  description = c(
    "The requested measurement is available.",
    "A required input is missing or is not finite.",
    "Diameter at breast height must be greater than zero and at most 400 inches.",
    "Total tree height must be greater than zero and at most 500 feet.",
    "The measurement height lies below ground or above the tree top.",
    "The requested diameter must be greater than zero.",
    "The lower boundary must be below the upper boundary.",
    "The species code is not recognized.",
    "The location falls outside the shipped ecological division boundaries.",
    "The taper model identifier is not registered.",
    "The selected model needs an additional tree measurement.",
    "The tree species is outside the model's stated species range.",
    "The selected model cannot supply this measurement.",
    "The selected model did not return a valid measurement.",
    "An auxiliary measurement is outside the model's domain.",
    "The requested diameter is smaller than the diameter at the tree top.",
    "The requested diameter is larger than the diameter at the stump.",
    "The same diameter occurs at more than one height and the highest was returned."
  ), stringsAsFactors = FALSE
)

.status_name <- function(code) {
  match <- match(code, .tv_status_table$code)
  ifelse(is.na(match), "unknown_status", .tv_status_table$name[match])
}

.validate_status <- function(status) {
  if (!is.logical(status) || length(status) != 1L || is.na(status)) {
    stop("status must be TRUE or FALSE.", call. = FALSE)
  }
  status
}

.validate_units <- function(measurement_system) {
  .scalar_character(measurement_system, "measurement_system", c("imperial", "metric"))
}

.status_warning <- function(function_name, status, details = NULL, size = length(
  status
), tree = seq_along(status)) {
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
    suffix <- if (retained)
      "" else " (NA returned)"
    extra <- if (length(code_details)) {
      paste0(": ", paste(code_details, collapse = " | "))
    } else {
      ""
    }
    warning(function_name, "(): ", .status_name(code), " for ", count, " of ", size,
      " trees",
      suffix, extra,
      call. = FALSE
    )
  }
  invisible(NULL)
}

.status_result <- function(
  value, status, include_status, details = NULL,
  function_name = "merchandiser"
) {
  if (include_status) {
    return(data.frame(value = value, status = as.integer(status)))
  }
  .status_warning(function_name, status, details)
  value
}

.public_status <- function(x) {
  known <- c(.tv_status_table$code, .mc_status_table$code)
  x$status[!x$status %in% known] <- 54L
  x
}
