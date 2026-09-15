.mc_status_table <- data.frame(code = c(400L:405L, 407L:412L), name = c(
  "no_entry_product",
  "defect_height_out_of_range", "defect_interval_invalid",
  "defect_percent_out_of_range", "model_unresolved",
  "defect_unknown_tree", "defect_unknown_product", "invalid_pruned_height",
  "defect_product_misplaced", "no_feasible_log", "invalid_stump_height", "optimizer_failed"
), category = c(
  "product",
  "defect", "defect", "defect", "model", "defect", "defect", "input", "defect",
  "product", "input", "product"
), description = c(
  "No product accepts this tree's recorded attributes.",
  "A defect height lies outside the tree.",
  "A defect section has reversed or equal bounds.",
  "A sweep percentage is missing, misplaced, or outside zero through 100.",
  "No taper equation was selected for this species.",
  "A defect names a tree that is not in the list.",
  "A defect restricts to a product that is not in the products table.",
  "The pruned height is negative, not finite, or above the tree.",
  "A product is named on a defect that is not a restriction.",
  "The tree is valid but no log meets the product specifications.",
  "Stump height must be at least zero and below the tree height.",
  "The value optimization did not complete for this tree."
))

.mc_scale_rules <- c(
  "scribner_decimal_c_split_20", "scribner_decimal_c_allocated_20",
  "scribner_decimal_c_whole_40",
  "scribner_factor_split_20", "scribner_factor_allocated_20",
  "scribner_factor_whole_40", "international_1_4_4ft",
  "doyle_formula", "smalian", "huber", "cubic"
)

.mc_rounding_operators <- c(
  "rule_default", "none", "truncate_1in", "nearest_1in_half_up", "nearest_0.5in_half_up",
  "truncate_1cm", "nearest_1cm_half_up", "truncate_1ft", "nearest_1ft_half_up",
  "truncate_0.1m",
  "nearest_0.1m_half_up", "truncate_board_foot", "nearest_board_foot_half_up",
  "nearest_10_board_feet_half_up"
)

.mc_numeric <- function(x) {
  is.integer(x) || is.double(x)
}

.mc_as_double <- function(x, name) {
  if (!.mc_numeric(x)) {
    stop(name, " must be numeric. Supply integer or decimal values and try again.",
      call. = FALSE
    )
  }
  as.double(x)
}

.mc_as_integer <- function(x, name, allow_na = TRUE) {
  if (!.mc_numeric(x)) {
    stop(name, " must contain whole numbers. Supply integer-valued numbers and try again.",
      call. = FALSE
    )
  }
  valid <- is.na(x) & allow_na | is.finite(x) & x == floor(x) & x >= -.Machine$integer.max &
    x <= .Machine$integer.max
  if (any(!valid)) {
    stop(name,
      " must contain whole numbers. Replace fractional or nonfinite values and try again.",
      call. = FALSE
    )
  }
  as.integer(x)
}

.mc_as_logical <- function(x, name, allow_na = TRUE) {
  if (is.logical(x)) {
    valid <- allow_na || !anyNA(x)
  } else if (.mc_numeric(x)) {
    valid <- all((x %in% c(0, 1)) | (allow_na & is.na(x)))
  } else {
    valid <- FALSE
  }
  if (!valid) {
    stop(name,
      " must contain TRUE/FALSE or 0/1 values. Replace other values and try again.",
      call. = FALSE
    )
  }
  as.logical(x)
}

.mc_scalar_choice <- function(x, name, choices) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "),
      ". Choose one of these values and try again.",
      call. = FALSE
    )
  }
  x
}

.mc_common_size <- function(values) {
  lengths <- vapply(values, length, integer(1L))
  nonempty <- lengths[lengths > 0L]
  if (!length(nonempty)) {
    return(0L)
  }
  common <- max(nonempty)
  invalid <- nonempty != 1L & nonempty != common
  if (any(invalid)) {
    stop("Only size-one inputs recycle and all other inputs must have one ",
      "common size. Use one value or vectors with matching lengths and ",
      "try again.",
      call. = FALSE
    )
  }
  common
}

.mc_recycle <- function(x, size, name) {
  if (!size) {
    return(x[integer()])
  }
  if (length(x) == size) {
    return(x)
  }
  if (length(x) == 1L) {
    return(rep(x, size))
  }
  stop(name, " must have size one or the common size. ",
    "Use one value or a vector aligned with the other inputs.",
    call. = FALSE
  )
}

.mc_require_atomic_id <- function(id, name = "id") {
  if (!is.atomic(id) || is.null(id) || anyNA(id)) {
    stop(name, " must be an atomic vector without missing values. ",
      "Supply a nonmissing identifier for every tree.",
      call. = FALSE
    )
  }
  id
}

.mc_empty_id <- function(id) {
  id[FALSE]
}

.mc_hash <- function(x) {
  raw <- serialize(x, NULL, version = 3L)
  sprintf("%08x", mc_hash_raw_cpp(raw))
}

.mc_model_hash <- function(x) {
  sprintf("%08x", mc_hash_character_cpp(x))
}

.mc_sum_defined <- function(x) {
  if (!length(x))
    return(0)
  if (any(!is.finite(x)))
    return(NA_real_)
  sum(x)
}

.mc_sum_by_index <- function(x, index, size, require_defined = FALSE) {
  if (length(x) != length(index)) {
    stop("Internal aggregation inputs have different sizes.", call. = FALSE)
  }
  result <- numeric(size)
  if (!length(x))
    return(result)
  if (anyNA(index) || any(index < 1L | index > size)) {
    stop("Internal aggregation index is out of range.", call. = FALSE)
  }
  undefined <- if (require_defined)
    !is.finite(x) else is.na(x)
  finite_values <- x
  finite_values[undefined] <- 0
  sums <- rowsum(finite_values, index, reorder = FALSE)
  result[as.integer(rownames(sums))] <- sums[, 1L]
  if (require_defined && any(undefined)) {
    result[unique(index[undefined])] <- NA_real_
  }
  result
}

.mc_bind_rows <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x), xs)
  if (!length(xs)) {
    return(NULL)
  }
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(columns, names(x))
    for (name in missing) {
      x[[name]] <- NA
    }
    x[columns]
  })
  result <- do.call(rbind, xs)
  rownames(result) <- NULL
  result
}

.mc_status_name <- function(code) {
  tree <- .tv_status_table
  names <- stats::setNames(as.character(tree$name), tree$code)
  names[as.character(.mc_status_table$code)] <- .mc_status_table$name
  result <- unname(names[as.character(code)])
  result[is.na(result)] <- "unknown_status"
  result
}

.mc_status_warning <- function(function_name, status, spcd = NULL) {
  causes <- sort(unique(status[!is.na(status) & status != 0L]))
  for (cause in causes) {
    if (cause == 404L) {
      species <- if (is.null(spcd)) {
        rep(NA_integer_, length(status))
      } else {
        spcd
      }
      codes <- unique(species[status == cause])
      code_text <- ifelse(is.na(codes), "missing", as.character(codes))
      warning(function_name, "(): model_unresolved [404] for ", sum(status == cause,
          na.rm = TRUE
        ),
        " stem(s). Species code(s): ", paste(code_text, collapse = ", "),
        ". Supply model or taper_map and try again.",
        call. = FALSE
      )
      next
    }
    warning(function_name, "(): ", .mc_status_name(cause), " [", cause, "] for ",
      sum(status ==
            cause, na.rm = TRUE), " stem(s). Call status_codes() to see what to change.",
      call. = FALSE
    )
  }
}

#' Interpret tree calculation and volume codes
#'
#' @return A data frame with these columns:
#'   * `status`: stable integer status code, unitless.
#'   * `name`: plain problem name.
#'   * `description`: result description.
#'   * `category`: problem group.
#'   * `source`: stem model or merchandising.
#'
#'   Columns other than `status` contain text.
#' @usage
#' status_codes()
#' @export
#' @examples
#' ## Inspect calculation status codes
#' codes <- status_codes()
#'
#' ## Show input status descriptions
#' head(codes, n = 3)
status_codes <- function() {
  tree <- .tv_status_table
  tree$source <- "stem model"
  merch <- .mc_status_table
  merch$source <- "merchandising"
  names <- union(names(tree), names(merch))
  for (name in setdiff(names, names(tree))) tree[[name]] <- NA_character_
  for (name in setdiff(names, names(merch))) merch[[name]] <- NA_character_
  result <- rbind(tree[names], merch[names])
  result <- result[order(result$code), , drop = FALSE]
  rownames(result) <- NULL
  names(result)[names(result) == "code"] <- "status"
  result
}
