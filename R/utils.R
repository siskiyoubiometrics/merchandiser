.mc_scale_rules <- c(
  "scribner_decimal_c_split_20",
  "scribner_decimal_c_allocated_20",
  "scribner_decimal_c_whole_40",
  "scribner_factor_split_20",
  "scribner_factor_allocated_20",
  "scribner_factor_whole_40",
  "international_1_4_4ft",
  "doyle_formula",
  "smalian",
  "huber",
  "cubic"
)

.mc_rounding_operators <- c(
  "rule_default", "none", "truncate_1in", "nearest_1in_half_up",
  "nearest_0.5in_half_up", "truncate_1cm", "nearest_1cm_half_up",
  "truncate_1ft", "nearest_1ft_half_up", "truncate_0.1m",
  "nearest_0.1m_half_up", "truncate_board_foot",
  "nearest_board_foot_half_up", "nearest_10_board_feet_half_up"
)

.mc_status_table <- data.frame(
  code = c(400L:404L, 406L:413L),
  name = c(
    "no_entry_product", "defect_height_out_of_range",
    "defect_interval_invalid", "defect_percent_out_of_range",
    "model_unresolved",
    "unknown_defect_effect", "unknown_curvature_category",
    "missing_product_attribute", "utilization_top_invalid",
    "no_feasible_log", "scale_input_unavailable",
    "profile_adapter_failure", "reconciliation_failure"
  ),
  category = c(
    "product", rep("defect", 3L), "model", "defect", "defect", "product",
    "utilization", "product", "scaling", "provider", "reconciliation"
  ),
  description = c(
    "no product passes the static entry rules",
    "a defect height is outside the physical tree",
    "a defect interval has invalid or reversed bounds",
    "a defect percentage is outside zero through 100",
    "no taper equation could be resolved for the species",
    "the defect effect is not in the supported vocabulary",
    "a sweep or crook category is not in curvature_scale",
    "a product rule needs a tree attribute that was not supplied",
    "the utilization-top request is invalid for this tree",
    "the tree is valid but no product log is feasible",
    "a requested product scale cannot be computed from available inputs",
    "the stem profile adapter failed for this tree",
    "the physical volume ledger did not reconcile"
  ),
  stringsAsFactors = FALSE
)

.mc_numeric <- function(x) {
  is.integer(x) || is.double(x)
}

.mc_as_double <- function(x, name) {
  if (!.mc_numeric(x)) {
    stop(
      name, " must be numeric. Supply integer or decimal values and try again.",
      call. = FALSE
    )
  }
  as.double(x)
}

.mc_as_integer <- function(x, name, allow_na = TRUE) {
  if (!.mc_numeric(x)) {
    stop(
      name, " must contain whole numbers. Supply integer-valued numbers and try again.",
      call. = FALSE
    )
  }
  valid <- is.na(x) & allow_na |
    is.finite(x) & x == floor(x) & x >= -.Machine$integer.max &
      x <= .Machine$integer.max
  if (any(!valid)) {
    stop(
      name, " must contain whole numbers. Replace fractional or nonfinite values and try again.",
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
    stop(
      name, " must contain TRUE/FALSE or 0/1 values. Replace other values and try again.",
      call. = FALSE
    )
  }
  as.logical(x)
}

.mc_normalize_species_text <- function(x) {
  trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
}

.mc_fia_aliases <- local({
  aliases <- NULL
  function() {
    if (!is.null(aliases)) return(aliases)
    path <- system.file(
      "extdata", "fia_species_aliases.csv", package = "merchandiser"
    )
    if (!nzchar(path)) return(NULL)
    aliases <<- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE
    )
    aliases
  }
})

.mc_species_from_character <- function(x) {
  result <- rep(NA_integer_, length(x))
  numeric_text <- !is.na(x) & grepl(
    "^[1-9][0-9]*(\\.0+)?$", trimws(x)
  )
  if (any(numeric_text)) {
    codes <- .mc_as_integer(
      as.double(trimws(x[numeric_text])), "species"
    )
    result[numeric_text] <- species_lookup(
      codes, from = "spcd", to = "spcd"
    )
  }
  pending <- which(!is.na(x) & !numeric_text)
  for (from in c("symbol", "common", "scientific")) {
    if (!length(pending)) break
    found <- tryCatch(
      species_lookup(
        as.character(x[pending]), from = from, to = "spcd"
      ),
      error = function(condition) condition
    )
    if (inherits(found, "condition")) {
      stop(
        conditionMessage(found),
        " Use an unambiguous FIA species code, symbol, or common name and try again.",
        call. = FALSE
      )
    }
    matched <- !is.na(found)
    result[pending[matched]] <- as.integer(found[matched])
    pending <- pending[!matched]
  }
  if (length(pending)) {
    aliases <- .mc_fia_aliases()
    if (!is.null(aliases)) {
      wanted <- .mc_normalize_species_text(as.character(x[pending]))
      alias_columns <- c("symbol", "common", "scientific")
      for (index in seq_along(wanted)) {
        matched <- Reduce(`|`, lapply(
          aliases[alias_columns],
          function(column) .mc_normalize_species_text(column) == wanted[index]
        ))
        matches <- unique(aliases$spcd[matched])
        matches <- matches[!is.na(matches)]
        if (length(matches) > 1L) {
          stop(
            "Species '", x[pending[index]], "' is ambiguous. Use one of the FIA codes ",
            paste(matches, collapse = ", "), " and try again.", call. = FALSE
          )
        }
        if (length(matches) == 1L) result[pending[index]] <- matches
      }
    }
  }
  result
}

.mc_resolve_species <- function(x, name = "species", allow_na = TRUE) {
  if (is.factor(x)) x <- as.character(x)
  result <- if (.mc_numeric(x)) {
    values <- .mc_as_integer(x, name, allow_na = allow_na)
    known <- species_lookup(values, from = "spcd", to = "spcd")
    as.integer(known)
  } else if (is.character(x)) {
    .mc_species_from_character(x)
  } else {
    stop(
      name, " must contain FIA codes, symbols, or common names. ",
      "Supply a numeric or text species vector and try again.",
      call. = FALSE
    )
  }
  if (!allow_na && anyNA(result)) {
    stop(
      name, " contains a missing or unrecognized species. ",
      "Check the spelling or supply its FIA code and try again.",
      call. = FALSE
    )
  }
  unknown <- !is.na(x) & is.na(result)
  if (any(unknown)) {
    stop(
      "Species '", as.character(x[which(unknown)[1L]]),
      "' was not recognized. Check the spelling or supply its FIA code ",
      "and try again.",
      call. = FALSE
    )
  }
  result
}

.mc_scalar_choice <- function(x, name, choices) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "),
         ". Choose one of these values and try again.",
         call. = FALSE)
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
    stop(
      "Only size-one inputs recycle and all other inputs must have one ",
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
  stop(
    name, " must have size one or the common size. ",
    "Use one value or a vector aligned with the other inputs.", call. = FALSE
  )
}

.mc_require_atomic_id <- function(id, name = "id") {
  if (!is.atomic(id) || is.null(id) || anyNA(id)) {
    stop(
      name, " must be an atomic vector without missing values. ",
      "Supply a nonmissing identifier for every tree.", call. = FALSE
    )
  }
  id
}

.mc_q <- function(units) {
  if (identical(units, "imperial")) 1 / 12 else 0.0254
}

.mc_ticks <- function(x, q) {
  floor(x / q + 0.5)
}

.mc_untick <- function(x, q) {
  x * q
}

.mc_empty_id <- function(id) {
  id[FALSE]
}

.mc_empty_assumptions <- function() {
  data.frame(
    assumption = character(), spcd = integer(), species = character(),
    model = character(), value = double(), units = character(),
    basis = character(), product = character(), preset = character(),
    source = character(), region = integer(), forest = integer(),
    district = integer(), stringsAsFactors = FALSE
  )
}

.mc_assumption_rows <- function(assumption, size = length(assumption),
                                spcd = NA_integer_, species = NA_character_,
                                model = NA_character_, value = NA_real_,
                                units = NA_character_, basis = NA_character_,
                                product = NA_character_, preset = NA_character_,
                                source = NA_character_, region = NA_integer_,
                                forest = NA_integer_, district = NA_integer_) {
  if (!size) return(.mc_empty_assumptions())
  data.frame(
    assumption = rep_len(as.character(assumption), size),
    spcd = rep_len(as.integer(spcd), size),
    species = rep_len(as.character(species), size),
    model = rep_len(as.character(model), size),
    value = rep_len(as.double(value), size),
    units = rep_len(as.character(units), size),
    basis = rep_len(as.character(basis), size),
    product = rep_len(as.character(product), size),
    preset = rep_len(as.character(preset), size),
    source = rep_len(as.character(source), size),
    region = rep_len(as.integer(region), size),
    forest = rep_len(as.integer(forest), size),
    district = rep_len(as.integer(district), size),
    stringsAsFactors = FALSE
  )
}

.mc_bind_assumptions <- function(...) {
  rows <- list(...)
  rows <- Filter(function(x) !is.null(x) && nrow(x), rows)
  if (!length(rows)) return(.mc_empty_assumptions())
  result <- unique(do.call(rbind, rows))
  rownames(result) <- NULL
  result
}

.mc_species_names <- function(spcd) {
  if (is.null(spcd)) return(character())
  result <- rep(NA_character_, length(spcd))
  for (field in c("common", "scientific")) {
    pending <- which(is.na(result) & !is.na(spcd))
    if (!length(pending)) break
    found <- tryCatch(
      species_lookup(spcd[pending], from = "spcd", to = field),
      error = function(condition) rep(NA_character_, length(pending))
    )
    matched <- !is.na(found) & nzchar(found)
    result[pending[matched]] <- as.character(found[matched])
  }
  result
}

.mc_hash <- function(x) {
  raw <- serialize(x, NULL, version = 3L)
  sprintf("%08x", mc_hash_raw_cpp(raw))
}

.mc_model_hash <- function(x) {
  sprintf("%08x", mc_hash_character_cpp(x))
}

.mc_products_hash <- function(x) {
  attr(x, "raw_products") <- NULL
  attr(x, "normalized_products") <- NULL
  for (name in c("measurement_fields", "source_policies", "measurement_aliases",
                 "parity_input", "parity_view",
                 "calculation_products", "legacy_units")) attr(x, name) <- NULL
  attributes(x) <- attributes(x)[sort(names(attributes(x)))]
  sprintf("%08x", mc_hash_raw_cpp(serialize(x, NULL, version = 2L)))
}

.mc_sum_defined <- function(x) {
  if (!length(x)) return(0)
  if (any(!is.finite(x))) return(NA_real_)
  sum(x)
}

.mc_sum_by_index <- function(x, index, size, require_defined = FALSE) {
  if (length(x) != length(index)) {
    stop("Internal aggregation inputs have different sizes.", call. = FALSE)
  }
  result <- numeric(size)
  if (!length(x)) return(result)
  if (anyNA(index) || any(index < 1L | index > size)) {
    stop("Internal aggregation index is out of range.", call. = FALSE)
  }
  undefined <- if (require_defined) !is.finite(x) else is.na(x)
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
  unname(names[as.character(code)])
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
      warning(
        function_name, "(): model_unresolved [404] for ",
        sum(status == cause, na.rm = TRUE), " stem(s). Species code(s): ",
        paste(code_text, collapse = ", "),
        ". Supply model or taper_map and try again.", call. = FALSE
      )
      next
    }
    warning(
      function_name, "(): ", .mc_status_name(cause), " [", cause, "] for ",
      sum(status == cause, na.rm = TRUE),
      " stem(s). Call status_codes() to see what to change.",
      call. = FALSE
    )
  }
}

#' Interpret tree calculation and volume codes
#'
#' Return calculation codes, names, meanings, and sources for stem and merchandising results.
#'
#' @return A data frame with integer `code`, character `name`, character
#'   `category`, character `description`, and character `source`. All fields
#'   are unitless. Code identifies the diagnosis, name gives its stable label,
#'   category groups related problems, description explains the code, and
#'   source identifies the calculation group.
#'
#' No values are user defaults.
#' @details Row codes describe individual calculations. Invalid argument
#'   structure can stop a call before row codes are assigned. The literal
#'   descriptions returned by this lookup retain some development wording.
#'
#' @inheritSection stem_volume Status and missing values
#' @inheritSection merchandise Status and missing values
#' @seealso [merchandise()] for tree and log diagnoses, [dib()] for diameter
#'   diagnoses, [stand_table()] for separating failed trees in expanded totals.
#' @export
#' @usage
#'
#' ## Call signatures
#' status_codes()
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect merchandising diagnoses
#' status_codes() %>%
#'   filter(source == 'merchandising') %>%
#'   slice_head(n = 3)
#' @section Coordinate and source codes:
#' `outside_divisions`, `outside_divisions`, means the coordinate lookup found no shipped
#' ecological
#' division. Its value is missing. Check coordinates and coverage.
#'
#' `library_error_base` is the offset added to positive source error numbers. The calculated
#' quantity is unavailable.
#'
#' Correct the equation's required measurements and geographic selection before repeating the
#' calculation.
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
  result
}
