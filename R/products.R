.mc_product_defaults <- function() {
  list(
    species = "all", min_dbh = 0, max_dbh = NA_real_, min_age = NA_real_,
    max_age = NA_real_, pruned = NA, fallback = TRUE, pulp_product = FALSE,
    grade = NULL, lengths = NULL, min_length = NA_real_, max_length = NA_real_,
    length_step = NA_real_, length_parity = "any", min_boundary_length = NA_real_,
    trim = 0, min_sed = NULL, max_sed = NA_real_, min_led = 0,
    max_led = NA_real_, diameter_basis = NULL, max_sweep = NA_character_,
    max_crook = NA_character_, max_defect_pct = NA_real_, max_logs_per_segment = NA_integer_,
    allow_lower_products = TRUE, segmentation_policy = "generic", scale_rule = NULL,
    measurement_quantity = NULL, scale_unit = NULL, scale_bark_basis = NULL,
    cord_solid_fraction = NA_real_, diameter_round = "rule_default",
    length_round = "rule_default", volume_round = "rule_default",
    price = NA_real_, price_quantity = 1, source_id = NA_character_
  )
}

.mc_product_columns <- function() {
  c("product", "priority", names(.mc_product_defaults()))
}

.mc_plain_product_frame <- function(x) {
  attributes(x) <- attributes(x)[c("names", "row.names", "class")]
  class(x) <- "data.frame"
  x
}

.mc_product_raw_input <- function(x) {
  raw <- attr(x, "raw_products", exact = TRUE)
  normalized <- attr(x, "normalized_products", exact = TRUE)
  if (is.null(raw) || is.null(normalized)) return(x)
  current <- .mc_plain_product_frame(x)
  row_match <- match(current$product, raw$product)
  unmatched <- is.na(row_match)
  row_match[unmatched] <- match(current$priority[unmatched], raw$priority)
  source_raw <- raw
  source_normalized <- normalized
  raw <- current
  normalized <- current
  matched <- which(!is.na(row_match))
  for (name in intersect(names(current), names(source_raw))) {
    if (is.list(current[[name]])) {
      for (row in matched) {
        raw[[name]][row] <- source_raw[[name]][row_match[row]]
        normalized[[name]][row] <- source_normalized[[name]][row_match[row]]
      }
    } else {
      raw[[name]][matched] <- source_raw[[name]][row_match[matched]]
      normalized[[name]][matched] <- source_normalized[[name]][row_match[matched]]
    }
  }
  length_fields <- c(
    "lengths", "min_length", "max_length", "length_step", "trim",
    "min_boundary_length"
  )
  for (name in setdiff(names(current), length_fields)) {
    raw[[name]] <- current[[name]]
  }
  for (name in intersect(length_fields, names(current))) {
    for (row in seq_len(nrow(current))) {
      current_value <- if (name == "lengths") current[[name]][[row]] else
        current[[name]][row]
      normalized_value <- if (name == "lengths") {
        normalized[[name]][[row]]
      } else {
        normalized[[name]][row]
      }
      if (!identical(current_value, normalized_value)) {
        if (name == "lengths") raw[[name]][row] <- list(current_value) else
          raw[[name]][row] <- current_value
      }
    }
  }
  rownames(raw) <- NULL
  raw
}

.mc_legacy_product <- function(product, priority, species = "all", ...) {
  dots <- list(...)
  old_fields <- c(
    min_top_length = "min_boundary_length",
    continue_to_top = "allow_lower_products",
    max_pieces = "max_logs_per_segment"
  )
  for (old in intersect(names(dots), names(old_fields))) {
    replacement <- old_fields[[old]]
    if (replacement %in% names(dots)) {
      stop("Supply only ", replacement, ".", call. = FALSE)
    }
    .merge_deprecated(old, replacement)
    names(dots)[names(dots) == old] <- replacement
  }
  if (identical(dots$scale_rule, "exact_profile")) {
    .merge_deprecated("exact_profile", "cubic")
    dots$scale_rule <- "cubic"
  }
  duplicate <- intersect(names(dots), c("product", "priority", "species"))
  if (length(duplicate)) {
    stop("Product fields were supplied more than once: ",
         paste(duplicate, collapse = ", "),
         ". Supply each field once and try again.", call. = FALSE)
  }
  unknown <- setdiff(
    names(dots),
    c(names(.mc_product_defaults()), grep("^meta_", names(dots), value = TRUE))
  )
  if (length(unknown)) {
    stop("Unknown product column: ", unknown[[1L]],
         ". Remove it or rename it to a documented product field.",
         call. = FALSE)
  }
  if (is.factor(product)) product <- as.character(product)
  species <- .mc_product_species_rule(species)
  values <- .mc_product_defaults()
  values$species <- species
  values[intersect(names(dots), names(values))] <-
    dots[intersect(names(dots), names(values))]
  if (is.null(values$grade)) {
    values$grade <- product
  }
  lengths <- values$lengths
  values$lengths <- NULL
  row <- c(list(product = product, priority = priority), values,
           dots[grep("^meta_", names(dots))])
  row$lengths <- I(list(lengths))
  row <- row[c(.mc_product_columns(), grep("^meta_", names(row), value = TRUE))]
  frame <- as.data.frame(row, stringsAsFactors = FALSE, optional = TRUE)
  answer <- .mc_validate_products_expanded(frame, "imperial")
  attr(answer, "calculation_products") <- TRUE
  answer
}

#' Combine log specifications into a product table
#'
#' Combine specification rows into a validated product table.
#'
#' @param ... Zero or more product data frames, each with the fields in
#'   [Product specification schema][product_schema]. Required fields cannot be missing.
#'
#' Omission returns an empty product
#'   table. Example: `products(saw, pulp)`. Supply matching columns, including
#'   any `meta_` fields, because combining dissimilar columns is an error.
#'
#' @details Lengths and trim are initially rounded to the nearest inch with
#'   halves upward. Original supplied lengths are retained for normalization
#'   when [merchandise()] selects imperial or metric units. Preserve the
#'   returned table and its attributes for that later conversion.
#'
#' @return A `merch_products` data frame with the columns described in
#'   [Product specification schema][product_schema].
#'   Labels are character, measurements are numeric, flags are logical, and
#'   priorities and log limits are integers. Additional `meta_` columns retain
#'   caller notes without changing calculations.
#' @section Product specifications:
#' See [Product specification schema][product_schema] for every field, its units,
#' defaults, and constraints. A discrete-length list cell can be constructed as
#' `I(list(c(16, 20)))`. Keep product attributes when passing the table to a run.
#' @section Status and missing values:
#' These functions return no calculation status codes. Invalid specifications
#' stop the call, so correct the named field before selecting logs. Missing
#' optional bounds mean no restriction only where the field definition says
#' so.
#'
#' A valid product table does not establish volume. `no_feasible_log` from a
#' later merchandising call means a valid result with no logs for that tree.
#' @seealso [product()] to specify one product,
#'   [merchandise()] to select logs using the resulting table.
#' @export
#' @usage
#'
#' ## Call signatures
#' products(...)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Combine compatible example specifications
#' example_products(name = 'pnw') %>%
#'   products() %>%
#'   select(product, priority, volume_unit)
products <- function(...) {
  rows <- list(...)
  if (!length(rows)) {
    empty <- data.frame(product = character(), priority = integer())
    for (name in names(.mc_product_defaults())) {
      type <- .mc_product_column_type(name)
      empty[[name]] <- switch(type,
        character = character(), integer = integer(), logical = logical(),
        double = double(), list = I(list())
      )
    }
    return(.mc_publish_products(.mc_validate_products_expanded(empty, "imperial")))
  }
  if (!all(vapply(rows, is.data.frame, logical(1L)))) {
    stop(
      "Every argument must be a product data frame. ",
      "Build rows with product() and try again.", call. = FALSE
    )
  }
  expanded <- lapply(rows, function(row) {
    .validate_products_units(row, "imperial", check_units = FALSE)
  })
  aliases <- unlist(lapply(expanded, attr, which = "measurement_aliases", exact = TRUE))
  columns <- unique(unlist(lapply(expanded, names), use.names = FALSE))
  combined <- do.call(rbind, lapply(expanded, function(row) {
    .mc_plain_product_frame(attr(row, "raw_products", exact = TRUE))[columns]
  }))
  rownames(combined) <- NULL
  normalized <- .mc_validate_products_expanded(combined, "imperial")
  if (all(vapply(rows, function(row) {
    isTRUE(attr(row, "calculation_products", exact = TRUE))
  }, logical(1L)))) {
    attr(normalized, "calculation_products") <- TRUE
    return(normalized)
  }
  measurement <- do.call(rbind, lapply(rows, function(row) {
    row <- .mc_publish_products(.validate_products_units(row, "imperial", check_units = FALSE))
    .mc_plain_product_frame(row)[c("product", "volume_unit", "inside_bark", "split_scale", "round")]
  }))
  attr(normalized, "legacy_units") <- do.call(rbind, lapply(expanded, attr, which = "legacy_units"))
  attr(normalized, "measurement_fields") <- measurement
  attr(attr(normalized, "raw_products"), "measurement_fields") <- measurement
  answer <- .mc_publish_products(normalized)
  if (length(aliases)) aliases <- aliases[order(match(names(aliases), answer$product))]
  attr(answer, "measurement_aliases") <- aliases
  answer
}

.mc_product_column_type <- function(name) {
  if (name %in% c(
    "product", "species", "grade", "length_parity", "diameter_basis",
    "max_sweep", "max_crook", "segmentation_policy", "scale_rule",
    "measurement_quantity", "scale_unit", "scale_bark_basis", "diameter_round",
    "length_round", "volume_round", "source_id"
  )) return("character")
  if (name %in% c("priority", "max_logs_per_segment")) return("integer")
  if (name %in% c("pruned", "fallback", "pulp_product", "allow_lower_products")) {
    return("logical")
  }
  if (name == "lengths") return("list")
  "double"
}

.mc_typed_default <- function(name, n) {
  value <- .mc_product_defaults()[[name]]
  if (name == "grade") return(rep(NA_character_, n))
  if (name == "lengths") return(I(rep(list(NULL), n)))
  if (is.null(value)) {
    type <- .mc_product_column_type(name)
    return(switch(type,
      character = rep(NA_character_, n),
      integer = rep(NA_integer_, n),
      logical = rep(NA, n),
      double = rep(NA_real_, n)
    ))
  }
  rep(value, n)
}

.mc_product_species_rule <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  pattern <- "^(all|softwood|hardwood|spcd\\([1-9][0-9]*(,[1-9][0-9]*)*\\))$"
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
        grepl(pattern, x, perl = TRUE)) {
    return(x)
  }
  codes <- unique(.mc_resolve_species(x, "species", allow_na = FALSE))
  paste0("spcd(", paste(codes, collapse = ","), ")")
}

.mc_coerce_product_types <- function(x) {
  character_columns <- .mc_product_columns()[vapply(
    .mc_product_columns(),
    function(name) .mc_product_column_type(name) == "character",
    logical(1L)
  )]
  for (name in intersect(character_columns, names(x))) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  }
  if ("species" %in% names(x)) {
    pattern <- "^(all|softwood|hardwood|spcd\\([1-9][0-9]*(,[1-9][0-9]*)*\\))$"
    if (.mc_numeric(x$species)) {
      x$species <- vapply(x$species, .mc_product_species_rule, character(1L))
    } else if (is.character(x$species)) {
      replace <- is.na(x$species) | !grepl(pattern, x$species, perl = TRUE)
      if (any(replace)) {
        x$species[replace] <- vapply(
          x$species[replace], .mc_product_species_rule, character(1L)
        )
      }
    }
  }
  for (name in intersect(c("priority", "max_logs_per_segment"), names(x))) {
    if (.mc_numeric(x[[name]])) {
      x[[name]] <- .mc_as_integer(x[[name]], name)
    }
  }
  for (name in intersect(
    c("pruned", "fallback", "pulp_product", "allow_lower_products"), names(x)
  )) {
    x[[name]] <- .mc_as_logical(x[[name]], name)
  }
  double_columns <- .mc_product_columns()[vapply(
    .mc_product_columns(),
    function(name) .mc_product_column_type(name) == "double",
    logical(1L)
  )]
  for (name in intersect(double_columns, names(x))) {
    if (.mc_numeric(x[[name]])) x[[name]] <- as.double(x[[name]])
  }
  if ("lengths" %in% names(x) && is.list(x$lengths)) {
    x$lengths <- I(lapply(x$lengths, function(value) {
      if (.mc_numeric(value)) as.double(value) else value
    }))
  }
  x
}

.mc_validate_species <- function(x) {
  pattern <- "^(all|softwood|hardwood|spcd\\([1-9][0-9]*(,[1-9][0-9]*)*\\))$"
  if (anyNA(x) || any(!grepl(pattern, x, perl = TRUE))) {
    stop(
      "species contains an invalid species rule. Use an FIA code, symbol, ",
      "common name, or a documented species rule.", call. = FALSE
    )
  }
  explicit <- grepl("^spcd\\(", x)
  if (any(explicit)) {
    codes <- lapply(x[explicit], function(rule) {
      values <- strsplit(substr(rule, 6L, nchar(rule) - 1L), ",", fixed = TRUE)[[1L]]
      integers <- as.integer(values)
      if (anyDuplicated(integers)) {
        stop(
          "species code lists may not contain duplicates. ",
          "List each FIA code once and try again.", call. = FALSE
        )
      }
      integers
    })
  } else {
    codes <- list()
  }
  invisible(codes)
}

.mc_species_codes <- function(rules) {
  reference <- species_reference
  lapply(rules, function(rule) {
    if (rule == "all") return(as.integer(reference$spcd))
    if (rule %in% c("softwood", "hardwood")) {
      return(as.integer(reference$spcd[reference$softwood_hardwood == rule]))
    }
    as.integer(strsplit(substr(rule, 6L, nchar(rule) - 1L), ",", fixed = TRUE)[[1L]])
  })
}

.mc_validate_product_types <- function(x) {
  for (name in .mc_product_columns()) {
    expected <- .mc_product_column_type(name)
    valid <- switch(expected,
      character = is.character(x[[name]]),
      integer = is.integer(x[[name]]),
      logical = is.logical(x[[name]]),
      list = is.list(x[[name]]),
      double = is.double(x[[name]])
    )
    if (!valid) {
      stop(
        name, " has an unsupported type. ",
        "Supply the documented values and try again.", call. = FALSE
      )
    }
  }
}

.mc_normalize_lengths <- function(x, units) {
  q <- .mc_q(units)
  normalized <- vector("list", nrow(x))
  min_tick <- max_tick <- step_tick <- rep(NA_integer_, nrow(x))
  trim_tick <- top_tick <- rep(NA_integer_, nrow(x))
  for (row in seq_len(nrow(x))) {
    lengths <- x$lengths[[row]]
    discrete <- !is.null(lengths)
    if (discrete) {
      if (!is.double(lengths) || !length(lengths) || anyNA(lengths) ||
            any(!is.finite(lengths)) || any(lengths < 0)) {
        stop(
          paste(
            "Each non-NULL lengths cell must be a nonempty numeric vector",
            "of finite nonnegative values. Supply usable product lengths and try again."
          ),
          call. = FALSE
        )
      }
      if (any(!is.na(c(x$min_length[row], x$max_length[row], x$length_step[row])))) {
        stop(
          "Discrete length rows must leave step fields missing. ",
          "Use either lengths or the step fields, not both.", call. = FALSE
        )
      }
      ticks <- sort(unique(.mc_ticks(lengths, q)))
      if (any(ticks == 0)) {
        stop(
          "Product lengths may not normalize to zero. ",
          "Supply lengths at least one package quantum long.", call. = FALSE
        )
      }
      values <- .mc_untick(ticks, q)
      integer_nominal <- abs(values - round(values)) < 1e-12
      keep <- switch(x$length_parity[row],
        any = rep(TRUE, length(values)),
        even = integer_nominal & round(values) %% 2 == 0,
        odd = integer_nominal & round(values) %% 2 == 1
      )
      normalized[[row]] <- values[keep]
      if (!length(normalized[[row]])) {
        stop(
          "Length parity removes every discrete product length. ",
          "Add a matching length or change length_parity.", call. = FALSE
        )
      }
    } else {
      fields <- c(x$min_length[row], x$max_length[row], x$length_step[row])
      if (is.na(fields[1L]) || is.na(fields[3L]) ||
            any(!is.na(fields) & (!is.finite(fields) | fields < 0))) {
        stop(
          "A length range requires finite nonnegative min_length and length_step. ",
          "Supply both fields and try again.", call. = FALSE
        )
      }
      min_tick[row] <- .mc_ticks(fields[1L], q)
      step_tick[row] <- .mc_ticks(fields[3L], q)
      if (min_tick[row] == 0L || step_tick[row] == 0L) {
        stop(
          "Step lengths and minimum lengths may not normalize to zero. ",
          "Increase them to at least one package quantum.", call. = FALSE
        )
      }
      if (!is.na(fields[2L])) {
        max_tick[row] <- .mc_ticks(fields[2L], q)
        if (max_tick[row] < min_tick[row]) {
          stop(
            "Normalized max_length must not be below min_length. ",
            "Increase max_length or decrease min_length.", call. = FALSE
          )
        }
      }
      x$min_length[row] <- .mc_untick(min_tick[row], q)
      x$max_length[row] <- if (is.na(max_tick[row])) NA_real_ else .mc_untick(max_tick[row], q)
      x$length_step[row] <- .mc_untick(step_tick[row], q)
    }
    trim_tick[row] <- .mc_ticks(x$trim[row], q)
    x$trim[row] <- .mc_untick(trim_tick[row], q)
    if (!is.na(x$min_boundary_length[row])) {
      if (!is.finite(x$min_boundary_length[row]) || x$min_boundary_length[row] < 0) {
        stop("min_boundary_length must be finite and nonnegative when supplied.",
             call. = FALSE)
      }
      top_tick[row] <- .mc_ticks(x$min_boundary_length[row], q)
      if (top_tick[row] == 0L) {
        stop(
          "min_boundary_length may not normalize to zero. ",
          "Increase it to at least one package quantum.", call. = FALSE
        )
      }
      x$min_boundary_length[row] <- .mc_untick(top_tick[row], q)
    }
  }
  x$lengths <- I(normalized)
  attr(x, "length_ticks") <- lapply(normalized, function(value) as.integer(.mc_ticks(value, q)))
  attr(x, "min_length_tick") <- min_tick
  attr(x, "max_length_tick") <- max_tick
  attr(x, "length_step_tick") <- step_tick
  attr(x, "trim_tick") <- trim_tick
  attr(x, "min_top_length_tick") <- top_tick
  attr(x, "quantum") <- q
  x
}

.mc_validate_product_values <- function(x) {
  n <- nrow(x)
  if (anyNA(x$product) || any(!nzchar(x$product)) || anyDuplicated(x$product)) {
    stop(
      "product must be nonempty, unique, and nonmissing. ",
      "Give every product a distinct name and try again.", call. = FALSE
    )
  }
  if (anyNA(x$priority) || any(x$priority <= 0L) || anyDuplicated(x$priority)) {
    stop(
      "priority must be positive, unique, and nonmissing. ",
      "Give every product a distinct positive priority.", call. = FALSE
    )
  }
  .mc_validate_species(x$species)
  if (anyNA(x$grade) || any(!nzchar(x$grade))) {
    stop(
      "grade must be nonempty and nonmissing. ",
      "Supply a grade or omit it to use the product name.", call. = FALSE
    )
  }
  logical_required <- c("fallback", "pulp_product", "allow_lower_products")
  if (any(vapply(x[logical_required], anyNA, logical(1L)))) {
    stop(
      "fallback, pulp_product, and allow_lower_products may not be missing. ",
      "Use TRUE/FALSE or 0/1 for each flag.", call. = FALSE
    )
  }
  intervals <- list(
    c("min_dbh", "max_dbh"), c("min_age", "max_age"),
    c("min_sed", "max_sed"), c("min_led", "max_led")
  )
  for (pair in intervals) {
    lo <- x[[pair[[1L]]]]
    hi <- x[[pair[[2L]]]]
    if (any(!is.na(lo) & (!is.finite(lo) | lo < 0)) ||
          any(!is.na(hi) & (!is.finite(hi) | hi < 0)) ||
          any(!is.na(lo) & !is.na(hi) & hi < lo)) {
      stop(pair[[1L]], " and ", pair[[2L]],
           " form an invalid interval. Correct the limits and try again.",
           call. = FALSE)
    }
  }
  if (anyNA(x$min_dbh) || anyNA(x$min_sed) || anyNA(x$min_led)) {
    stop(
      "min_dbh, min_sed, and min_led may not be missing. ",
      "Supply each required lower limit.", call. = FALSE
    )
  }
  finite_nonnegative <- c("trim", "price_quantity")
  for (name in finite_nonnegative) {
    if (anyNA(x[[name]]) || any(!is.finite(x[[name]])) || any(x[[name]] < 0)) {
      stop(
        name, " must contain finite nonnegative values. ",
        "Correct the values and try again.", call. = FALSE
      )
    }
  }
  if (any(x$price_quantity <= 0)) {
    stop(
      "price_quantity must be positive. ",
      "Supply the quantity covered by one price and try again.", call. = FALSE
    )
  }
  if (any(!is.na(x$max_defect_pct) &
            (!is.finite(x$max_defect_pct) | x$max_defect_pct < 0 |
               x$max_defect_pct > 100))) {
    stop(
      "max_defect_pct must be missing or between zero and 100. ",
      "Correct the percentage and try again.", call. = FALSE
    )
  }
  if (any(!is.na(x$max_logs_per_segment) &
            (x$max_logs_per_segment < 1L | x$max_logs_per_segment > 1000L))) {
    stop(
      "max_logs_per_segment must be missing or an integer from 1 through 1000. ",
      "Correct the log limit and try again.", call. = FALSE
    )
  }
  if (any(!is.na(x$price) & (!is.finite(x$price) | x$price < 0))) {
    stop(
      "price must be missing or finite and nonnegative. ",
      "Correct the product price and try again.", call. = FALSE
    )
  }
  if (any(!x$length_parity %in% c("any", "even", "odd"))) {
    stop(
      "length_parity must be any, even, or odd. ",
      "Choose one of those values and try again.", call. = FALSE
    )
  }
  if (any(!x$diameter_basis %in% c("ib", "ob")) ||
        any(!x$scale_bark_basis %in% c("ib", "ob"))) {
    stop(
      "Diameter and scale bark bases must be ib or ob. ",
      "Choose inside bark or outside bark and try again.", call. = FALSE
    )
  }
  policies <- c("generic", paste0("nvel_opt_", c(11:14, 21:24)))
  if (any(!x$segmentation_policy %in% policies)) {
    stop(
      "segmentation_policy is not recognized. ",
      "Use one of: ", paste(policies, collapse = ", "), ".", call. = FALSE
    )
  }
  nvel_policy <- x$segmentation_policy != "generic"
  discrete <- !vapply(x$lengths, is.null, logical(1L))
  if (any(nvel_policy & (discrete | is.na(x$max_length)))) {
    stop(
      "NVEL segmentation policies require step lengths with a finite maximum. ",
      "Supply min_length, max_length, and length_step.",
      call. = FALSE
    )
  }
  if (any(!x$scale_rule %in% .mc_scale_rules)) {
    stop(
      "scale_rule is not registered. ",
      "Choose a scale rule documented by product().", call. = FALSE
    )
  }
  if (any(!x$measurement_quantity %in% c("board_foot", "cubic", "green_weight", "cord"))) {
    stop(
      "measurement_quantity is not recognized. ",
      "Choose board_foot, cubic, green_weight, or cord.", call. = FALSE
    )
  }
  valid_units <- list(
    board_foot = "board_foot", cubic = c("ft3", "m3"),
    green_weight = c("green_short_ton", "green_metric_ton"), cord = "cord"
  )
  bad_unit <- vapply(seq_len(n), function(i) {
    !x$scale_unit[i] %in% valid_units[[x$measurement_quantity[i]]]
  }, logical(1L))
  if (any(bad_unit)) {
    stop(
      "scale_unit is incompatible with measurement_quantity. ",
      "Choose the matching unit documented by product().", call. = FALSE
    )
  }
  board_rule <- grepl("^(scribner|international|doyle)", x$scale_rule)
  if (any(board_rule != (x$measurement_quantity == "board_foot"))) {
    stop(
      "Board-foot rules require board_foot and other rules may not use it. ",
      "Match scale_rule to measurement_quantity and try again.", call. = FALSE
    )
  }
  if (any(x$measurement_quantity %in% c("green_weight", "cord") &
            x$scale_rule != "cubic")) {
    stop(
      "Green weight and cord products require cubic. ",
      "Set scale_rule to cubic and try again.", call. = FALSE
    )
  }
  fixed_ib <- board_rule
  if (any(fixed_ib & x$scale_bark_basis != "ib")) {
    stop(
      "Board-foot rules require inside-bark scaling. ",
      "Set scale_bark_basis to ib and try again.", call. = FALSE
    )
  }
  nvel_internal <- grepl("^(scribner|international)", x$scale_rule)
  if (any(nvel_internal & (x$length_round != "rule_default" |
                             x$volume_round != "rule_default"))) {
    stop(
      "nvel_internal rounding is not overrideable. ",
      "Leave all rounding fields at rule_default.", call. = FALSE
    )
  }
  if (any(x$measurement_quantity == "cord" &
            (is.na(x$cord_solid_fraction) |
               !is.finite(x$cord_solid_fraction) |
               x$cord_solid_fraction <= 0 |
               x$cord_solid_fraction >= 1))) {
    stop(
      "cord_solid_fraction must be strictly between zero and one for cord ",
      "products. Supply a valid solid-wood fraction.", call. = FALSE
    )
  }
  if (any(x$measurement_quantity != "cord" & !is.na(x$cord_solid_fraction))) {
    stop(
      "cord_solid_fraction is only accepted for cord products. ",
      "Remove it or change measurement_quantity to cord.", call. = FALSE
    )
  }
  for (name in c("diameter_round", "length_round", "volume_round")) {
    if (anyNA(x[[name]]) || any(!x[[name]] %in% .mc_rounding_operators)) {
      stop(
        name, " contains an unsupported rounding operator. ",
        "Choose a documented rounding value and try again.", call. = FALSE
      )
    }
  }
  diameter_ops <- c(
    "rule_default", "none", "truncate_1in", "nearest_1in_half_up",
    "nearest_0.5in_half_up", "truncate_1cm", "nearest_1cm_half_up"
  )
  length_ops <- c(
    "rule_default", "none", "truncate_1ft", "nearest_1ft_half_up",
    "truncate_0.1m", "nearest_0.1m_half_up"
  )
  volume_ops <- c(
    "rule_default", "none", "truncate_board_foot",
    "nearest_board_foot_half_up", "nearest_10_board_feet_half_up"
  )
  if (any(!x$diameter_round %in% diameter_ops) ||
        any(!x$length_round %in% length_ops) ||
        any(!x$volume_round %in% volume_ops)) {
    stop(
      "A rounding operator was supplied in the wrong dimension column. ",
      "Move it to the matching diameter, length, or volume field.",
      call. = FALSE
    )
  }
  if (any(!board_rule & !x$volume_round %in% c("rule_default", "none"))) {
    stop(
      "Board-foot volume rounding is incompatible with a cubic scale. ",
      "Use none or rule_default for cubic volume.", call. = FALSE
    )
  }
  exact <- x$scale_rule == "cubic"
  if (any(exact & (!x$diameter_round %in% c("rule_default", "none") |
                     !x$length_round %in% c("rule_default", "none")))) {
    stop(
      "cubic does not accept dimension rounding. ",
      "Use none or rule_default for diameter and length.", call. = FALSE
    )
  }
  for (name in c("max_sweep", "max_crook")) {
    if (any(!is.na(x[[name]]) & !nzchar(x[[name]]))) {
      stop(
        name, " categories must be nonempty when supplied. ",
        "Supply a category label or use NA.", call. = FALSE
      )
    }
  }
  if (sum(x$pulp_product) > 1L) {
    stop(
      "At most one product may be the pulp product. ",
      "Mark only one row with pulp_product = TRUE.", call. = FALSE
    )
  }
  invisible(x)
}

.mc_validate_products_expanded <- function(x, units) {
  .mc_scalar_choice(units, "units", c("imperial", "metric"))
  if (!is.data.frame(x)) {
    stop(
      "x must be a data frame. ",
      "Build products with product() or products().",
      call. = FALSE
    )
  }
  x <- .mc_product_raw_input(x)
  unknown <- setdiff(names(x), c(.mc_product_columns(), grep("^meta_", names(x), value = TRUE)))
  if (length(unknown)) {
    stop("Unknown product column: ", unknown[[1L]],
         ". Remove it or rename it to a documented product field.",
         call. = FALSE)
  }
  required <- c(
    "product", "priority", "min_sed", "diameter_basis", "scale_rule",
    "measurement_quantity", "scale_unit", "scale_bark_basis"
  )
  missing_required <- setdiff(required, names(x))
  if (length(missing_required)) {
    stop("Missing required product column: ", missing_required[[1L]],
         ". Add that field or build the row with product().",
         call. = FALSE)
  }
  n <- nrow(x)
  for (name in setdiff(.mc_product_columns(), names(x))) {
    x[[name]] <- .mc_typed_default(name, n)
  }
  x <- x[c(.mc_product_columns(), setdiff(names(x), .mc_product_columns()))]
  x <- .mc_coerce_product_types(x)
  x$grade[is.na(x$grade)] <- x$product[is.na(x$grade)]
  .mc_validate_product_types(x)
  .mc_validate_product_values(x)
  x <- x[order(x$priority, x$product, method = "radix"), , drop = FALSE]
  rownames(x) <- NULL
  raw_products <- .mc_plain_product_frame(x)
  x <- .mc_normalize_lengths(x, units)
  attr(x, "species_codes") <- .mc_species_codes(x$species)
  attr(x, "units") <- units
  attr(x, "raw_products") <- raw_products
  attr(x, "normalized_products") <- .mc_plain_product_frame(x)
  class(x) <- c("merch_products", "data.frame")
  rownames(x) <- NULL
  x
}

#' Check a product table before selecting logs
#'
#' Validate and normalize a product table while retaining its original measurement values.
#'
#' @param x Required data frame with one row per product and the fields in
#'   [Product specification schema][product_schema].
#'   Zero rows are accepted when required columns are present. Omission or
#'   a missing object is an error. Example: `example_products('us_south')`.
#'
#' @details Lengths and trim are initially rounded to the nearest inch with
#'   halves upward. Original supplied lengths are retained for normalization
#'   when [merchandise()] selects imperial or metric units. Preserve the
#'   returned table and its attributes for that later conversion.
#'
#' @return A `merch_products` data frame with the columns described in
#'   [Product specification schema][product_schema].
#'   Labels are character, measurements are numeric, flags are logical, and
#'   priorities and log limits are integers. Additional `meta_` columns retain
#'   caller notes without changing calculations.
#' @section Product specifications:
#' See [Product specification schema][product_schema] for every field, its units,
#' defaults, and constraints. A discrete-length list cell can be constructed as
#' `I(list(c(16, 20)))`. Keep product attributes when passing the table to a run.
#' @section Status and missing values:
#' These functions return no calculation status codes. Invalid specifications
#' stop the call, so correct the named field before selecting logs. Missing
#' optional bounds mean no restriction only where the field definition says
#' so.
#'
#' A valid product table does not establish volume. `no_feasible_log` from a
#' later merchandising call means a valid result with no logs for that tree.
#' @seealso [product()] to specify one product, [products()] to combine rows,
#'   [merchandise()] to select logs using the resulting table.
#' @export
#' @usage
#'
#' ## Call signatures
#' validate_products(x)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Validate the shipped product table
#' example_products(name = 'pnw') %>%
#'   validate_products() %>%
#'   select(product, priority, min_sed)
validate_products <- function(x) {
  .mc_publish_products(.validate_products_units(x, "imperial", check_units = FALSE))
}

.mc_subset_products <- function(x, keep) {
  result <- x[keep, , drop = FALSE]
  for (name in c(
    "species_codes", "length_ticks", "min_length_tick", "max_length_tick",
    "length_step_tick", "trim_tick", "min_top_length_tick"
  )) {
    attr(result, name) <- attr(x, name)[keep]
  }
  attr(result, "quantum") <- attr(x, "quantum")
  attr(result, "units") <- attr(x, "units")
  raw <- attr(x, "raw_products", exact = TRUE)
  if (!is.null(raw)) attr(result, "raw_products") <- raw[keep, , drop = FALSE]
  normalized <- attr(x, "normalized_products", exact = TRUE)
  if (!is.null(normalized)) {
    attr(result, "normalized_products") <- normalized[keep, , drop = FALSE]
  }
  class(result) <- c("merch_products", "data.frame")
  rownames(result) <- NULL
  result
}
