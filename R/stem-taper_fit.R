.published_taper_forms <- c("kozak_2002", "kozak_1988", "max_burkhart")

.published_coefficient_names <- function(form) {
  switch(form,
    kozak_1988 = c("a0", "a1", "a2", paste0("b", 1:5), "p"),
    kozak_2002 = c("a0", "a1", "a2", paste0("b", 1:6)),
    max_burkhart = c(paste0("b", 1:4), "a1", "a2")
  )
}

.published_taper_source <- function(form) {
  switch(form,
    kozak_1988 = paste(
      "Kozak, A. (1988). A variable-exponent taper equation.",
      "Canadian Journal of Forest Research 18(11):1363-1368.",
      "doi:10.1139/x88-213."
    ),
    kozak_2002 = paste(
      "Kozak, A. (2004). My last words on taper equations.",
      "The Forestry Chronicle 80(4):507-515.",
      "doi:10.5558/tfc80507-4."
    ),
    max_burkhart = paste(
      "Max, T.A. and Burkhart, H.E. (1976). Segmented polynomial",
      "regression applied to taper equations. Forest Science 22(3):283-289.",
      "doi:10.1093/forestscience/22.3.283."
    )
  )
}

.validate_published_taper_data <- function(form, coefficients) {
  form <- .scalar_character(form, "form", .published_taper_forms)
  expected <- .published_coefficient_names(form)
  if (!is.numeric(coefficients) || !is.null(dim(coefficients)) ||
        !identical(names(coefficients), expected) ||
        any(!is.finite(coefficients))) {
    stop(
      "data must be a finite named coefficient vector in this order: ",
      paste(expected, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (form == "kozak_1988" &&
        (coefficients[["a0"]] <= 0 || coefficients[["a2"]] <= 0 ||
           coefficients[["p"]] <= 0 || coefficients[["p"]] >= 1)) {
    stop("Kozak 1988 requires a0 and a2 above zero and p in (0, 1).",
      call. = FALSE
    )
  }
  if (form == "kozak_2002" && coefficients[["a0"]] <= 0) {
    stop("Kozak 2002 requires a0 above zero.", call. = FALSE)
  }
  if (form == "max_burkhart" &&
        (coefficients[["a2"]] <= 0 ||
           coefficients[["a2"]] >= coefficients[["a1"]] ||
           coefficients[["a1"]] >= 1)) {
    stop("Max and Burkhart requires 0 < a2 < a1 < 1.", call. = FALSE)
  }
  invisible(coefficients)
}

#' Create a taper model from a coefficient set
#'
#' Create a compiled taper model from a named coefficient vector in centimeters and meters.
#'
#' @param id Required single nonempty character equation name, unitless. Missing and empty names
#' are errors. Example: `'example.fitted'`.
#'
#' Registration additionally requires a distinct local name containing a dot.
#' @param form Required single character equation name, "kozak_2002",
#'   "kozak_1988", or "max_burkhart". No default. Unitless.
#'
#' Missing or unknown names are errors. Example: `'max_burkhart'`.
#' @param coefficients Required complete finite named numeric vector, with
#'   the specified order and bounds. No default. Missing or incomplete values are
#'   errors.
#'
#' Coefficients use centimeters and meters.
#' @param species Positive whole-number Forest Inventory and Analysis species
#'   codes supplied as an integer
#'   or double vector, or `integer()` for an unrestricted model. Values are
#'   normalized to integers during validation. Default `integer()` is unrestricted.
#'
#' Missing or invalid codes are errors. Unitless.
#' @param stump_ht One finite nonnegative numeric stump height in meters. Default
#'   `0` means ground. Missing is an error.
#' @param bark_ratio One numeric ratio of inside-bark to outside-bark diameter,
#'   strictly greater than zero and at most one. Default `NA_real_` supplies
#'   no bark ratio. Unitless.
#' @param source Source text for the coefficient set. The equation-form
#'   citation is used when this is `NULL`.
#' @param notes One character note, unitless. Empty text is allowed, missing text
#'   is an error. The coefficient conversion defaults to `''`.
#'
#' Conversion
#'   from a fit defaults to `'Population fixed effects from fit_taper().'`,
#'   identifying the common fitted relationship. Example: `notes = 'Local fit'`.
#'
#' @details Kozak 1988 coefficients are ordered `a0`, `a1`, `a2`, `b1`
#' through `b5`, and `p`. Kozak 2002 coefficients are ordered `a0`, `a1`,
#' `a2`, and `b1` through `b6`. Max and Burkhart coefficients are ordered
#' `b1` through `b4`, `a1`, and `a2`, where `a2` is the lower join point.
#'
#' Diameters are in centimeters and heights are in meters in each equation.
#' The computation functions convert imperial calls at the package boundary.
#'
#' @references
#' Kozak A (1988). "A variable-exponent taper equation." *Canadian Journal
#' of Forest Research*, 18(11), 1363-1368. \doi{10.1139/x88-213}.
#'
#' Kozak A (2004). "My last words on taper equations." *The Forestry
#' Chronicle*, 80(4), 507-515. \doi{10.5558/tfc80507-4}.
#'
#' Max TA, Burkhart HE (1976). "Segmented polynomial regression applied to
#' taper equations." *Forest Science*, 22(3), 283-289.
#' \doi{10.1093/forestscience/22.3.283}.
#'
#' @return A validated `taper_model` with the coefficients in its `data`
#'   field.
#' @export
#' @usage
#'
#' ## Call signatures
#' taper_model_from_coefficients(id, form, coefficients, species = integer(), stump_ht = 0,
#'   bark_ratio = NA_real_, source = NULL, notes = '')
#' @examples
#' ## Fit coefficients from the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements,
#'                  form = 'max_burkhart',
#'                  units = 'imperial')
#'
#' ## Create a compiled model from the coefficients
#' taper_model_from_coefficients(id = 'example.coefficients',
#'                               form = fit$form,
#'                               coefficients = fit$coefficients)
#' @section Coefficient order and bounds:
#' Kozak 1988 requires `a0`, `a1`, `a2`, `b1`, `b2`, `b3`, `b4`, `b5`, `p`,
#' in that order. `a0` and `a2` must be positive and `p` strictly between zero
#' and one. A fitting start additionally requires `0.05 < p < 0.35`.
#'
#' Kozak 2002 requires `a0`, `a1`, `a2`, `b1`, `b2`, `b3`, `b4`, `b5`, `b6`,
#' with positive `a0`. Max and Burkhart requires `b1`, `b2`, `b3`, `b4`,
#' `a1`, `a2`, with `0 < a2 < a1 < 1`. Other coefficients may be any finite
#' numeric value.
#'
#' Names and order must match, with no missing entries. Coefficients describe diameter in
#' centimeters and height in meters and
#' must not be treated as coefficients for an imperial equation. Join heights
#' and `p` are unitless proportions of total height.
#' @inheritSection new_taper_model Taper model fields
#' @inheritSection new_taper_model Equation function inputs and returns
#' @section Status and missing values:
#' These functions return no tree codes. Missing or invalid coefficients,
#' identifiers, species codes, or conventions stop construction. An omitted
#' bark ratio leaves outside-bark calculations unavailable unless a later
#' call supplies one.
#'
#' Register the object before using its name.
#' @seealso [fit_taper()] to estimate coefficients, [register_taper_model()]
#'   to make the returned equation available, [dib()] to calculate diameter.
#' @param source One nonmissing character source description, unitless. Default `NULL` supplies the
#'   equation-form citation and, for a converted
#'   fit, its fitting origin. An empty string is allowed.
#'
#' Example:
#'   `source = 'Illustrative coefficient set'`. Missing text is an error.
taper_model_from_coefficients <- function(
    id, form, coefficients, species = integer(), stump_ht = 0,
    bark_ratio = NA_real_, source = NULL, notes = "") {
  id <- .scalar_character(id, "id")
  form <- .scalar_character(form, "form", .published_taper_forms)
  .validate_published_taper_data(form, coefficients)
  coefficient_names <- names(coefficients)
  coefficients <- stats::setNames(as.double(coefficients), coefficient_names)
  species <- .normalize_species_codes(species)
  if (is.null(source)) {
    source <- .published_taper_source(form)
  }
  source <- .scalar_character(source, "source", allow_empty = TRUE)
  notes <- .scalar_character(notes, "notes", allow_empty = TRUE)
  model <- new_stem_model_unchecked(
    id = id,
    family = form,
    kernel = list(
      type = "compiled",
      key = paste0("published:", form),
      has_dob = FALSE,
      has_inverse = identical(form, "max_burkhart"),
      has_integral = identical(form, "max_burkhart")
    ),
    inputs = list(
      required = character(), optional = "bark_ratio", pairs = list()
    ),
    units = "metric",
    species = species,
    stump_ht = stump_ht,
    bark_ratio = bark_ratio,
    source = source,
    notes = notes,
    data = coefficients
  )
  validate_taper_model(model)
}

.taper_kozak_1988 <- function(dbh, ht, h, coefficients) {
  output <- numeric(length(dbh))
  selected <- h < ht
  z <- h[selected] / ht[selected]
  x <- (1 - sqrt(z)) / (1 - sqrt(coefficients[["p"]]))
  exponent <- coefficients[["b1"]] * z^2 +
    coefficients[["b2"]] * log(z + 0.001) +
    coefficients[["b3"]] * sqrt(z) +
    coefficients[["b4"]] * exp(z) +
    coefficients[["b5"]] * dbh[selected] / ht[selected]
  output[selected] <- coefficients[["a0"]] *
    dbh[selected]^coefficients[["a1"]] *
    coefficients[["a2"]]^dbh[selected] * x^exponent
  output
}

.taper_kozak_2002 <- function(dbh, ht, h, coefficients) {
  output <- numeric(length(dbh))
  selected <- h < ht
  z <- h[selected] / ht[selected]
  q <- 1 - z^(1 / 3)
  x <- q / (1 - (1.3 / ht[selected])^(1 / 3))
  exponent <- coefficients[["b1"]] * z^4 +
    coefficients[["b2"]] * exp(-dbh[selected] / ht[selected]) +
    coefficients[["b3"]] * x^0.1 +
    coefficients[["b4"]] / dbh[selected] +
    coefficients[["b5"]] * ht[selected]^q +
    coefficients[["b6"]] * x
  output[selected] <- coefficients[["a0"]] *
    dbh[selected]^coefficients[["a1"]] *
    ht[selected]^coefficients[["a2"]] * x^exponent
  output
}

.taper_max_burkhart <- function(dbh, ht, h, coefficients,
                                stabilize = FALSE) {
  x <- h / ht
  squared_ratio <- coefficients[["b1"]] * (x - 1) +
    coefficients[["b2"]] * (x^2 - 1) +
    coefficients[["b3"]] * (coefficients[["a1"]] - x)^2 *
      (x <= coefficients[["a1"]]) +
    coefficients[["b4"]] * (coefficients[["a2"]] - x)^2 *
      (x <= coefficients[["a2"]])
  if (stabilize) {
    squared_ratio <- pmax(squared_ratio, 1e-12)
  }
  output <- dbh * sqrt(squared_ratio)
  output[h == ht] <- 0
  output
}

.taper_evaluate <- function(form, dbh, ht, h, coefficients,
                            stabilize = FALSE) {
  switch(form,
    kozak_1988 = .taper_kozak_1988(dbh, ht, h, coefficients),
    kozak_2002 = .taper_kozak_2002(dbh, ht, h, coefficients),
    max_burkhart = .taper_max_burkhart(
      dbh, ht, h, coefficients, stabilize = stabilize
    )
  )
}

.taper_fit_p <- function(lp) {
  0.05 + 0.30 * stats::plogis(lp)
}

.taper_fit_knots <- function(ka2, kspan) {
  a2 <- 0.001 + 0.998 * stats::plogis(ka2)
  a1 <- a2 + (0.999 - a2) * stats::plogis(kspan)
  list(a1 = a1, a2 = a2)
}

.taper_fit_kozak_1988 <- function(dbh, ht, h, la0, a1, la2, b1, b2,
                                  b3, b4, b5, lp) {
  z <- h / ht
  x <- (1 - sqrt(z)) / (1 - sqrt(.taper_fit_p(lp)))
  exponent <- b1 * z^2 + b2 * log(z + 0.001) + b3 * sqrt(z) +
    b4 * exp(z) + b5 * dbh / ht
  result <- exp(la0) * dbh^a1 * exp(la2)^dbh * x^exponent
  result[h == ht] <- 0
  result
}

.taper_fit_kozak_2002 <- function(dbh, ht, h, la0, a1, a2, b1, b2,
                                  b3, b4, b5, b6) {
  z <- h / ht
  q <- 1 - z^(1 / 3)
  x <- q / (1 - (1.3 / ht)^(1 / 3))
  exponent <- b1 * z^4 + b2 * exp(-dbh / ht) + b3 * x^0.1 +
    b4 / dbh + b5 * ht^q + b6 * x
  result <- exp(la0) * dbh^a1 * ht^a2 * x^exponent
  result[h == ht] <- 0
  result
}

.taper_fit_max_burkhart <- function(dbh, ht, h, b1, b2, b3, b4,
                                    ka2, kspan) {
  knots <- .taper_fit_knots(ka2, kspan)
  x <- h / ht
  squared_ratio <- b1 * (x - 1) + b2 * (x^2 - 1) +
    b3 * (knots$a1 - x)^2 * (x <= knots$a1) +
    b4 * (knots$a2 - x)^2 * (x <= knots$a2)
  result <- dbh * sqrt(pmax(squared_ratio, 1e-12))
  result[h == ht] <- 0
  result
}

.taper_internal_names <- function(form) {
  switch(form,
    kozak_1988 = c("la0", "a1", "la2", paste0("b", 1:5)),
    kozak_2002 = c("la0", "a1", "a2", paste0("b", 1:6)),
    max_burkhart = c(paste0("b", 1:4), "ka2", "kspan")
  )
}

.taper_natural_to_internal <- function(coefficients, form) {
  if (form == "kozak_1988") {
    if (coefficients[["p"]] <= 0.05 || coefficients[["p"]] >= 0.35) {
      stop("a fit start for Kozak 1988 requires p in (0.05, 0.35).",
        call. = FALSE
      )
    }
    return(c(
      la0 = log(coefficients[["a0"]]), a1 = coefficients[["a1"]],
      la2 = log(coefficients[["a2"]]), coefficients[paste0("b", 1:5)],
      lp = stats::qlogis((coefficients[["p"]] - 0.05) / 0.30)
    ))
  }
  if (form == "kozak_2002") {
    return(c(
      la0 = log(coefficients[["a0"]]), coefficients[c("a1", "a2")],
      coefficients[paste0("b", 1:6)]
    ))
  }
  a1 <- coefficients[["a1"]]
  a2 <- coefficients[["a2"]]
  ka2 <- stats::qlogis((a2 - 0.001) / 0.998)
  kspan <- stats::qlogis((a1 - a2) / (0.999 - a2))
  c(coefficients[paste0("b", 1:4)], ka2 = ka2, kspan = kspan)
}

.taper_internal_to_natural <- function(coefficients, form) {
  if (form == "kozak_1988") {
    return(c(
      a0 = exp(coefficients[["la0"]]), a1 = coefficients[["a1"]],
      a2 = exp(coefficients[["la2"]]), coefficients[paste0("b", 1:5)],
      p = .taper_fit_p(coefficients[["lp"]])
    ))
  }
  if (form == "kozak_2002") {
    return(c(
      a0 = exp(coefficients[["la0"]]), coefficients[c("a1", "a2")],
      coefficients[paste0("b", 1:6)]
    ))
  }
  knots <- .taper_fit_knots(
    coefficients[["ka2"]], coefficients[["kspan"]]
  )
  c(
    coefficients[paste0("b", 1:4)], a1 = knots$a1, a2 = knots$a2
  )
}

.taper_weighted_lm <- function(design, response, weights) {
  fit <- tryCatch(
    stats::lm.wfit(design, response, weights),
    error = function(error) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- fit$coefficients
  if (length(coefficients) != ncol(design) || any(!is.finite(coefficients))) {
    return(NULL)
  }
  list(
    coefficients = unname(coefficients),
    objective = sum(weights * fit$residuals^2)
  )
}

.taper_kozak_1988_starts <- function(data) {
  selected <- data$dib > 0 & data$h < data$ht
  dbh <- data$dbh[selected]
  ht <- data$ht[selected]
  h <- data$h[selected]
  response <- log(data$dib[selected])
  weights <- data$weight[selected]
  candidates <- lapply(c(0.12, 0.16, 0.20, 0.24, 0.28, 0.32), function(p) {
    z <- h / ht
    log_x <- log((1 - sqrt(z)) / (1 - sqrt(p)))
    design <- cbind(
      1, log(dbh), dbh, z^2 * log_x, log(z + 0.001) * log_x,
      sqrt(z) * log_x, exp(z) * log_x, dbh / ht * log_x
    )
    fit <- .taper_weighted_lm(design, response, weights)
    if (is.null(fit)) {
      return(NULL)
    }
    value <- fit$coefficients
    list(
      coefficients = c(
        a0 = exp(value[[1L]]), a1 = value[[2L]], a2 = exp(value[[3L]]),
        b1 = value[[4L]], b2 = value[[5L]], b3 = value[[6L]],
        b4 = value[[7L]], b5 = value[[8L]], p = p
      ),
      objective = fit$objective
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  candidates <- candidates[order(vapply(candidates, `[[`, numeric(1), "objective"))]
  lapply(utils::head(candidates, 3L), `[[`, "coefficients")
}

.taper_kozak_2002_starts <- function(data) {
  selected <- data$dib > 0 & data$h < data$ht
  dbh <- data$dbh[selected]
  ht <- data$ht[selected]
  h <- data$h[selected]
  z <- h / ht
  q <- 1 - z^(1 / 3)
  x <- q / (1 - (1.3 / ht)^(1 / 3))
  log_x <- log(x)
  design <- cbind(
    1, log(dbh), log(ht), z^4 * log_x, exp(-dbh / ht) * log_x,
    x^0.1 * log_x, log_x / dbh, ht^q * log_x, x * log_x
  )
  fit <- .taper_weighted_lm(
    design, log(data$dib[selected]), data$weight[selected]
  )
  if (is.null(fit)) {
    base <- c(
      a0 = stats::median(data$dib / data$dbh), a1 = 1, a2 = 0,
      b1 = 0, b2 = 0.5, b3 = 0.5, b4 = 0, b5 = 0, b6 = 0
    )
  } else {
    value <- fit$coefficients
    base <- c(
      a0 = exp(value[[1L]]), a1 = value[[2L]], a2 = value[[3L]],
      b1 = value[[4L]], b2 = value[[5L]], b3 = value[[6L]],
      b4 = value[[7L]], b5 = value[[8L]], b6 = value[[9L]]
    )
  }
  list(
    base,
    base * c(a0 = 1.05, a1 = 1, a2 = 1, rep(1, 6)),
    base * c(a0 = 0.95, a1 = 1, a2 = 1, rep(1, 6))
  )
}

.taper_max_burkhart_starts <- function(data) {
  x <- data$h / data$ht
  response <- (data$dib / data$dbh)^2
  candidates <- list()
  for (a2 in c(0.05, 0.10, 0.15, 0.20, 0.25)) {
    for (a1 in c(0.55, 0.65, 0.75, 0.85, 0.93)) {
      design <- cbind(
        x - 1, x^2 - 1, (a1 - x)^2 * (x <= a1),
        (a2 - x)^2 * (x <= a2)
      )
      fit <- .taper_weighted_lm(design, response, data$weight)
      if (!is.null(fit)) {
        candidates[[length(candidates) + 1L]] <- list(
          coefficients = c(
            b1 = fit$coefficients[[1L]], b2 = fit$coefficients[[2L]],
            b3 = fit$coefficients[[3L]], b4 = fit$coefficients[[4L]],
            a1 = a1, a2 = a2
          ),
          objective = fit$objective
        )
      }
    }
  }
  candidates <- candidates[order(vapply(candidates, `[[`, numeric(1), "objective"))]
  lapply(utils::head(candidates, 3L), `[[`, "coefficients")
}

.taper_default_starts <- function(data, form) {
  switch(form,
    kozak_1988 = .taper_kozak_1988_starts(data),
    kozak_2002 = .taper_kozak_2002_starts(data),
    max_burkhart = .taper_max_burkhart_starts(data)
  )
}

.taper_validate_start <- function(start, form) {
  expected <- .published_coefficient_names(form)
  if (!is.numeric(start) || !is.null(dim(start)) ||
        length(start) != length(expected) || anyDuplicated(names(start)) ||
        !setequal(names(start), expected)) {
    stop(
      "start must be a named numeric vector containing: ",
      paste(expected, collapse = ", "), ".",
      call. = FALSE
    )
  }
  start <- stats::setNames(as.double(start[expected]), expected)
  .validate_published_taper_data(form, start)
  .taper_natural_to_internal(start, form)
  start
}

.taper_required_columns <- function(data) {
  choices <- list(
    tree_id = c("tree_id", "tree"),
    dbh = "dbh",
    ht = c("ht", "total_height"),
    h = c("h", "measurement_height"),
    dib = c("dib", "diameter_inside_bark")
  )
  selected <- vapply(choices, function(names) {
    found <- names[names %in% names(data)]
    if (length(found)) found[[1L]] else NA_character_
  }, character(1))
  if (anyNA(selected)) {
    stop(
      "data must contain tree_id, dbh, ht, h, and dib columns.",
      call. = FALSE
    )
  }
  selected
}

.taper_argument_vector <- function(data, value, name, default = NULL) {
  if (is.null(value)) {
    return(default)
  }
  if (is.character(value) && length(value) == 1L && value %in% names(data)) {
    return(data[[value]])
  }
  if (length(value) == 1L) {
    return(rep(value, nrow(data)))
  }
  if (length(value) != nrow(data) || !is.null(dim(value))) {
    stop(name, " must name a data column or have one value per row.",
      call. = FALSE
    )
  }
  value
}

.taper_prepare_data <- function(data, species, group, weights, units, form) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  columns <- .taper_required_columns(data)
  values <- data.frame(
    tree_id = data[[columns[["tree_id"]]]],
    dbh = data[[columns[["dbh"]]]],
    ht = data[[columns[["ht"]]]],
    h = data[[columns[["h"]]]],
    dib = data[[columns[["dib"]]]],
    stringsAsFactors = FALSE
  )
  numeric_names <- c("dbh", "ht", "h", "dib")
  if (any(!vapply(values[numeric_names], is.numeric, logical(1)))) {
    stop("dbh, ht, h, and dib columns must be numeric.", call. = FALSE)
  }
  values$species <- .taper_argument_vector(data, species, "species")
  values$group <- .taper_argument_vector(data, group, "group")
  values$weight <- .taper_argument_vector(
    data, weights, "weights", default = rep(1, nrow(data))
  )
  if (!is.numeric(values$weight) || any(!is.finite(values$weight)) ||
        any(values$weight <= 0)) {
    stop("weights must contain finite values above zero.", call. = FALSE)
  }
  if (!is.null(values$species)) {
    if (!is.numeric(values$species)) {
      stop("species must contain positive whole-number FIA species codes.",
        call. = FALSE
      )
    }
  }
  missing <- is.na(values$tree_id) | !is.finite(values$dbh) |
    !is.finite(values$ht) | !is.finite(values$h) | !is.finite(values$dib)
  if (!is.null(values$species)) {
    missing <- missing | !is.finite(values$species)
  }
  if (!is.null(values$group)) {
    group_missing <- is.na(values$group)
    if (is.numeric(values$group)) {
      group_missing <- group_missing | !is.finite(values$group)
    }
    missing <- missing | group_missing
  }
  if (any(missing)) {
    warning(
      "fit_taper(): omitted ", sum(missing), " incomplete measurement ",
      if (sum(missing) == 1L) "row." else "rows.",
      call. = FALSE
    )
    values <- values[!missing, , drop = FALSE]
  }
  values$dbh <- .diameter_to_native(as.double(values$dbh), units, "metric")
  values$ht <- .height_to_native(as.double(values$ht), units, "metric")
  values$h <- .height_to_native(as.double(values$h), units, "metric")
  values$dib <- .diameter_to_native(as.double(values$dib), units, "metric")
  invalid <- values$dbh <= 0 | values$ht <= 0 | values$h < 0 |
    values$h > values$ht | values$dib < 0
  if (form == "kozak_2002") {
    invalid <- invalid | values$ht <= 1.3
  }
  if (any(invalid)) {
    stop(
      "data contain ", sum(invalid), " out-of-domain measurement ",
      if (sum(invalid) == 1L) "row." else "rows.",
      call. = FALSE
    )
  }
  if (!is.null(values$species)) {
    invalid_species <- values$species <= 0 |
      values$species > .Machine$integer.max |
      values$species != floor(values$species)
    if (any(invalid_species)) {
      stop("species must contain positive whole-number FIA species codes.",
        call. = FALSE
      )
    }
    values$species <- as.integer(values$species)
  }
  if (!is.null(values$group)) {
    values$group <- factor(as.character(values$group))
  }
  by_tree <- split(seq_len(nrow(values)), values$tree_id)
  inconsistent <- vapply(by_tree, function(rows) {
    for (name in c("dbh", "ht")) {
      observed <- values[[name]][rows]
      tolerance <- sqrt(.Machine$double.eps) * max(1, abs(observed))
      if (diff(range(observed)) > tolerance) {
        return(TRUE)
      }
    }
    FALSE
  }, logical(1))
  if (any(inconsistent)) {
    stop("dbh and ht must be constant within each tree_id.", call. = FALSE)
  }
  rownames(values) <- NULL
  values
}

.taper_model_formula <- function(form) {
  if (form == "kozak_1988") {
    return(
      dib ~ exp(la0) * dbh^a1 * exp(la2)^dbh *
        ((1 - sqrt(h / ht)) / (1 - sqrt(p_fixed)))^(
          b1 * (h / ht)^2 + b2 * log(h / ht + 0.001) +
            b3 * sqrt(h / ht) + b4 * exp(h / ht) + b5 * dbh / ht
        )
    )
  }
  if (form == "kozak_2002") {
    return(
      dib ~ exp(la0) * dbh^a1 * ht^a2 *
        ((1 - (h / ht)^(1 / 3)) /
           (1 - (1.3 / ht)^(1 / 3)))^(
          b1 * (h / ht)^4 + b2 * exp(-dbh / ht) +
            b3 * ((1 - (h / ht)^(1 / 3)) /
                    (1 - (1.3 / ht)^(1 / 3)))^0.1 +
            b4 / dbh + b5 * ht^(1 - (h / ht)^(1 / 3)) +
            b6 * ((1 - (h / ht)^(1 / 3)) /
                    (1 - (1.3 / ht)^(1 / 3)))
        )
    )
  }
  lower_knot <- quote(0.001 + 0.998 / (1 + exp(-ka2)))
  upper_knot <- bquote(
    .(lower_knot) + (0.999 - .(lower_knot)) / (1 + exp(-kspan))
  )
  stats::as.formula(bquote(
    dib ~ dbh * sqrt(pmax(
      b1 * (h / ht - 1) + b2 * ((h / ht)^2 - 1) +
        b3 * (.(upper_knot) - h / ht)^2 * (h / ht <= .(upper_knot)) +
        b4 * (.(lower_knot) - h / ht)^2 * (h / ht <= .(lower_knot)),
      1e-12
    ))
  ))
}

.taper_fit_nls <- function(data, form, starts) {
  last_message <- "unknown nonlinear least-squares failure"
  for (attempt in seq_along(starts)) {
    fit_data <- data
    fit_start <- starts[[attempt]]
    p_internal <- NULL
    if (form == "kozak_1988") {
      p_internal <- fit_start[["lp"]]
      fit_data$p_fixed <- .taper_fit_p(p_internal)
      fit_start <- fit_start[.taper_internal_names(form)]
    }
    fit <- tryCatch(
      suppressWarnings(do.call(
        stats::nls,
        list(
          formula = .taper_model_formula(form), data = fit_data,
          start = as.list(fit_start), weights = fit_data$weight,
          algorithm = "port",
          control = stats::nls.control(
            maxiter = 500L, tol = 1e-7, minFactor = 1 / 4096,
            warnOnly = TRUE
          )
        )
      )),
      error = function(error) {
        last_message <<- conditionMessage(error)
        NULL
      }
    )
    stationary <- !is.null(fit) &&
      is.finite(fit$convInfo$finTol) && fit$convInfo$finTol <= 1e-6 &&
      all(is.finite(stats::coef(fit))) &&
      all(is.finite(stats::fitted(fit)))
    if (!is.null(fit) && (isTRUE(fit$convInfo$isConv) || stationary)) {
      return(list(
        fit = fit, attempts = attempt, lp = p_internal,
        optimizer_converged = isTRUE(fit$convInfo$isConv),
        stop_message = fit$convInfo$stopMessage
      ))
    }
  }
  stop(
    "fit_taper(): form '", form,
    "' failed to converge after alternate start values: ", last_message,
    call. = FALSE
  )
}

.taper_fit_nlme <- function(data, form, fixed_start) {
  parameters <- .taper_internal_names(form)
  fixed <- stats::as.formula(paste(paste(parameters, collapse = " + "), "~ 1"))
  random_parameter <- if (form == "max_burkhart") "b1" else "la0"
  random <- stats::as.formula(paste(random_parameter, "~ 1 | group"))
  model_formula <- .taper_model_formula(form)
  if (!all(data$weight == 1)) {
    data$weighted_dib <- sqrt(data$weight) * data$dib
    data$sqrt_weight <- sqrt(data$weight)
    model_formula[[2L]] <- quote(weighted_dib)
    model_formula[[3L]] <- bquote(
      sqrt_weight * .(model_formula[[3L]])
    )
  }
  starts <- list(
    fixed_start,
    fixed_start + stats::setNames(
      ifelse(parameters == random_parameter, 0.05, 0), parameters
    ),
    fixed_start + stats::setNames(
      ifelse(parameters == random_parameter, -0.05, 0), parameters
    )
  )
  last_message <- "unknown nonlinear mixed-effects failure"
  for (attempt in seq_along(starts)) {
    fit <- tryCatch(
      suppressWarnings(nlme::nlme(
        model = model_formula, data = data,
        fixed = fixed, random = random, groups = ~group,
        start = starts[[attempt]],
        na.action = stats::na.fail,
        control = nlme::nlmeControl(
          maxIter = 250L, pnlsMaxIter = 40L, msMaxIter = 250L,
          tolerance = 1e-6, pnlsTol = 1e-5, niterEM = 20L,
          returnObject = FALSE, msWarnNoConv = TRUE, apVar = FALSE
        )
      )),
      error = function(error) {
        last_message <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(fit)) {
      return(list(fit = fit, attempts = attempt, parameter = random_parameter))
    }
  }
  stop(
    "fit_taper(): random-effects fit for form '", form,
    "' failed to converge after alternate start values: ", last_message,
    call. = FALSE
  )
}

.taper_random_effects <- function(model, random_parameter) {
  if (!inherits(model, "nlme")) {
    return(data.frame(group = character(), effect = double()))
  }
  effects <- nlme::ranef(model)
  data.frame(
    group = row.names(effects), effect = as.double(effects[[random_parameter]]),
    stringsAsFactors = FALSE
  )
}

.taper_predict_metric <- function(form, internal, data, random_effects = NULL,
                                  random_parameter = NULL) {
  count <- nrow(data)
  parameters <- lapply(internal, rep, count)
  if (!is.null(random_effects) && nrow(random_effects) &&
        !is.null(data$group)) {
    matched <- match(as.character(data$group), random_effects$group)
    effect <- numeric(count)
    seen <- !is.na(matched)
    effect[seen] <- random_effects$effect[matched[seen]]
    parameters[[random_parameter]] <- parameters[[random_parameter]] + effect
  }
  coefficients <- .taper_internal_to_natural(internal, form)
  if (is.null(random_parameter) || is.null(random_effects) || !nrow(random_effects)) {
    return(.taper_evaluate(
      form, data$dbh, data$ht, data$h, coefficients, stabilize = TRUE
    ))
  }
  do.call(
    switch(form,
      kozak_1988 = .taper_fit_kozak_1988,
      kozak_2002 = .taper_fit_kozak_2002,
      max_burkhart = .taper_fit_max_burkhart
    ),
    c(list(dbh = data$dbh, ht = data$ht, h = data$h), parameters)
  )
}

.taper_fit_statistics <- function(observed, fitted, relative_height) {
  residual <- observed - fitted
  breaks <- seq(0, 1, by = 0.1)
  labels <- sprintf("%.1f-%.1f", utils::head(breaks, -1L), utils::tail(breaks, -1L))
  height_class <- cut(
    relative_height, breaks = breaks, labels = labels,
    include.lowest = TRUE, right = FALSE
  )
  rows <- lapply(levels(height_class), function(level) {
    selected <- which(height_class == level)
    if (!length(selected)) {
      return(NULL)
    }
    data.frame(
      relative_height_class = level,
      n = length(selected),
      rmse = sqrt(mean(residual[selected]^2)),
      bias = mean(residual[selected]),
      stringsAsFactors = FALSE
    )
  })
  by_class <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(by_class) <- NULL
  list(
    overall = data.frame(
      n = length(residual), rmse = sqrt(mean(residual^2)),
      bias = mean(residual)
    ),
    by_relative_height = by_class
  )
}

.taper_validate_fit <- function(fit) {
  required <- c(
    "form", "coefficients", "fit_statistics", "residual_diagnostics",
    "n_trees", "n_measurements", "species", "groups_seen", "units",
    "input_units", "method", "convergence", "model", "internal_coefficients",
    "random_effects", "random_parameter", "data_metric", "package_version"
  )
  if (!inherits(fit, "taper_fit") || !all(required %in% names(fit))) {
    stop("fit must be a taper_fit object returned by fit_taper().",
      call. = FALSE
    )
  }
  fit
}

#' Fit a taper model to stem-analysis data
#'
#' Fit a compiled taper form to repeated stem measurements. Return coefficients, residuals, fit
#' statistics, and the fitted model.
#'
#' @param data Required data frame with one row per diameter measurement and the
#'   fields. Omission or missing required columns is an error. Missing
#'   measurement rows are omitted with a warning.
#'
#' Example: `example_stem_measurements`. Tree measurements must remain consistent within `tree_id`.
#' @param form One character form: `'kozak_2002'` (default), `'kozak_1988'`, or
#'   `'max_burkhart'`. Unitless. Missing or unknown choices are errors.
#'
#' Example: `form = 'max_burkhart'`.
#' @param species Default `NULL` leaves model scope unrestricted. Otherwise supply
#'   a positive whole-number inventory species code, a vector with one code
#'   per measurement, or a column name. Unitless.
#'
#' Missing species rows are
#'   omitted, and invalid finite codes are errors.
#' @param group Default `NULL` fits one common relationship. Otherwise supply a
#'   group label, one atomic value per measurement, or a column name, such as
#'   `'stand'`. Unitless.
#'
#' Missing groups are omitted. At least two distinct
#'   retained groups enable estimated group differences. One group produces
#'   a warning and fits a common relationship.
#' @param weights Default `NULL` weights each measurement equally. Otherwise supply
#'   a numeric value to repeat, one per measurement, or a column name. Weights
#'   must be finite and positive, and missing values are errors.
#'
#' Unitless
#'   relative weights.
#' @param start Default `NULL` derives starting coefficients from the measurements. Otherwise
#'   supply a complete finite named numeric vector in the coefficient
#'   specified order. Missing or incomplete vectors are errors.
#'
#' Coefficients must
#'   use centimeters and meters even with imperial measurement inputs. Example: `c(b1 = -3, b2 =
#' 1.5, b3 = -0.4, b4 = 25, a1 = 0.75, a2 = 0.12)`
#'   for an illustrative Max and Burkhart start.
#' @param units One character value, `'metric'` (default) or `'imperial'`.
#'   Measurements use centimeters and meters or inches and feet, respectively.
#'   Missing or unknown values are errors. Example: `units = 'imperial'`.
#'
#' @details Fit one row per measured diameter and retain tree identifiers so
#'   repeated measurements can be checked together. Inspect the fitted
#'   residuals across stem height and tree size before applying the model.
#'
#' @section Numerical methods:
#' All fits use centimeters and meters, with diameter in centimeters and height in meters.
#' Kozak 1988 estimates the coefficients at a fixed inflection proportion `p`. Its starts are
#' weighted log-linear fits over a grid of candidate inflection proportions.
#'
#' A user-supplied start fixes `p` at that supplied value. Kozak 2002 starts from its weighted
#' log-linear representation. Max and Burkhart starts use a grid of lower and upper join points
#' followed by weighted linear fits of squared relative diameter.
#'
#' Each final nonlinear fit tries up to three starts.
#'
#' Bias is mean observed minus fitted diameter, so positive bias means the model underpredicts.
#' Root mean squared error and bias are stored in centimeters overall and by relative-height class.
#' Mixed-model diagnostics use
#' conditional fitted values.
#'
#' The coefficients used by [as_taper_model()] are the population fixed effects. Some well-
#' scaled fits reach a stationary solution while `nls()` reports its false-convergence code.
#' Acceptance requires a sufficiently small final tolerance and finite coefficients and fitted
#' values. `convergence` retains the optimizer report.
#'
#' @references
#' Kozak A (1988). "A variable-exponent taper equation." *Canadian Journal
#' of Forest Research*, 18(11), 1363-1368. \doi{10.1139/x88-213}.
#'
#' Kozak A (2004). "My last words on taper equations." *The Forestry
#' Chronicle*, 80(4), 507-515. \doi{10.5558/tfc80507-4}.
#'
#' Max TA, Burkhart HE (1976). "Segmented polynomial regression applied to
#' taper equations." *Forest Science*, 22(3), 283-289.
#' \doi{10.1093/forestscience/22.3.283}.
#'
#' @return An object of class `taper_fit` containing fixed coefficients, fit
#'   statistics, residual diagnostics, counts, convergence details, and the
#'   fitted `nls` or `nlme` object.
#' @export
#' @usage
#'
#' ## Call signatures
#' fit_taper(data, form = c('kozak_2002', 'kozak_1988', 'max_burkhart'), species = NULL, group =
#'   NULL, weights = NULL, start = NULL, units = 'metric')
#' @examples
#' ## Fit the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements,
#'                  form = 'max_burkhart',
#'                  units = 'imperial')
#'
#' ## Inspect diameter errors in centimeters
#' fit$fit_statistics$overall
#' @section Measurement table fields:
#' `tree_id` is a required tree label repeated for measurements on one stem. It is unitless,
#' for example `'example-1'`. `tree` is an accepted alias.
#'
#' Required numeric `dbh` is positive outside-bark breast-height diameter, and `ht` is positive
#' total height above ground. `total_height` is an alias for `ht`. Required numeric `h` is
#' measurement height above ground, from zero through `ht`.
#'
#' Its alias is `measurement_height`. Required numeric `dib` is nonnegative inside-bark
#' diameter at that height, with alias `diameter_inside_bark`. Diameters use inches or
#' centimeters, heights use feet or meters.
#'
#' No field has a default. Missing and nonfinite measurements are omitted with a warning. If
#' both a preferred name and its alias occur, the preferred name is used.
#'
#' Other columns matter only when named in `species`, `group`, or `weights`. No list columns
#' are required.
#'
#' @section Fitted taper fields:
#' `coefficients` is a named numeric vector in the form's required order. `form`, `method`,
#' `units`, `input_units`, and `package_version` are character labels. `n_trees` and
#' `n_measurements` are counts.
#'
#' `species` is an integer scope vector and `groups_seen` is a character label vector.
#' `convergence` records logical `converged`, counts `nls_attempts` and `nlme_attempts`,
#' logical `nls_optimizer_converged`, and character `nls_stop_message`. These record numerical
#' completion, not model validity.
#'
#' `fit_statistics$overall` has sample count `n`, root mean squared error `rmse`, and mean error
#' `bias`. The count is unitless, while both
#' error measures use centimeters. `fit_statistics$by_relative_height` has the same fields plus
#' character `relative_height_class`, in tenths of height.
#'
#' Classes include the lower boundary and exclude the upper boundary, except the final class
#' also contains the tip. Bias is observed minus fitted.
#'
#' `residual_diagnostics` has `tree_id`, numeric `dbh`, `ht`, `h`, `relative_height`,
#' `observed_dib`, `fitted_dib`, `population_fitted_dib`, `residual`, and
#' `standardized_residual`. Diameter and residual fields use centimeters, heights use meters,
#' and relative height and standardized residuals are unitless. `species` and `group` appear
#' when supplied.
#'
#' `data_metric` retains fitting fields in centimeters and meters, with numeric `weight` and
#' optional `species`, `group`, and `p_fixed`. `random_effects` contains character `group` and
#' numeric `effect` for group differences, or zero rows. `random_parameter` names the adjusted
#' coefficient or is `NULL`.
#'
#' `model` holds the fitted R model and `internal_coefficients` holds its transformed parameter
#' vector. None is a data-frame list column.
#'
#' @section Status and missing values:
#' No tree status codes are returned. Incomplete measurement rows are omitted with a warning,
#' while invalid finite measurements stop the fit. For Kozak 2002 total height must exceed 1.3
#' meters.
#'
#' The usable measurement count must exceed the fitted parameter count. A fit that cannot
#' converge stops the call. Zero residual variation leaves standardized residuals missing with
#' a warning.
#'
#' Review residuals before applying the model to new trees. These fits do not establish valid
#' zero log volume.
#' @seealso [as_taper_model()] to use fitted coefficients in stem
#' calculations, [taper_model_from_coefficients()] for known coefficients, [taper_fit_methods]
#' for fitted predictions and diagnostic plots.
#' @inheritSection taper_model_from_coefficients Coefficient order and bounds
fit_taper <- function(
    data, form = c("kozak_2002", "kozak_1988", "max_burkhart"),
    species = NULL, group = NULL, weights = NULL, start = NULL,
    units = "metric") {
  form <- match.arg(form)
  units <- .validate_units(units)
  prepared <- .taper_prepare_data(
    data, species, group, weights, units, form
  )
  parameter_count <- length(.published_coefficient_names(form))
  if (nrow(prepared) <= parameter_count) {
    stop(
      "fit_taper(): more than ", parameter_count,
      " complete measurements are required for form '", form, "'.",
      call. = FALSE
    )
  }
  defaults <- .taper_default_starts(prepared, form)
  if (!length(defaults)) {
    stop("fit_taper(): data could not produce finite start values.",
      call. = FALSE
    )
  }
  if (!is.null(start)) {
    start <- .taper_validate_start(start, form)
    defaults <- c(list(start), defaults)
  }
  internal_starts <- lapply(defaults, .taper_natural_to_internal, form = form)
  nls_result <- .taper_fit_nls(prepared, form, internal_starts)
  nls_coefficients <- stats::coef(nls_result$fit)
  if (form == "kozak_1988") {
    prepared$p_fixed <- .taper_fit_p(nls_result$lp)
    nls_coefficients <- c(nls_coefficients, lp = unname(nls_result$lp))
  }
  use_random <- !is.null(prepared$group) && nlevels(prepared$group) >= 2L
  if (!is.null(prepared$group) && !use_random) {
    warning(
      "fit_taper(): group has fewer than two levels, so fixed-effects nls was used.",
      call. = FALSE
    )
  }
  if (use_random) {
    mixed <- .taper_fit_nlme(
      prepared, form, nls_coefficients[.taper_internal_names(form)]
    )
    model <- mixed$fit
    internal <- nlme::fixef(model)
    if (form == "kozak_1988") {
      internal <- c(internal, lp = unname(nls_result$lp))
    }
    random_parameter <- mixed$parameter
    random_effects <- .taper_random_effects(model, random_parameter)
    method <- "nlme"
    convergence <- list(
      converged = TRUE, nls_attempts = nls_result$attempts,
      nlme_attempts = mixed$attempts,
      nls_optimizer_converged = nls_result$optimizer_converged,
      nls_stop_message = nls_result$stop_message
    )
  } else {
    model <- nls_result$fit
    internal <- nls_coefficients
    random_parameter <- NULL
    random_effects <- .taper_random_effects(model, random_parameter)
    method <- "nls"
    convergence <- list(
      converged = TRUE, nls_attempts = nls_result$attempts,
      nlme_attempts = 0L,
      nls_optimizer_converged = nls_result$optimizer_converged,
      nls_stop_message = nls_result$stop_message
    )
  }
  coefficients <- .taper_internal_to_natural(internal, form)
  coefficients <- coefficients[.published_coefficient_names(form)]
  population_fitted <- .taper_predict_metric(form, internal, prepared)
  conditional_fitted <- .taper_predict_metric(
    form, internal, prepared, random_effects, random_parameter
  )
  residual <- prepared$dib - conditional_fitted
  residual_sd <- sqrt(sum(residual^2) / max(1, length(residual) - length(internal)))
  standardized <- if (is.finite(residual_sd) && residual_sd > 0) {
    residual / residual_sd
  } else {
    warning(
      "fit_taper(): standardized residuals are undefined because residual variation is zero.",
      call. = FALSE
    )
    rep(NA_real_, length(residual))
  }
  diagnostics <- data.frame(
    tree_id = prepared$tree_id,
    dbh = prepared$dbh,
    ht = prepared$ht,
    h = prepared$h,
    relative_height = prepared$h / prepared$ht,
    observed_dib = prepared$dib,
    fitted_dib = conditional_fitted,
    population_fitted_dib = population_fitted,
    residual = residual,
    standardized_residual = standardized,
    stringsAsFactors = FALSE
  )
  if (!is.null(prepared$species)) {
    diagnostics$species <- prepared$species
  }
  if (!is.null(prepared$group)) {
    diagnostics$group <- as.character(prepared$group)
  }
  package_version <- tryCatch(tv_version(), error = function(error) NA_character_)
  structure(
    list(
      form = form,
      coefficients = coefficients,
      fit_statistics = .taper_fit_statistics(
        prepared$dib, conditional_fitted, prepared$h / prepared$ht
      ),
      residual_diagnostics = diagnostics,
      n_trees = length(unique(prepared$tree_id)),
      n_measurements = nrow(prepared),
      species = if (is.null(prepared$species)) {
        integer()
      } else {
        sort(unique(prepared$species))
      },
      groups_seen = if (is.null(prepared$group)) {
        character()
      } else {
        levels(prepared$group)
      },
      units = "metric",
      input_units = units,
      method = method,
      convergence = convergence,
      model = model,
      internal_coefficients = internal,
      random_effects = random_effects,
      random_parameter = random_parameter,
      data_metric = prepared,
      package_version = package_version
    ),
    class = "taper_fit"
  )
}

#' Inspect fitted diameters and taper residuals
#'
#' Predict diameters, summarize fit statistics, or draw fitted diameters and residuals from a
#' taper fit.
#'
#' @param x,object Required complete `taper_fit` from [fit_taper()]. No default. Missing or other
#' objects are errors.
#'
#' Example: `fit`.
#' @param newdata Data frame with one row per predicted diameter, default `NULL`
#'   to use retained fitting measurements. Required numeric columns are positive
#'   `dbh` and `ht`, and `h` from zero through `ht`. Height aliases accepted by
#'   [fit_taper()] also work.
#'
#' Missing or invalid rows are errors. Kozak 2002
#'   requires total height above 1.3 meters. Units follow `units`.
#'
#'
#' @param re_form One character value, `'conditional'` (default) to include known
#'   group differences, or `'population'` for the common relationship. Unitless. Missing or unknown
#' choices are errors.
#'
#' Example: `'population'`.
#' @param group Atomic group labels, one to repeat or one per prediction, or a
#'   column name such as `'stand'`. Default `NULL` uses the retained or new-data
#'   group column when available. Missing or unknown groups use the common
#'   fitted relationship.
#'
#' Labels are unitless.
#' @param units One character value, `'imperial'` or `'metric'`, defaulting to the
#'   fit's original input units. Imperial uses inches and feet, metric centimeters
#'   and meters. With `newdata = NULL`, only output diameter units change.
#'
#' Missing or unknown choices are errors. Example: `units = 'metric'`.
#' @param ... Named graphical settings for the plot method, for example
#'   `cex = 0.7`. Omission uses its default points and axes.
#'
#' Plot coordinates are
#'   relative height and relative diameter, both unitless. Missing graphical
#'   settings follow base graphics behavior. The other methods ignore dots.
#' @details Predictions use the common fitted relationship plus group differences
#'   when requested and available. Plots compare observed and fitted relative
#'   diameters across relative height. They use retained measurements, not new
#'   field observations.
#' @return `predict()` returns a numeric diameter vector in inches or centimeters. `summary()`
#'   returns
#'   a `summary.taper_fit` object. The print and plot methods return `x`
#'   invisibly.
#' @name taper_fit_methods
#' @inheritSection fit_taper Measurement table fields
#' @inheritSection fit_taper Fitted taper fields
#' @section Status and missing values:
#' No status codes are added. Missing or invalid new diameter and height
#' measurements stop prediction. With `newdata = NULL`, retained fitting
#' measurements are used.
#'
#' Unknown or missing groups use the common fitted
#' relationship. Plotting displays retained fitting measurements only.
#' @seealso [fit_taper()] to estimate the relationship, [as_taper_model()]
#'   to use its common coefficients in volume calculations.
#' @usage
#'
#' ## Predict diameters from a taper fit
#' \method{predict}{taper_fit}(object, newdata = NULL, re_form = c('conditional', 'population'),
#'   group = NULL, units = object$input_units, ...)
#'
#' ## Draw retained measurements and fitted diameters
#' \method{plot}{taper_fit}(x, ...)
#' @examples
#' ## Fit the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements,
#'                  form = 'max_burkhart',
#'                  units = 'imperial')
#'
#' ## Draw fitted diameters and residuals
#' plot(fit)
NULL

#' @rdname taper_fit_methods
#' @export
#' @usage NULL
print.taper_fit <- function(x, ...) {
  x <- .taper_validate_fit(x)
  cat("<taper_fit>\n")
  cat("  form: ", x$form, "\n", sep = "")
  cat("  method: ", x$method, "\n", sep = "")
  cat("  trees: ", x$n_trees, "\n", sep = "")
  cat("  measurements: ", x$n_measurements, "\n", sep = "")
  cat(
    "  RMSE: ", format(x$fit_statistics$overall$rmse, digits = 5),
    " cm\n", sep = ""
  )
  invisible(x)
}

#' @rdname taper_fit_methods
#' @export
#' @usage NULL
summary.taper_fit <- function(object, ...) {
  object <- .taper_validate_fit(object)
  structure(
    list(
      form = object$form,
      method = object$method,
      coefficients = object$coefficients,
      fit_statistics = object$fit_statistics,
      n_trees = object$n_trees,
      n_measurements = object$n_measurements,
      species = object$species,
      groups_seen = object$groups_seen,
      convergence = object$convergence,
      residual_quantiles = stats::quantile(
        object$residual_diagnostics$residual,
        c(0, 0.25, 0.5, 0.75, 1), names = TRUE
      )
    ),
    class = "summary.taper_fit"
  )
}

#' @rdname taper_fit_methods
#' @export
#' @usage NULL
print.summary.taper_fit <- function(x, ...) {
  cat("Taper model fit\n")
  cat("Form: ", x$form, "\n", sep = "")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Trees: ", x$n_trees, "\n", sep = "")
  cat("Measurements: ", x$n_measurements, "\n\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("\nOverall fit statistics in centimeters:\n")
  print(x$fit_statistics$overall, row.names = FALSE)
  cat("\nFit statistics by relative-height class:\n")
  print(x$fit_statistics$by_relative_height, row.names = FALSE)
  invisible(x)
}

.taper_predict_data <- function(object, newdata, units, group) {
  if (is.null(newdata)) {
    data <- object$data_metric
    if (!is.null(group)) {
      data$group <- factor(
        .taper_argument_vector(data, group, "group"),
        levels = object$groups_seen
      )
    }
    return(data)
  }
  choices <- list(
    dbh = "dbh",
    ht = c("ht", "total_height"),
    h = c("h", "measurement_height")
  )
  columns <- vapply(choices, function(column_names) {
    found <- column_names[column_names %in% names(newdata)]
    if (length(found)) found[[1L]] else NA_character_
  }, character(1))
  if (anyNA(columns)) {
    stop("newdata must contain dbh, ht, and h columns.", call. = FALSE)
  }
  if (any(!vapply(newdata[columns], is.numeric, logical(1)))) {
    stop("newdata dbh, ht, and h columns must be numeric.", call. = FALSE)
  }
  data <- data.frame(
    dbh = .diameter_to_native(
      as.double(newdata[[columns[["dbh"]]]]), units, "metric"
    ),
    ht = .height_to_native(
      as.double(newdata[[columns[["ht"]]]]), units, "metric"
    ),
    h = .height_to_native(
      as.double(newdata[[columns[["h"]]]]), units, "metric"
    ),
    stringsAsFactors = FALSE
  )
  group_value <- if (!is.null(group)) {
    .taper_argument_vector(newdata, group, "group")
  } else if ("group" %in% names(newdata)) {
    newdata$group
  } else {
    NULL
  }
  if (!is.null(group_value)) {
    data$group <- factor(as.character(group_value), levels = object$groups_seen)
  }
  invalid <- !is.finite(data$dbh) | !is.finite(data$ht) |
    !is.finite(data$h) | data$dbh <= 0 | data$ht <= 0 |
    data$h < 0 | data$h > data$ht
  if (object$form == "kozak_2002") {
    invalid <- invalid | data$ht <= 1.3
  }
  if (any(invalid)) {
    stop("newdata contain out-of-domain prediction rows.", call. = FALSE)
  }
  data
}

#' @rdname taper_fit_methods
#' @export
#' @usage NULL
predict.taper_fit <- function(
    object, newdata = NULL, re_form = c("conditional", "population"),
    group = NULL, units = object$input_units, ...) {
  object <- .taper_validate_fit(object)
  re_form <- match.arg(re_form)
  units <- .validate_units(units)
  data <- .taper_predict_data(object, newdata, units, group)
  conditional <- re_form == "conditional" && nrow(object$random_effects) > 0L
  result <- .taper_predict_metric(
    object$form, object$internal_coefficients, data,
    if (conditional) object$random_effects else NULL,
    if (conditional) object$random_parameter else NULL
  )
  .diameter_from_native(result, units, "metric")
}

#' @rdname taper_fit_methods
#' @export
#' @usage NULL
plot.taper_fit <- function(x, ...) {
  x <- .taper_validate_fit(x)
  diagnostics <- x$residual_diagnostics
  observed <- diagnostics$observed_dib / diagnostics$dbh
  fitted <- diagnostics$fitted_dib / diagnostics$dbh
  graphics::plot(
    diagnostics$relative_height, observed,
    xlab = "Relative height", ylab = "Relative diameter",
    col = grDevices::adjustcolor("black", alpha.f = 0.35), pch = 16,
    ...
  )
  graphics::points(
    diagnostics$relative_height, fitted,
    col = grDevices::adjustcolor("firebrick", alpha.f = 0.55), pch = 1
  )
  graphics::legend(
    "topright", legend = c("Observed", "Fitted"),
    col = c("black", "firebrick"), pch = c(16, 1), bty = "n"
  )
  invisible(x)
}

#' Use a fitted taper relationship in stem calculations
#'
#' Convert fitted population coefficients into a taper model for session registration.
#'
#' @param x Required complete `taper_fit` from [fit_taper()]. No default. Missing or unsupported
#'   objects are errors.
#'
#' Example: `fit`. Its measurement conventions are retained during conversion.
#' @param id Required single nonempty character equation name, unitless. Missing and empty names
#' are errors. Example: `'example.fitted'`.
#'
#' Registration additionally requires a distinct local name containing a dot.
#' @param species Positive whole-number Forest Inventory and Analysis species
#'   codes supplied as an integer
#'   or double vector. The fitted scope is the default for a `taper_fit`, and
#'   `integer()` means unrestricted. Values are normalized to integers during
#'   validation.
#'
#' Missing or invalid codes are errors. Unitless.
#' @param stump_ht One finite nonnegative numeric stump height in meters. Default
#'   `0` means ground. Missing is an error.
#' @param bark_ratio One numeric ratio of inside-bark to outside-bark diameter,
#'   strictly greater than zero and at most one. Default `NA_real_` supplies
#'   no bark ratio. Unitless.
#' @param source Source text. The form citation and fitting origin are used by
#'   default.
#' @param notes One character note, unitless. Empty text is allowed, missing text
#'   is an error. The coefficient conversion defaults to `''`.
#'
#' Conversion
#'   from a fit defaults to `'Population fixed effects from fit_taper().'`,
#'   identifying the common fitted relationship. Example: `notes = 'Local fit'`.
#' @param ... Additional named arguments for a class method. The supplied
#'   `taper_fit` method ignores them, including missing values.
#'
#' Omit them. Use the named options above, for example `bark_ratio = 0.9`.
#'
#' @return A validated `taper_model`.
#' @export
#' @usage
#'
#' ## Call signatures
#' as_taper_model(x, id, ...)
#'
#' \method{as_taper_model}{taper_fit}(x, id, species = x$species, stump_ht = 0, bark_ratio =
#'   NA_real_, source = NULL, notes = 'Population fixed effects from fit_taper().', ...)
#' @examples
#' ## Fit the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements,
#'                  form = 'max_burkhart',
#'                  units = 'imperial')
#'
#' ## Convert population coefficients to a stem model
#' as_taper_model(x = fit, id = 'example.fitted', bark_ratio = 0.9)
#' @inheritSection new_taper_model Taper model fields
#' @inheritSection new_taper_model Equation function inputs and returns
#' @section Status and missing values:
#' These functions return no tree codes. Missing or invalid coefficients,
#' identifiers, species codes, or conventions stop construction. An omitted
#' bark ratio leaves outside-bark calculations unavailable unless a later
#' call supplies one.
#'
#' Register the object before using its name.
#' @seealso [fit_taper()] to estimate coefficients, [register_taper_model()]
#'   to make the returned equation available, [dib()] to calculate diameter.
#' @param source One nonmissing character source description, unitless. Default `NULL` supplies the
#'   equation-form citation and, for a converted
#'   fit, its fitting origin. An empty string is allowed.
#'
#' Example:
#'   `source = 'Illustrative coefficient set'`. Missing text is an error.
as_taper_model <- function(x, id, ...) {
  UseMethod("as_taper_model")
}

#' @rdname as_taper_model
#' @export
#' @usage NULL
as_taper_model.taper_fit <- function(
    x, id, species = x$species, stump_ht = 0, bark_ratio = NA_real_,
    source = NULL, notes = "Population fixed effects from fit_taper().", ...) {
  x <- .taper_validate_fit(x)
  species <- .normalize_species_codes(species)
  if (is.null(source)) {
    source <- paste(
      .published_taper_source(x$form),
      "Coefficients fitted by fit_taper()."
    )
  }
  taper_model_from_coefficients(
    id = id, form = x$form, coefficients = x$coefficients,
    species = species, stump_ht = stump_ht,
    bark_ratio = bark_ratio, source = source, notes = notes
  )
}
