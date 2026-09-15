.published_taper_forms <- c("kozak_2002", "kozak_1988", "max_burkhart")

.published_coefficient_names <- function(form) {
  switch(form,
    kozak_1988 = c("a0", "a1", "a2", paste0("b", 1:5), "p"),
    kozak_2002 = c(
      "a0",
      "a1", "a2", paste0("b", 1:6)
    ),
    max_burkhart = c(paste0("b", 1:4), "a1", "a2")
  )
}

.published_taper_source <- function(form) {
  switch(form,
    kozak_1988 = paste(
      "Kozak, A. (1988). A variable-exponent taper equation.",
      "Canadian Journal of Forest Research 18(11):1363-1368.", "doi:10.1139/x88-213."
    ),
    kozak_2002 = paste(
      "Kozak, A. (2004). My last words on taper equations.",
      "The Forestry Chronicle 80(4):507-515.", "doi:10.5558/tfc80507-4."
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
  if (!is.numeric(coefficients) || !is.null(dim(coefficients)) || !identical(
    names(coefficients),
    expected
  ) || any(!is.finite(coefficients))) {
    stop("coefficients must be a finite named coefficient vector in this order: ", paste(expected,
           collapse = ", "
         ), ".", call. = FALSE)
  }
  if (form == "kozak_1988" && (coefficients[["a0"]] <= 0 ||
                                 coefficients[["a2"]] <= 0 || coefficients[["p"]] <=
                                 0 || coefficients[["p"]] >= 1)) {
    stop("Kozak 1988 requires a0 and a2 above zero and p in (0, 1).", call. = FALSE)
  }
  if (form == "kozak_2002" && coefficients[["a0"]] <= 0) {
    stop("Kozak 2002 requires a0 above zero.", call. = FALSE)
  }
  if (form == "max_burkhart" && (coefficients[["a2"]] <= 0 ||
                                   coefficients[["a2"]] >= coefficients[["a1"]] ||
                                   coefficients[["a1"]] >= 1)) {
    stop("Max and Burkhart requires 0 < a2 < a1 < 1.", call. = FALSE)
  }
  invisible(coefficients)
}

#' Create a taper model from a coefficient set
#'
#' @param id The identifier names the taper model in the registry. Character scalar model
#'   identifier. Required, with no default.
#' @param form The equation form chooses the relationship to fit. Character scalar, equation name.
#'   Required, with no default.
#' @param coefficients The coefficients supply the published taper equation values. Named numeric
#'   vector. Required, with no default.
#' @param spcd The species codes limit the trees this model covers. Numeric vector of species
#'   codes. Default: \code{integer()}.
#' @param stump_ht Stump height above ground. Numeric
#'   scalar, feet. Default: \code{1}.
#' @param bark_ratio The bark ratio estimates inside diameter from outside diameter when needed.
#'   Numeric scalar, inside diameter divided by outside diameter. Default: \code{NA_real_}.
#' @param source The source records where the model came from. Character scalar, provenance text.
#'   Default: \code{NULL}.
#' @return A taper_model object with the supplied coefficients and provenance, ready for
#'   register_taper_model().
#' @usage
#' taper_model_from_coefficients(
#'   id,
#'   form,
#'   coefficients,
#'   spcd = integer(),
#'   stump_ht = 1,
#'   bark_ratio = NA_real_,
#'   source = NULL
#' )
#' @export
#' @examples
#' ## Obtain coefficients from the shipped synthetic stem measurements.
#' fitted <- fit_taper(
#'   tree_id = example_stem_measurements$tree_id,
#'   dbh = example_stem_measurements$dbh,
#'   ht = example_stem_measurements$ht,
#'   h = example_stem_measurements$h,
#'   dib = example_stem_measurements$dib,
#'   spcd = example_stem_measurements$spcd,
#'   form = 'max_burkhart'
#' )
#'
#' ## Create a model from that coefficient set.
#' local_model <- taper_model_from_coefficients(id = 'example.coefficients',
#'                                              form = 'max_burkhart',
#'                                              coefficients = fitted$coefficients)
#'
#' ## Inspect the local model
#' print(local_model)
taper_model_from_coefficients <- function(
  id,
  form,
  coefficients,
  spcd = integer(),
  stump_ht = 1,
  bark_ratio = NA_real_,
  source = NULL
) {
  id <- .scalar_character(id, "id")
  form <- .scalar_character(form, "form", .published_taper_forms)
  .validate_published_taper_data(form, coefficients)
  if (!is.numeric(stump_ht) || length(stump_ht) != 1L || !is.finite(stump_ht) || stump_ht < 0) {
    stop("stump_ht must be one finite nonnegative number.", call. = FALSE)
  }
  coefficient_names <- names(coefficients)
  coefficients <- stats::setNames(as.double(coefficients), coefficient_names)
  spcd <- .normalize_species_codes(spcd)
  if (is.null(source)) {
    source <- .published_taper_source(form)
  }
  source <- .scalar_character(source, "source", allow_empty = TRUE)
  model <- new_stem_model_unchecked(id = id, form = form, kernel = list(
    type = "compiled",
    key = paste0("published:", form), has_dob = FALSE, has_inverse = identical(
      form,
      "max_burkhart"
    ),
    has_integral = identical(form, "max_burkhart")
  ), inputs = list(
    required = character(),
    optional = "bark_ratio", pairs = list()
  ), measurement_system = "metric", spcd = spcd, stump_ht = stump_ht *
    0.3048, bark_ratio = bark_ratio, source = source, data = coefficients)
  .validate_taper_model(model)
}

.taper_kozak_1988 <- function(dbh, ht, h, coefficients) {
  output <- numeric(length(dbh))
  selected <- h < ht
  z <- h[selected] / ht[selected]
  x <- (1 - sqrt(z)) / (1 - sqrt(coefficients[["p"]]))
  exponent <- coefficients[["b1"]] * z^2 + coefficients[["b2"]] * log(z + 0.001) +
    coefficients[["b3"]] *
      sqrt(z) + coefficients[["b4"]] * exp(z) + coefficients[["b5"]] * dbh[selected] /
    ht[selected]
  output[selected] <- coefficients[["a0"]] * dbh[selected]^coefficients[["a1"]] *
    coefficients[["a2"]]^dbh[selected] *
    x^exponent
  output
}

.taper_kozak_2002 <- function(dbh, ht, h, coefficients) {
  output <- numeric(length(dbh))
  selected <- h < ht
  z <- h[selected] / ht[selected]
  q <- 1 - z^(1 / 3)
  x <- q / (1 - (1.3 / ht[selected])^(1 / 3))
  exponent <- coefficients[["b1"]] * z^4 + coefficients[["b2"]] * exp(-
    dbh[
      selected
    ] /
    ht[selected]) +
    coefficients[["b3"]] * x^0.1 + coefficients[["b4"]] / dbh[selected] +
    coefficients[["b5"]] *
      ht[selected]^q + coefficients[["b6"]] * x
  output[selected] <- coefficients[["a0"]] * dbh[selected]^coefficients[["a1"]] *
    ht[selected]^coefficients[["a2"]] *
    x^exponent
  output
}

.taper_max_burkhart <- function(dbh, ht, h, coefficients, stabilize = FALSE) {
  x <- h / ht
  squared_ratio <- coefficients[["b1"]] * (x - 1) + coefficients[["b2"]] * (x^2 -
                                                                              1) +
    coefficients[["b3"]] *
      (coefficients[["a1"]] - x)^2 * (x <= coefficients[["a1"]]) +
    coefficients[["b4"]] * (coefficients[["a2"]] -
                              x)^2 * (x <= coefficients[["a2"]])
  if (stabilize) {
    squared_ratio <- pmax(squared_ratio, 1e-12)
  }
  output <- dbh * sqrt(squared_ratio)
  output[h == ht] <- 0
  output
}

.taper_evaluate <- function(form, dbh, ht, h, coefficients, stabilize = FALSE) {
  switch(form,
    kozak_1988 = .taper_kozak_1988(dbh, ht, h, coefficients),
    kozak_2002 = .taper_kozak_2002(
      dbh,
      ht, h, coefficients
    ),
    max_burkhart = .taper_max_burkhart(dbh, ht, h, coefficients, stabilize = stabilize)
  )
}

.taper_fit_p <- function(lp) {
  0.05 + 0.3 * stats::plogis(lp)
}

.taper_fit_knots <- function(ka2, kspan) {
  a2 <- 0.001 + 0.998 * stats::plogis(ka2)
  a1 <- a2 + (0.999 - a2) * stats::plogis(kspan)
  list(a1 = a1, a2 = a2)
}

.taper_fit_kozak_1988 <- function(dbh, ht, h, la0, a1, la2, b1, b2, b3, b4, b5, lp) {
  z <- h / ht
  x <- (1 - sqrt(z)) / (1 - sqrt(.taper_fit_p(lp)))
  exponent <- b1 * z^2 + b2 * log(z + 0.001) + b3 * sqrt(z) + b4 * exp(z) + b5 * dbh / ht
  result <- exp(la0) * dbh^a1 * exp(la2)^dbh * x^exponent
  result[h == ht] <- 0
  result
}

.taper_fit_kozak_2002 <- function(dbh, ht, h, la0, a1, a2, b1, b2, b3, b4, b5, b6) {
  z <- h / ht
  q <- 1 - z^(1 / 3)
  x <- q / (1 - (1.3 / ht)^(1 / 3))
  exponent <- b1 * z^4 + b2 * exp(-dbh / ht) + b3 * x^0.1 + b4 / dbh + b5 * ht^q + b6 * x
  result <- exp(la0) * dbh^a1 * ht^a2 * x^exponent
  result[h == ht] <- 0
  result
}

.taper_fit_max_burkhart <- function(dbh, ht, h, b1, b2, b3, b4, ka2, kspan) {
  knots <- .taper_fit_knots(ka2, kspan)
  x <- h / ht
  squared_ratio <- b1 * (x - 1) + b2 * (x^2 - 1) + b3 * (knots$a1 - x)^2 * (x <= knots$a1) +
    b4 * (knots$a2 - x)^2 * (x <= knots$a2)
  result <- dbh * sqrt(pmax(squared_ratio, 1e-12))
  result[h == ht] <- 0
  result
}

.taper_internal_names <- function(form) {
  switch(form,
    kozak_1988 = c("la0", "a1", "la2", paste0("b", 1:5)),
    kozak_2002 = c(
      "la0",
      "a1", "a2", paste0("b", 1:6)
    ),
    max_burkhart = c(paste0("b", 1:4), "ka2", "kspan")
  )
}

.taper_natural_to_internal <- function(coefficients, form) {
  if (form == "kozak_1988") {
    if (coefficients[["p"]] <= 0.05 || coefficients[["p"]] >= 0.35) {
      stop("a fit start for Kozak 1988 requires p in (0.05, 0.35).", call. = FALSE)
    }
    return(c(
      la0 = log(coefficients[["a0"]]), a1 = coefficients[["a1"]], la2 = log(
        coefficients[["a2"]]
      ),
      coefficients[paste0("b", 1:5)], lp = stats::qlogis((coefficients[["p"]] - 0.05) / 0.3)
    ))
  }
  if (form == "kozak_2002") {
    return(c(
      la0 = log(coefficients[["a0"]]), coefficients[c("a1", "a2")],
      coefficients[paste0(
        "b",
        1:6
      )]
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
      a0 = exp(coefficients[["la0"]]), a1 = coefficients[["a1"]], a2 = exp(
        coefficients[["la2"]]
      ),
      coefficients[paste0("b", 1:5)], p = .taper_fit_p(coefficients[["lp"]])
    ))
  }
  if (form == "kozak_2002") {
    return(c(
      a0 = exp(coefficients[["la0"]]), coefficients[c("a1", "a2")],
      coefficients[paste0(
        "b",
        1:6
      )]
    ))
  }
  knots <- .taper_fit_knots(coefficients[["ka2"]], coefficients[["kspan"]])
  c(coefficients[paste0("b", 1:4)], a1 = knots$a1, a2 = knots$a2)
}

.taper_weighted_lm <- function(design, response, weights) {
  fit <- tryCatch(stats::lm.wfit(design, response, weights), error = function(error) NULL)
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- fit$coefficients
  if (length(coefficients) != ncol(design) || any(!is.finite(coefficients))) {
    return(NULL)
  }
  list(coefficients = unname(coefficients), objective = sum(weights * fit$residuals^2))
}

.taper_kozak_1988_starts <- function(data) {
  selected <- data$dib > 0 & data$h < data$ht
  dbh <- data$dbh[selected]
  ht <- data$ht[selected]
  h <- data$h[selected]
  response <- log(data$dib[selected])
  weights <- data$weight[selected]
  candidates <- lapply(c(0.12, 0.16, 0.2, 0.24, 0.28, 0.32), function(p) {
    z <- h / ht
    log_x <- log((1 - sqrt(z)) / (1 - sqrt(p)))
    design <- cbind(
      1, log(dbh), dbh, z^2 * log_x, log(z + 0.001) * log_x, sqrt(z) * log_x,
      exp(z) * log_x, dbh / ht * log_x
    )
    fit <- .taper_weighted_lm(design, response, weights)
    if (is.null(fit)) {
      return(NULL)
    }
    value <- fit$coefficients
    list(coefficients = c(
      a0 = exp(value[[1L]]), a1 = value[[2L]], a2 = exp(value[[3L]]),
      b1 = value[[4L]], b2 = value[[5L]], b3 = value[[6L]], b4 = value[[7L]],
      b5 = value[[8L]],
      p = p
    ), objective = fit$objective)
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
    1, log(dbh), log(ht), z^4 * log_x, exp(-dbh / ht) * log_x, x^0.1 * log_x, log_x / dbh,
    ht^q * log_x, x * log_x
  )
  fit <- .taper_weighted_lm(design, log(data$dib[selected]), data$weight[selected])
  if (is.null(fit)) {
    base <- c(
      a0 = stats::median(data$dib / data$dbh), a1 = 1, a2 = 0, b1 = 0, b2 = 0.5, b3 = 0.5,
      b4 = 0, b5 = 0, b6 = 0
    )
  } else {
    value <- fit$coefficients
    base <- c(
      a0 = exp(value[[1L]]), a1 = value[[2L]], a2 = value[[3L]], b1 = value[[4L]],
      b2 = value[[5L]], b3 = value[[6L]], b4 = value[[7L]], b5 = value[[8L]],
      b6 = value[[9L]]
    )
  }
  list(base, base * c(a0 = 1.05, a1 = 1, a2 = 1, rep(1, 6)), base * c(
    a0 = 0.95, a1 = 1, a2 = 1,
    rep(1, 6)
  ))
}

.taper_max_burkhart_starts <- function(data) {
  x <- data$h / data$ht
  response <- (data$dib / data$dbh)^2
  candidates <- list()
  for (a2 in c(0.05, 0.1, 0.15, 0.2, 0.25)) {
    for (a1 in c(0.55, 0.65, 0.75, 0.85, 0.93)) {
      design <- cbind(x - 1, x^2 - 1, (a1 - x)^2 * (x <= a1), (a2 - x)^2 * (x <= a2))
      fit <- .taper_weighted_lm(design, response, data$weight)
      if (!is.null(fit)) {
        candidates[[length(candidates) + 1L]] <- list(coefficients = c(
          b1 = fit$coefficients[[1L]],
          b2 = fit$coefficients[[2L]], b3 = fit$coefficients[[3L]],
          b4 = fit$coefficients[[4L]],
          a1 = a1, a2 = a2
        ), objective = fit$objective)
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
  if (!is.numeric(start) || !is.null(dim(start)) || length(start) != length(
    expected
  ) || anyDuplicated(names(start)) ||
    !setequal(names(start), expected)) {
    stop("start must be a named numeric vector containing: ", paste(expected,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }
  start <- stats::setNames(as.double(start[expected]), expected)
  .validate_published_taper_data(form, start)
  .taper_natural_to_internal(start, form)
  start
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
    stop(name, " must name a data column or have one value per row.", call. = FALSE)
  }
  value
}

.taper_prepare_data <- function(
  tree_id, dbh, ht, h, dib, spcd, group, weights, measurement_system, form
) {
  inputs <- Filter(Negate(is.null), list(
    tree_id = tree_id,
    dbh = dbh,
    ht = ht,
    h = h,
    dib = dib,
    species = spcd,
    group = group,
    weight = if (is.null(weights)) 1 else weights
  ))
  size <- .common_size(inputs, c(species = "spcd", weight = "weights"))
  values <- as.data.frame(Map(
    function(value, name) .recycle_common(value, size, name),
    inputs,
    names(inputs)
  ), stringsAsFactors = FALSE)
  numeric_names <- c("dbh", "ht", "h", "dib")
  if (any(!vapply(values[numeric_names], is.numeric, logical(1)))) {
    stop("dbh, ht, h, and dib must be numeric.", call. = FALSE)
  }
  if (!is.numeric(values$weight) || any(!is.finite(values$weight)) || any(values$weight <=
                                                                            0)) {
    stop("weights must contain finite values above zero.", call. = FALSE)
  }
  if (!is.null(values$species)) {
    if (!is.numeric(values$species)) {
      stop("spcd must contain positive whole-number FIA species codes.", call. = FALSE)
    }
  }
  missing <- is.na(values$tree_id) | !is.finite(values$dbh) | !is.finite(
    values$ht
  ) | !is.finite(values$h) |
    !is.finite(values$dib)
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
  values$dbh <- .diameter_to_native(as.double(values$dbh), measurement_system, "metric")
  values$ht <- .height_to_native(as.double(values$ht), measurement_system, "metric")
  values$h <- .height_to_native(as.double(values$h), measurement_system, "metric")
  values$dib <- .diameter_to_native(as.double(values$dib), measurement_system, "metric")
  invalid <- values$dbh <= 0 | values$dbh > 400 * 2.54 |
    values$ht <= 0 | values$ht > 500 * 0.3048 | values$h < 0 |
    values$h > values$ht | values$dib < 0 | values$dib > 400 * 2.54
  if (form == "kozak_2002") {
    invalid <- invalid | values$ht <= 1.3
  }
  invalid[is.na(invalid)] <- FALSE
  invalid_species <- rep(FALSE, nrow(values))
  if (!is.null(values$species)) {
    invalid_species <- !values$species %in% species_reference$spcd
  }
  omitted <- missing | invalid | invalid_species
  if (any(omitted)) {
    reasons <- c(
      if (any(missing)) "incomplete measurements",
      if (any(invalid)) "out-of-domain dbh, ht, h, or dib",
      if (any(invalid_species)) "spcd absent from species_reference"
    )
    warning("fit_taper(): omitted ", sum(omitted), " ",
      if (all(omitted == missing)) "incomplete measurement " else "measurement ",
      if (sum(omitted) == 1L) "row" else "rows", ": ",
      paste(reasons, collapse = ", "), ".", call. = FALSE
    )
    values <- values[!omitted, , drop = FALSE]
  }
  if (!nrow(values)) {
    stop("No complete, valid measurement rows remain for dbh, ht, h, dib, and spcd.",
      call. = FALSE
    )
  }
  if (!is.null(values$species)) {
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
    return(dib ~ exp(la0) * dbh^a1 * exp(la2)^dbh * ((1 - sqrt(h / ht)) / (1 - sqrt(
      p_fixed
    )))^(b1 *
           (h / ht)^2 + b2 * log(h / ht + 0.001) + b3 * sqrt(h / ht) + b4 * exp(h / ht) +
           b5 * dbh / ht))
  }
  if (form == "kozak_2002") {
    return(dib ~ exp(la0) * dbh^a1 * ht^a2 * ((1 - (h / ht)^(1 / 3)) / (1 - (1.3 /
                                                                               ht)^(
                                                                                    1 / 3)))^(b1 *
      (h / ht)^4 + b2 * exp(-dbh / ht) + b3 * ((1 - (h / ht)^(1 / 3)) / (1 - (1.3 /
                                                                                ht)^(
                                                                                     1 / 3)))^0.1 +
      b4 / dbh + b5 * ht^(1 - (h / ht)^(1 / 3)) + b6 * ((1 - (h / ht)^(1 / 3)) /
                                                          (1 - (1.3 / ht)^(1 / 3)))))
  }
  lower_knot <- quote(0.001 + 0.998 / (1 + exp(-ka2)))
  upper_knot <- bquote(.(lower_knot) + (0.999 - .(lower_knot)) / (1 + exp(-kspan)))
  stats::as.formula(bquote(dib ~ dbh * sqrt(pmax(b1 * (h / ht - 1) + b2 * ((h /
                                                                              ht)^2 - 1) + b3 *
    (
     .(
       upper_knot
     ) -
       h /
         ht)^2 *
    (
     h /
       ht <= .(
       upper_knot
     )) +
    b4 * (.(lower_knot) -
            h / ht)^2 *
      (h / ht <= .(lower_knot)), 1e-12))))
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
      suppressWarnings(do.call(stats::nls, list(
        formula = .taper_model_formula(form),
        data = fit_data, start = as.list(fit_start), weights = fit_data$weight,
        algorithm = "port",
        control = stats::nls.control(maxiter = 500L, tol = 1e-07, minFactor = 1 /
                                       4096, warnOnly = TRUE)
      ))),
      error = function(error) {
        last_message <<- conditionMessage(error)
        NULL
      }
    )
    stationary <- !is.null(fit) && is.finite(fit$convInfo$finTol) && fit$convInfo$finTol <=
      1e-06 && all(is.finite(stats::coef(fit))) && all(is.finite(stats::fitted(fit)))
    if (!is.null(fit) && (isTRUE(fit$convInfo$isConv) || stationary)) {
      return(list(
        fit = fit, attempts = attempt, lp = p_internal,
        optimizer_converged = isTRUE(fit$convInfo$isConv),
        stop_message = fit$convInfo$stopMessage
      ))
    }
  }
  stop("fit_taper(): form '", form, "' failed to converge after alternate start values: ",
    last_message,
    call. = FALSE
  )
}

.taper_fit_nlme <- function(data, form, fixed_start) {
  parameters <- .taper_internal_names(form)
  fixed <- stats::as.formula(paste(paste(parameters, collapse = " + "), "~ 1"))
  random_parameter <- if (form == "max_burkhart")
    "b1" else "la0"
  random <- stats::as.formula(paste(random_parameter, "~ 1 | group"))
  model_formula <- .taper_model_formula(form)
  if (!all(data$weight == 1)) {
    data$weighted_dib <- sqrt(data$weight) * data$dib
    data$sqrt_weight <- sqrt(data$weight)
    model_formula[[2L]] <- quote(weighted_dib)
    model_formula[[3L]] <- bquote(sqrt_weight * .(model_formula[[3L]]))
  }
  starts <- list(fixed_start, fixed_start + stats::setNames(ifelse(
    parameters == random_parameter,
    0.05, 0
  ), parameters), fixed_start + stats::setNames(ifelse(parameters == random_parameter,
                                                  -0.05, 0
                                                ), parameters))
  last_message <- "unknown nonlinear mixed-effects failure"
  for (attempt in seq_along(starts)) {
    fit <- tryCatch(suppressWarnings(nlme::nlme(
      model = model_formula, data = data, fixed = fixed,
      random = random, groups = ~group, start = starts[[attempt]],
      na.action = stats::na.fail,
      control = nlme::nlmeControl(
        maxIter = 250L, pnlsMaxIter = 40L, msMaxIter = 250L,
        tolerance = 1e-06, pnlsTol = 1e-05, niterEM = 20L, returnObject = FALSE,
        msWarnNoConv = TRUE,
        apVar = FALSE
      )
    )), error = function(error) {
      last_message <<- conditionMessage(error)
      NULL
    })
    if (!is.null(fit)) {
      return(list(fit = fit, attempts = attempt, parameter = random_parameter))
    }
  }
  stop("fit_taper(): random-effects fit for form '", form,
    "' failed to converge after alternate start values: ",
    last_message,
    call. = FALSE
  )
}

.taper_random_effects <- function(model, random_parameter) {
  if (!inherits(model, "nlme")) {
    return(data.frame(group = character(), effect = double()))
  }
  effects <- nlme::ranef(model)
  data.frame(group = row.names(effects), effect = as.double(
    effects[[random_parameter]]
  ), stringsAsFactors = FALSE)
}

.taper_predict_metric <- function(
  form, internal, data, random_effects = NULL,
  random_parameter = NULL
) {
  count <- nrow(data)
  parameters <- lapply(internal, rep, count)
  if (!is.null(random_effects) && nrow(random_effects) && !is.null(data$group)) {
    matched <- match(as.character(data$group), random_effects$group)
    effect <- numeric(count)
    seen <- !is.na(matched)
    effect[seen] <- random_effects$effect[matched[seen]]
    parameters[[random_parameter]] <- parameters[[random_parameter]] + effect
  }
  coefficients <- .taper_internal_to_natural(internal, form)
  if (is.null(random_parameter) || is.null(random_effects) || !nrow(random_effects)) {
    return(.taper_evaluate(form, data$dbh, data$ht, data$h, coefficients, stabilize = TRUE))
  }
  do.call(switch(form,
            kozak_1988 = .taper_fit_kozak_1988,
            kozak_2002 = .taper_fit_kozak_2002,
            max_burkhart = .taper_fit_max_burkhart
          ), c(
            list(dbh = data$dbh, ht = data$ht, h = data$h),
            parameters
          ))
}

.taper_fit_statistics <- function(observed, fitted, relative_height) {
  residual <- .diameter_from_native(
    observed - fitted,
    "imperial",
    "metric"
  )
  breaks <- seq(0, 1, by = 0.1)
  labels <- sprintf("%.1f-%.1f", utils::head(breaks, -1L), utils::tail(breaks, -1L))
  height_class <- cut(relative_height,
    breaks = breaks, labels = labels, include.lowest = TRUE,
    right = FALSE
  )
  rows <- lapply(levels(height_class), function(level) {
    selected <- which(height_class == level)
    if (!length(selected)) {
      return(NULL)
    }
    data.frame(
      relative_height_class = level, n = length(selected), rmse = sqrt(mean(
        residual[selected]^2
      )),
      bias = mean(residual[selected]), stringsAsFactors = FALSE
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
    "form", "coefficients", "fit_statistics", "residual_diagnostics", "n_trees",
    "n_measurements", "spcd", "groups_seen", "measurement_system",
    "input_measurement_system", "method",
    "convergence",
    "model", "internal_coefficients", "random_effects", "random_parameter", "data_metric",
    "package_version"
  )
  if (!inherits(fit, "taper_fit") || !all(required %in% names(fit))) {
    stop("fit must be a taper_fit object returned by fit_taper().", call. = FALSE)
  }
  fit
}

#' Fit a taper model to stem-analysis data
#'
#' @param tree_id Identify each measurement's tree. Atomic vector, unitless, required.
#' @param dbh Diameter at breast height outside bark. Numeric vector, inches, required.
#' @param ht Total tree height above ground. Numeric vector, feet, required.
#' @param h Measurement height above ground. Numeric vector, feet, required.
#' @param dib Measured inside bark diameter. Numeric vector, inches, required.
#' @param spcd Species code of each measurement's tree. Numeric vector, required.
#'   The fitted species scope is the sorted unique set of measured codes.
#' @param form Equation form to fit. Character scalar, default `'max_burkhart'`.
#'   Choices are `'max_burkhart'`, `'kozak_1988'`, and `'kozak_2002'`.
#' @param group Trees in the same group share a local height or taper adjustment. Atomic vector of
#'   group identifiers. Default: \code{NULL}.
#' @param weights The weights control the contribution of each measured diameter to the fit.
#'   Numeric vector, relative weights. Default: \code{NULL}.
#' @param start Starting coefficients give the fitting routine an initial estimate. Named numeric
#'   vector. Default: \code{NULL}.
#' @return A `taper_fit` object with coefficients, form, species scope, fitting method,
#'   convergence details, the fitted model, and these diagnostic tables.
#'   `residual_diagnostics` has `tree_id` (identifier), `dbh`, `observed_dib`,
#'   `fitted_dib`, `population_fitted_dib`, and `residual` (inches), `ht` and `h`
#'   (feet), `relative_height` (height fraction), and `standardized_residual`
#'   (unitless). Optional `group` retains the supplied group identifiers.
#'   `fit_statistics$overall` has `n` (measurement count), `rmse` (root mean squared
#'   error, inches), and `bias` (mean residual, inches).
#'   `fit_statistics$by_relative_height` adds `relative_height_class` (height-fraction
#'   interval) to those columns. `random_effects` has `group` (identifier) and `effect`
#'   (adjustment on the fitted parameter's internal scale).
#'   `data_metric` is the internal metric copy the fit uses for prediction. It has
#'   `tree_id`, `dbh`, `dib` (centimeters), `ht`, `h` (meters), and
#'   `weight` (relative weight), with optional `group` and `p_fixed`
#'   (relative inflection height). Published equations retain their native metric
#'   coefficients. Inputs, predictions, and diagnostics use inches and feet.
#' @usage
#' fit_taper(
#'   tree_id,
#'   dbh,
#'   ht,
#'   h,
#'   dib,
#'   spcd,
#'   form = 'max_burkhart',
#'   group = NULL,
#'   weights = NULL,
#'   start = NULL
#' )
#' @details
#' Invalid rows are omitted with one warning giving the count and reasons. Diameters
#'   must be at most 400 inches and total heights at most 500 feet. Species must occur
#'   in `species_reference`. Taper measurement heights must lie within the tree.
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Fit the shipped synthetic stem measurements
#' fit <- fit_taper(
#'   tree_id = example_stem_measurements$tree_id,
#'   dbh = example_stem_measurements$dbh,
#'   ht = example_stem_measurements$ht,
#'   h = example_stem_measurements$h,
#'   dib = example_stem_measurements$dib,
#'   spcd = example_stem_measurements$spcd,
#'   form = 'max_burkhart'
#' )
#'
#' ## Show fit statistics with units
#' fit$fit_statistics$overall %>%
#'   rename(`measurements (count)` = n,
#'          `root mean squared error (inches)` = rmse,
#'          `mean residual (inches)` = bias)
fit_taper <- function(
  tree_id,
  dbh,
  ht,
  h,
  dib,
  spcd,
  form = "max_burkhart",
  group = NULL,
  weights = NULL,
  start = NULL
) {
  form <- .scalar_character(form, "form", .published_taper_forms)
  if (!is.numeric(spcd)) {
    stop("spcd must contain numeric species codes.", call. = FALSE)
  }
  measurement_system <- "imperial"
  prepared <- .taper_prepare_data(
    tree_id, dbh, ht, h, dib, spcd, group, weights, measurement_system, form
  )
  species_scope <- sort(unique(prepared$species))
  parameter_count <- length(.published_coefficient_names(form))
  if (nrow(prepared) <= parameter_count) {
    stop("fit_taper(): more than ", parameter_count,
      " complete measurements are required for form '",
      form, "'.",
      call. = FALSE
    )
  }
  defaults <- .taper_default_starts(prepared, form)
  if (!length(defaults)) {
    stop("fit_taper(): data could not produce finite start values.", call. = FALSE)
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
    warning("fit_taper(): group has fewer than two levels, so fixed-effects nls was used.",
      call. = FALSE
    )
  }
  if (use_random) {
    mixed <- .taper_fit_nlme(prepared, form, nls_coefficients[.taper_internal_names(form)])
    model <- mixed$fit
    internal <- nlme::fixef(model)
    if (form == "kozak_1988") {
      internal <- c(internal, lp = unname(nls_result$lp))
    }
    random_parameter <- mixed$parameter
    random_effects <- .taper_random_effects(model, random_parameter)
    method <- "nlme"
    convergence <- list(
      converged = TRUE, nls_attempts = nls_result$attempts, nlme_attempts = mixed$attempts,
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
      converged = TRUE, nls_attempts = nls_result$attempts, nlme_attempts = 0L,
      nls_optimizer_converged = nls_result$optimizer_converged,
      nls_stop_message = nls_result$stop_message
    )
  }
  coefficients <- .taper_internal_to_natural(internal, form)
  coefficients <- coefficients[.published_coefficient_names(form)]
  population_fitted <- .taper_predict_metric(form, internal, prepared)
  conditional_fitted <- .taper_predict_metric(
    form, internal, prepared,
    random_effects, random_parameter
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
  diameter_columns <- c(
    "dbh", "observed_dib", "fitted_dib", "population_fitted_dib", "residual"
  )
  diagnostics[diameter_columns] <- lapply(
    diagnostics[diameter_columns],
    .diameter_from_native,
    caller_units = "imperial",
    native_units = "metric"
  )
  diagnostics[c("ht", "h")] <- lapply(
    diagnostics[c("ht", "h")],
    .height_from_native,
    caller_units = "imperial",
    native_units = "metric"
  )
  if (!is.null(prepared$species)) {
    diagnostics$spcd <- prepared$species
  }
  if (!is.null(prepared$group)) {
    diagnostics$group <- as.character(prepared$group)
  }
  package_version <- tryCatch(tv_version(), error = function(error) NA_character_)
  structure(list(
    form = form, coefficients = coefficients, fit_statistics = .taper_fit_statistics(
      prepared$dib,
      conditional_fitted, prepared$h / prepared$ht
    ), residual_diagnostics = diagnostics, n_trees = length(unique(prepared$tree_id)),
    n_measurements = nrow(prepared), spcd = species_scope,
    groups_seen = if (is.null(prepared$group)) {
      character()
    } else {
      levels(prepared$group)
    },
    measurement_system = "metric",
    input_measurement_system = measurement_system, method = method, convergence = convergence,
    model = model, internal_coefficients = internal,
    random_effects = random_effects, random_parameter = random_parameter,
    data_metric = prepared, package_version = package_version
  ), class = "taper_fit")
}

NULL

#' @export
print.taper_fit <- function(x, ...) {
  x <- .taper_validate_fit(x)
  cat("<taper_fit>\n")
  cat("  form: ", x$form, "\n", sep = "")
  cat("  method: ", x$method, "\n", sep = "")
  cat("  trees: ", x$n_trees, "\n", sep = "")
  cat("  measurements: ", x$n_measurements, "\n", sep = "")
  cat("  RMSE: ",
    format(x$fit_statistics$overall$rmse, digits = 5),
    " inches\n",
    sep = ""
  )
  invisible(x)
}

#' @export
summary.taper_fit <- function(object, ...) {
  object <- .taper_validate_fit(object)
  structure(list(
    form = object$form, method = object$method, coefficients = object$coefficients,
    fit_statistics = object$fit_statistics, n_trees = object$n_trees,
    n_measurements = object$n_measurements,
    spcd = object$spcd, groups_seen = object$groups_seen,
    convergence = object$convergence,
    residual_quantiles = stats::quantile(object$residual_diagnostics$residual, c(
      0, 0.25,
      0.5, 0.75, 1
    ), names = TRUE)
  ), class = "summary.taper_fit")
}

#' @export
print.summary.taper_fit <- function(x, ...) {
  cat("Taper model fit\n")
  cat("Form: ", x$form, "\n", sep = "")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Trees: ", x$n_trees, "\n", sep = "")
  cat("Measurements: ", x$n_measurements, "\n\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("\nOverall fit statistics in inches:\n")
  print(x$fit_statistics$overall, row.names = FALSE)
  cat("\nFit statistics by relative-height class:\n")
  print(x$fit_statistics$by_relative_height, row.names = FALSE)
  invisible(x)
}

.taper_predict_data <- function(object, newdata, measurement_system, group) {
  if (is.null(newdata)) {
    data <- object$data_metric
    if (!is.null(group)) {
      data$group <- factor(.taper_argument_vector(data, group, "group"),
        levels = object$groups_seen
      )
    }
    return(data)
  }
  choices <- list(dbh = "dbh", ht = c("ht", "total_height"), h = c(
    "h",
    "measurement_height"
  ))
  columns <- vapply(choices, function(column_names) {
    found <- column_names[column_names %in% names(newdata)]
    if (length(found))
      found[[1L]] else NA_character_
  }, character(1))
  if (anyNA(columns)) {
    stop("newdata must contain dbh, ht, and h columns.", call. = FALSE)
  }
  if (any(!vapply(newdata[columns], is.numeric, logical(1)))) {
    stop("newdata dbh, ht, and h columns must be numeric.", call. = FALSE)
  }
  data <- data.frame(
    dbh = .diameter_to_native(
      as.double(newdata[[columns[["dbh"]]]]), measurement_system,
      "metric"
    ),
    ht = .height_to_native(
      as.double(newdata[[columns[["ht"]]]]), measurement_system, "metric"
    ),
    h = .height_to_native(as.double(newdata[[columns[["h"]]]]), measurement_system, "metric"),
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
  invalid <- !is.finite(data$dbh) | !is.finite(data$ht) | !is.finite(data$h) | data$dbh <=
    0 | data$ht <= 0 | data$h < 0 | data$h > data$ht
  if (object$form == "kozak_2002") {
    invalid <- invalid | data$ht <= 1.3
  }
  if (any(invalid)) {
    stop("newdata contain out-of-domain prediction rows.", call. = FALSE)
  }
  data
}

#' @export
predict.taper_fit <- function(
  object, newdata = NULL, re_form = c("conditional", "population"),
  group = NULL, ...
) {
  object <- .taper_validate_fit(object)
  re_form <- match.arg(re_form)
  measurement_system <- "imperial"
  data <- .taper_predict_data(object, newdata, measurement_system, group)
  conditional <- re_form == "conditional" && nrow(object$random_effects) > 0L
  result <- .taper_predict_metric(
    object$form, object$internal_coefficients, data,
    if (conditional)
      object$random_effects else NULL, if (conditional)
      object$random_parameter else NULL
  )
  .diameter_from_native(result, measurement_system, "metric")
}

#' @export
plot.taper_fit <- function(x, ...) {
  x <- .taper_validate_fit(x)
  diagnostics <- x$residual_diagnostics
  observed <- diagnostics$observed_dib / diagnostics$dbh
  fitted <- diagnostics$fitted_dib / diagnostics$dbh
  graphics::plot(diagnostics$relative_height, observed,
    xlab = "Relative height", ylab = "Relative diameter",
    col = grDevices::adjustcolor("black", alpha.f = 0.35), pch = 16, ...
  )
  graphics::points(diagnostics$relative_height, fitted,
    col = grDevices::adjustcolor("firebrick",
      alpha.f = 0.55
    ), pch = 1
  )
  graphics::legend("topright",
    legend = c("Observed", "Fitted"), col = c("black", "firebrick"),
    pch = c(16, 1), bty = "n"
  )
  invisible(x)
}

#' Use a fitted taper relationship in stem calculations
#'
#' @param x The fitted taper relationship supplies the model coefficients. A taper_fit object.
#'   Required, with no default.
#' @param model Identifier to assign to the fitted model. Character scalar, required.
#' @param stump_ht Stump height above ground. Numeric scalar, feet, default `1`.
#' @param bark_ratio Inside to outside diameter ratio. Numeric scalar, default `NA_real_`.
#' @param source Model provenance. Character scalar, default `NULL` uses the equation citation
#'   and fitted note. The fitted species scope is retained.
#' @return A taper_model object with fitted coefficients and provenance, ready for
#'   register_taper_model().
#' @usage
#' as_taper_model(
#'   x,
#'   model,
#'   stump_ht = 1,
#'   bark_ratio = NA_real_,
#'   source = NULL
#' )
#' @export
#' @examples
#' ## Fit the shipped synthetic stem measurements.
#' fitted <- fit_taper(
#'   tree_id = example_stem_measurements$tree_id,
#'   dbh = example_stem_measurements$dbh,
#'   ht = example_stem_measurements$ht,
#'   h = example_stem_measurements$h,
#'   dib = example_stem_measurements$dib,
#'   spcd = example_stem_measurements$spcd,
#'   form = 'max_burkhart'
#' )
#'
#' ## Prepare the fitted relationship for registration.
#' local_model <- as_taper_model(
#'   x = fitted,
#'   model = 'example.fitted'
#' )
#'
#' ## Inspect the local model
#' print(local_model)
as_taper_model <- function(
  x,
  model,
  stump_ht = 1,
  bark_ratio = NA_real_,
  source = NULL
) {
  x <- .taper_validate_fit(x)
  species <- .normalize_species_codes(x$spcd)
  if (is.null(source)) {
    source <- paste(.published_taper_source(x$form), "Coefficients fitted by fit_taper().")
  }
  taper_model_from_coefficients(
    id = model, form = x$form, coefficients = x$coefficients, spcd = species,
    stump_ht = stump_ht, bark_ratio = bark_ratio, source = source
  )
}
