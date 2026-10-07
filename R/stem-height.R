.height_forms <- c("chapman_richards", "curtis", "wykoff", "naslund", "schumacher")

.height_breast_height <- 4.5

.height_nlme_chapman <- function(dbh, la, lb, lc) {
  .height_breast_height + exp(la) * (1 - exp(-exp(lb) * dbh))^exp(lc)
}

.height_nlme_curtis <- function(dbh, la, lb) {
  .height_breast_height + exp(la) * dbh / (1 + dbh)^exp(lb)
}

.height_nlme_wykoff <- function(dbh, a, lb) {
  .height_breast_height + exp(a - exp(lb) / (dbh + 1))
}

.height_nlme_naslund <- function(dbh, la, lb) {
  .height_breast_height + dbh^2 / (exp(la) * dbh + exp(lb))^2
}

.height_nlme_schumacher <- function(dbh, la, lb) {
  .height_breast_height + exp(la) * exp(-exp(lb) / dbh)
}

.height_parameter_names <- function(form) {
  if (identical(form, "chapman_richards")) {
    c("la", "lb", "lc")
  } else if (identical(form, "wykoff")) {
    c("a", "lb")
  } else {
    c("la", "lb")
  }
}

.height_internal_to_natural <- function(coefficients, form) {
  if (identical(form, "chapman_richards")) {
    return(c(a = exp(coefficients[["la"]]), b = exp(coefficients[["lb"]]), c = exp(
      coefficients[["lc"]]
    )))
  }
  if (identical(form, "wykoff")) {
    return(c(a = coefficients[["a"]], b = exp(coefficients[["lb"]]), c = NA_real_))
  }
  c(a = exp(coefficients[["la"]]), b = exp(coefficients[["lb"]]), c = NA_real_)
}

.height_evaluate <- function(dbh, coefficients, form, random_effect = 0) {
  if (identical(form, "chapman_richards")) {
    return(.height_nlme_chapman(
      dbh, coefficients[["la"]] + random_effect, coefficients[["lb"]],
      coefficients[["lc"]]
    ))
  }
  if (identical(form, "curtis")) {
    return(.height_nlme_curtis(
      dbh, coefficients[["la"]] + random_effect,
      coefficients[["lb"]]
    ))
  }
  if (identical(form, "wykoff")) {
    return(.height_nlme_wykoff(
      dbh, coefficients[["a"]] + random_effect,
      coefficients[["lb"]]
    ))
  }
  if (identical(form, "naslund")) {
    return(.height_nlme_naslund(
      dbh, coefficients[["la"]] + random_effect,
      coefficients[["lb"]]
    ))
  }
  .height_nlme_schumacher(dbh, coefficients[["la"]] + random_effect, coefficients[["lb"]])
}

.height_validate_integer <- function(value, name, minimum = 1L) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < minimum ||
        value > .Machine$integer.max || value != floor(value)) {
    stop(name, " must be one integer of at least ", minimum, ".", call. = FALSE)
  }
  as.integer(value)
}

.height_validate_interval <- function(interval) {
  if (!is.logical(interval) || length(interval) != 1L || is.na(interval)) {
    stop("interval must be TRUE or FALSE.", call. = FALSE)
  }
  interval
}

.height_validate_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
        level <= 0 || level >=
        1) {
    stop("level must be one number strictly between zero and one.", call. = FALSE)
  }
  as.double(level)
}

.height_validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed < 0 ||
        seed > .Machine$integer.max ||
        seed != floor(seed)) {
    stop("seed must be NULL or one nonnegative integer.", call. = FALSE)
  }
  as.integer(seed)
}

.height_prepare <- function(dbh, spcd, group = NULL, ht = NULL, include_ht = !is.null(ht)) {
  supplied <- list(dbh = dbh, spcd = spcd)
  if (include_ht) {
    supplied["ht"] <- list(ht)
  }
  if (!is.null(group)) {
    supplied$group <- group
  }
  size <- .common_size(supplied)
  supplied <- Map(
    function(value, name) .recycle_common(value, size, name),
    supplied, names(supplied)
  )
  numeric_names <- intersect(c("dbh", "ht", "spcd"), names(supplied))
  for (name in numeric_names) {
    if (!is.numeric(supplied[[name]]) || !is.null(dim(supplied[[name]]))) {
      stop(name, " must be a numeric vector.", call. = FALSE)
    }
    supplied[[name]] <- as.double(supplied[[name]])
  }
  if (is.null(group)) {
    supplied$group <- rep(".treevolume_all", size)
  } else {
    if (!is.atomic(supplied$group) || !is.null(dim(supplied$group))) {
      stop("group must be NULL or an atomic vector.", call. = FALSE)
    }
    missing_group <- is.na(supplied$group)
    if (is.numeric(supplied$group)) {
      missing_group <- missing_group | !is.finite(supplied$group)
    }
    supplied$group <- as.character(supplied$group)
    supplied$group[missing_group] <- NA_character_
  }
  c(list(size = size), supplied)
}

.height_status <- function(values, include_ht = FALSE, group_required = FALSE) {
  size <- values$size
  status <- integer(size)
  missing <- !is.finite(values$dbh) | !is.finite(values$spcd)
  if (include_ht) {
    missing <- missing | !is.finite(values$ht)
  }
  if (group_required) {
    missing <- missing | is.na(values$group) | !nzchar(values$group)
  }
  status[missing] <- 1L
  status <- .assign_status(status, values$dbh <= 0 | values$dbh > 400, 2L)
  if (include_ht) {
    status <- .assign_status(status, values$ht <= 0 | values$ht > 500, 3L)
  }
  valid_species <- values$spcd > 0 & values$spcd <= .Machine$integer.max &
    values$spcd == floor(values$spcd)
  .assign_status(status, !valid_species, 7L)
}

.height_linear_start <- function(response, predictor, intercept_default, slope_default) {
  fit <- tryCatch(stats::lm.fit(cbind(1, predictor), response), error = function(error) {
    NULL
  })
  if (is.null(fit) || length(fit$coefficients) != 2L || any(!is.finite(fit$coefficients))) {
    return(c(intercept_default, slope_default))
  }
  unname(fit$coefficients)
}

.height_start_candidates <- function(data, form) {
  dbh <- data$dbh
  excess <- pmax(data$ht - .height_breast_height, 0.1)
  median_dbh <- max(stats::median(dbh), 0.1)
  max_excess <- max(excess)
  if (identical(form, "chapman_richards")) {
    base <- c(la = log(max(max_excess * 1.1, 1)), lb = log(log(2) / median_dbh), lc = 0)
    return(list(base, base + c(la = log(1.25), lb = log(0.6), lc = log(0.7)), base + c(
      la = log(1.5),
      lb = log(1.6), lc = log(1.5)
    )))
  }
  if (identical(form, "curtis")) {
    make_start <- function(b) {
      a <- stats::median(excess * (1 + dbh)^b / dbh)
      c(la = log(max(a, 1e-06)), lb = log(b))
    }
    return(lapply(c(0.5, 0.25, 0.9), make_start))
  }
  if (identical(form, "wykoff")) {
    linear <- .height_linear_start(log(excess), 1 / (dbh + 1), log(max_excess), -1)
    a <- linear[[1L]]
    b <- max(-linear[[2L]], 0.05)
    return(list(c(a = a, lb = log(b)), c(a = log(max_excess), lb = log(1)), c(
      a = log(max_excess *
                1.2), lb = log(5)
    )))
  }
  if (identical(form, "naslund")) {
    linear <- .height_linear_start(dbh / sqrt(excess), dbh, 1, 0.1)
    a <- max(linear[[2L]], 1e-04)
    b <- max(linear[[1L]], 1e-04)
    return(list(
      c(la = log(a), lb = log(b)), c(la = log(a * 0.7), lb = log(b *
                                                                   1.5)),
      c(la = log(a *
                   1.5), lb = log(b * 0.7))
    ))
  }
  linear <- .height_linear_start(log(excess), 1 / dbh, log(max_excess), -1)
  a <- exp(linear[[1L]])
  b <- max(-linear[[2L]], 0.05)
  list(
    c(la = log(a), lb = log(b)), c(la = log(max_excess * 1.1), lb = log(median_dbh / 2)),
    c(la = log(max_excess * 1.5), lb = log(median_dbh))
  )
}

.height_nlme_formula <- function(form) {
  switch(form,
    chapman_richards = ht ~ 4.5 + exp(la) * (1 - exp(-exp(lb) * dbh))^exp(lc),
    curtis = ht ~
      4.5 + exp(la) * dbh / (1 + dbh)^exp(lb),
    wykoff = ht ~ 4.5 + exp(a - exp(lb) / (dbh + 1)),
    naslund = ht ~ 4.5 + dbh^2 / (exp(la) * dbh + exp(lb))^2,
    schumacher = ht ~ 4.5 + exp(la) *
      exp(-exp(lb) / dbh)
  )
}

.height_fit_one <- function(data, form, species_label) {
  parameters <- .height_parameter_names(form)
  fixed <- stats::as.formula(paste(paste(parameters, collapse = " + "), "~ 1"))
  random <- stats::as.formula(paste(parameters[[1L]], "~ 1 | group"))
  starts <- .height_start_candidates(data, form)
  last_message <- "unknown nlme failure"
  for (start in starts) {
    fit <- tryCatch(suppressWarnings(nlme::nlme(
      model = .height_nlme_formula(form), data = data,
      fixed = fixed, random = random, groups = ~group, start = start,
      na.action = stats::na.fail,
      control = nlme::nlmeControl(
        maxIter = 200L, pnlsMaxIter = 30L, msMaxIter = 200L,
        tolerance = 1e-06, pnlsTol = 1e-05, niterEM = 20L, returnObject = FALSE,
        msWarnNoConv = TRUE,
        apVar = FALSE
      )
    )), error = function(error) {
      last_message <<- conditionMessage(error)
      NULL
    })
    if (!is.null(fit)) {
      return(fit)
    }
  }
  stop("fit_height(): species ", species_label, " with form '", form,
    "' failed to converge after alternate start values: ",
    last_message,
    call. = FALSE
  )
}

.height_model_summary <- function(model, model_key, spcd, form) {
  fixed <- nlme::fixef(model)
  natural <- .height_internal_to_natural(fixed, form)
  variance <- nlme::VarCorr(model)
  standard_deviation <- as.double(variance[, "StdDev"])
  random_effect <- nlme::ranef(model)
  list(
    fixed = data.frame(
      model_key = model_key, spcd = spcd, a = natural[["a"]], b = natural[["b"]],
      c = natural[["c"]], stringsAsFactors = FALSE
    ), internal = fixed, variance = data.frame(
      model_key = model_key,
      spcd = spcd, random_parameter = names(fixed)[[1L]],
      random_effect_sd = standard_deviation[[1L]],
      residual_sd = standard_deviation[[length(standard_deviation)]],
      stringsAsFactors = FALSE
    ),
    random = data.frame(
      model_key = model_key, group = row.names(random_effect), effect = as.double(
        random_effect[[1L]]
      ),
      stringsAsFactors = FALSE
    )
  )
}

.height_bind_rows <- function(values) {
  if (!length(values)) {
    return(data.frame())
  }
  do.call(rbind, values)
}

.height_validate_fit <- function(fit) {
  required <- c(
    "form", "fixed_effects", "variance_components", "groups_seen", "n_by_species",
    "pooled_species", "package_version", "models", "model_map", "internal_fixed_effects",
    "random_effects"
  )
  if (!inherits(fit, "height_fit") || !all(required %in% names(fit))) {
    stop("fit must be a height_fit object returned by fit_height().", call. = FALSE)
  }
  fit
}

#' Fit height relationships by species and group
#'
#' Fit pooled and species height relationships, optionally with group adjustments. Invalid rows
#'   are omitted with a warning. Species below the minimum sample size or with an unsuccessful
#'   fit use the pooled relationship.
#'
#' @param dbh Outside bark diameter at breast height, in inches. Accepts numeric values greater
#'   than zero and no greater than 400. Required, with no default. Rows with invalid measurements
#'   are omitted with a warning.
#' @param ht Measured total height above ground, in feet. Accepts numeric values greater than
#'   zero and no greater than 500. Required, with no default. Valid measurements supply the
#'   response for pooled and species fits.
#' @param spcd Species identifiers as numeric positive whole-number codes within the R integer
#'   range and present in species_reference. Required, without a default. Codes group
#'   observations for species fits and need not have a registered taper equation. Unknown
#'   species rows are omitted with a warning.
#' @param group Group identifiers for shared adjustments, supplied as an atomic vector matching
#'   the observations. Defaults to `NULL`, with no separate group assignments. Known groups use
#'   their fitted adjustments for conditional predictions, while unseen groups use the population
#'   relationship.
#' @param form Height relationship as one character string. Accepts `'chapman_richards'`,
#'   `'curtis'`, `'wykoff'`, `'naslund'`, or `'schumacher'`. Defaults to `'chapman_richards'`.
#'   The selected form is used for pooled and species fits.
#' @param min_n Minimum valid observations for a separate species fit, as one positive whole
#'   number. Defaults to `7`. Species with fewer observations use the pooled model.
#' @return A `height_fit` list containing:
#' * `data`: retained `dbh` (inches), `ht` (feet), `spcd` (code), and `group` (identifier).
#' * `fixed_effects`: `spcd` (code), `model_key` (identifier), `pooled` (indicator),
#'   and equation coefficients `a`, `b`, `c` on the form-specific scale evaluated with
#'   diameter in inches and height in feet. Unused coefficients are missing.
#' * `variance_components`: `spcd`, `model_key`, `random_parameter`, `random_effect_sd`
#'   (coefficient scale), and `residual_sd` (feet).
#' * `random_effects`: `model_key`, `group`, and `effect` (internal coefficient scale).
#' * `n_by_species`: `spcd` and `n` (observation count).
#' * `form`, `min_n`, `groups_seen`, `pooled_species`, `convergence_pooled_species`,
#'   `measurement_system`, and `package_version`: fitting choices and provenance.
#' * `models`, `model_map`, and `internal_fixed_effects`: fitted model objects,
#'   species-to-model assignments, and coefficients used for prediction.
#' @usage
#' fit_height(
#'   dbh,
#'   ht,
#'   spcd,
#'   group = NULL,
#'   form = 'chapman_richards',
#'   min_n = 7
#' )
#' @export
#' @examples
#' ## Select measured heights from the shipped list
#' library(dplyr)
#'
#' ## Retain the tree rows used in this calculation
#' trees <- example_trees_pnw %>%
#'   filter(ht_status == 'measured')
#'
#' ## Fit and inspect the relationship with plot adjustments
#' fit_height(dbh = trees$dbh,
#'            ht = trees$ht,
#'            spcd = trees$spcd,
#'            group = trees$plot)

fit_height <- function(dbh, ht, spcd, group = NULL, form = "chapman_richards", min_n = 7) {
  form <- .scalar_character(form, "form", .height_forms)
  min_n <- .height_validate_integer(min_n, "min_n")
  measurement_system <- "imperial"
  values <- .height_prepare(dbh, spcd, group, ht, include_ht = TRUE)
  status <- .height_status(values, include_ht = TRUE, group_required = TRUE)
  status <- .assign_status(status, !values$spcd %in% species_reference$spcd, 7L)
  if (any(status != 0L)) {
    reasons <- c(
      "missing or nonfinite dbh, ht, spcd, or group",
      "invalid_dbh must be greater than zero and at most 400 inches",
      "invalid_height must be greater than zero and at most 500 feet",
      "unknown_species spcd absent from species_reference"
    )
    codes <- c(1L, 2L, 3L, 7L)
    warning("fit_height(): ", sum(status != 0L), " rows with ",
      paste(reasons[codes %in% status], collapse = ", "), " were omitted.",
      call. = FALSE
    )
  }
  keep <- status == 0L
  if (!any(keep)) {
    stop("fit_height(): no complete, valid measured heights remain, found 0.", call. = FALSE)
  }
  data <- data.frame(
    dbh = .diameter_to_native(
      values$dbh[keep], measurement_system, "imperial"
    ), ht = .height_to_native(
      values$ht[keep],
      measurement_system, "imperial"
    ), spcd = as.integer(values$spcd[keep]), group = factor(values$group[keep]),
    stringsAsFactors = FALSE
  )
  counts <- table(data$spcd)
  count_data <- data.frame(
    spcd = as.integer(names(counts)), n = as.integer(counts),
    stringsAsFactors = FALSE
  )
  count_data <- count_data[order(count_data$spcd), , drop = FALSE]
  pooled_species <- count_data$spcd[count_data$n < min_n]
  eligible_species <- count_data$spcd[count_data$n >= min_n]

  pooled <- .height_fit_one(data, form, "pooled")
  models <- list(pooled = pooled)
  model_map <- stats::setNames(rep("pooled", nrow(count_data)), count_data$spcd)
  convergence_pooled <- integer()
  for (species in eligible_species) {
    species_data <- data[data$spcd == species, , drop = FALSE]
    key <- paste0("spcd_", species)
    species_fit <- tryCatch(.height_fit_one(species_data, form, as.character(
      species
    )), error = function(error) {
      warning(conditionMessage(error),
        ". The pooled fit was used for this species.",
        call. = FALSE
      )
      NULL
    })
    if (is.null(species_fit)) {
      convergence_pooled <- c(convergence_pooled, species)
    } else {
      models[[key]] <- species_fit
      model_map[[as.character(species)]] <- key
    }
  }

  summaries <- Map(function(model, key) {
    species <- if (identical(key, "pooled")) {
      NA_integer_
    } else {
      as.integer(sub("^spcd_", "", key))
    }
    .height_model_summary(model, key, species, form)
  }, models, names(models))
  model_fixed <- .height_bind_rows(lapply(summaries, `[[`, "fixed"))
  model_variance <- .height_bind_rows(lapply(summaries, `[[`, "variance"))
  random_effects <- .height_bind_rows(lapply(summaries, `[[`, "random"))
  internal_fixed <- lapply(summaries, `[[`, "internal")
  names(internal_fixed) <- names(models)
  active_keys <- unname(model_map[as.character(count_data$spcd)])
  fixed_rows <- match(active_keys, model_fixed$model_key)
  fixed_effects <- data.frame(
    spcd = count_data$spcd, model_key = active_keys, pooled = active_keys ==
      "pooled", a = model_fixed$a[fixed_rows], b = model_fixed$b[fixed_rows],
    c = model_fixed$c[fixed_rows],
    stringsAsFactors = FALSE
  )
  variance_rows <- match(active_keys, model_variance$model_key)
  variance_components <- data.frame(
    spcd = count_data$spcd, model_key = active_keys,
    random_parameter = model_variance$random_parameter[variance_rows],
    random_effect_sd = model_variance$random_effect_sd[variance_rows],
    residual_sd = model_variance$residual_sd[variance_rows],
    stringsAsFactors = FALSE
  )
  package_version <- tryCatch(tv_version(), error = function(error) NA_character_)

  structure(list(
    data = data, form = form, fixed_effects = fixed_effects,
    variance_components = variance_components,
    groups_seen = sort(unique(as.character(data$group))), n_by_species = count_data,
    pooled_species = as.integer(pooled_species),
    package_version = package_version,
    measurement_system = "imperial", min_n = min_n, models = models,
    model_map = model_map, internal_fixed_effects = internal_fixed,
    random_effects = random_effects,
    convergence_pooled_species = as.integer(convergence_pooled)
  ), class = "height_fit")
}

#' @export
print.height_fit <- function(x, ...) {
  x <- .height_validate_fit(x)
  cat("<height_fit>\n")
  cat("  form: ", x$form, "\n", sep = "")
  cat("  measured trees: ", sum(x$n_by_species$n), "\n", sep = "")
  cat("  species: ", nrow(x$n_by_species), "\n", sep = "")
  cat("  groups: ", length(x$groups_seen), "\n", sep = "")
  cat("  below-minimum pooled species: ", length(x$pooled_species), "\n", sep = "")
  invisible(x)
}

.height_prediction_rows <- function(fit, spcd, group, status) {
  model_key <- rep(NA_character_, length(spcd))
  eligible <- which(status == 0L)
  model_key[eligible] <- unname(fit$model_map[as.character(as.integer(spcd[eligible]))])
  unknown <- is.na(model_key) & status == 0L
  status <- .assign_status(status, unknown, 7L)
  random_effect <- numeric(length(spcd))
  for (key in unique(stats::na.omit(model_key))) {
    rows <- which(model_key == key & status == 0L)
    effects <- fit$random_effects[fit$random_effects$model_key == key, , drop = FALSE]
    matched <- match(group[rows], effects$group)
    found <- !is.na(matched)
    random_effect[rows[found]] <- effects$effect[matched[found]]
  }
  list(model_key = model_key, random_effect = random_effect, status = status)
}

.height_rng_state <- function(seed) {
  old_kind <- RNGkind()
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  if (is.null(seed)) {
    local_seed <- sample.int(.Machine$integer.max, 1L)
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_exists <- TRUE
  } else {
    local_seed <- seed
  }
  RNGkind("L'Ecuyer-CMRG")
  set.seed(local_seed)
  list(kind = old_kind, exists = old_exists, seed = old_seed)
}

.height_restore_rng <- function(state) {
  do.call(RNGkind, as.list(state$kind))
  if (state$exists) {
    assign(".Random.seed", state$seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

.height_simulate_interval <- function(
  fit, dbh, rows, predictions, re_form, level,
  nsim, seed
) {
  lower <- upper <- rep(NA_real_, length(dbh))
  rng_state <- .height_rng_state(seed)
  on.exit(.height_restore_rng(rng_state), add = TRUE)
  stream <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  probabilities <- c((1 - level) / 2, 1 - (1 - level) / 2)
  for (row in rows) {
    assign(".Random.seed", stream, envir = .GlobalEnv)
    key <- predictions$model_key[[row]]
    variance <- fit$variance_components[match(key, fit$variance_components$model_key), ,
      drop = FALSE
    ]
    center <- if (identical(re_form, "conditional")) {
      predictions$random_effect[[row]]
    } else {
      0
    }
    simulated_effect <- stats::rnorm(nsim, center, variance$random_effect_sd[[1L]])
    simulated <- .height_evaluate(
      rep(dbh[[row]], nsim), fit$internal_fixed_effects[[key]],
      fit$form, simulated_effect
    )
    simulated <- simulated + stats::rnorm(nsim, 0, variance$residual_sd[[1L]])
    simulated <- pmax(simulated, .height_breast_height)
    limits <- stats::quantile(simulated, probabilities, names = FALSE, type = 8)
    lower[[row]] <- limits[[1L]]
    upper[[row]] <- limits[[2L]]
    stream <- parallel::nextRNGStream(stream)
  }
  list(lower = lower, upper = upper)
}

#' Predict tree heights and optional intervals
#'
#' Predict total height from a fitted relationship. Conditional predictions use known group
#'   adjustments, while missing or unseen groups use population values. Intervals simulate group
#'   variation and residual error, holding fixed coefficients constant. They exclude coefficient
#'   uncertainty and inventory sampling uncertainty.
#'
#' @param fit Fitted height relationship returned by [fit_height()]. Required, with no default.
#'   Predictions use its species mapping and fitted group adjustments.
#' @param dbh Outside bark diameter at breast height, in inches. Accepts numeric values greater
#'   than zero and no greater than 400. Required, with no default. Invalid rows return missing
#'   heights, with warnings for invalid finite inputs.
#' @param spcd Species identifiers as numeric positive whole-number codes within the R integer
#'   range. Required, without a default. A code absent from the fitted species mapping returns a
#'   missing prediction with a warning.
#' @param group Group identifiers for shared adjustments, supplied as an atomic vector matching
#'   the observations. Defaults to `NULL`, with no separate group assignments. Known groups use
#'   their fitted adjustments for conditional predictions, while unseen groups use the population
#'   relationship.
#' @param re_form Adjustment basis as one character string. Accepts `'conditional'` or
#'   `'population'`. Defaults to `'conditional'`, using available fitted group effects.
#'   Population predictions omit those fitted adjustments.
#' @param interval Whether to return simulated prediction intervals. Accepts one `TRUE` or
#'   `FALSE`. Defaults to `FALSE`, returning a numeric vector. `TRUE` returns fitted values and
#'   interval endpoints, with lower endpoints constrained to breast height.
#' @param level Coverage requested for prediction intervals, as a single finite fraction strictly
#'   between zero and one. Defaults to `0.95`. Used only when `interval = TRUE`.
#' @param nsim Simulation count for intervals as one positive whole number, no greater than
#'   1000000. Defaults to `1000`. More simulations refine the simulated quantiles without adding
#'   coefficient uncertainty.
#' @param seed Random seed as one nonnegative whole number within the R integer range, or `NULL`.
#'   Defaults to `NULL`, using the current random state. A supplied seed makes interval
#'   simulation repeatable.
#' @return With `interval = FALSE`, a numeric vector of heights in feet. With `TRUE`, a data
#'   frame with `fit`, `lower`, and `upper`, all in feet. Input order is retained. Species absent
#'   from the fit return missing values with a warning.
#' @usage
#' predict_height(
#'   fit,
#'   dbh,
#'   spcd,
#'   group = NULL,
#'   re_form = 'conditional',
#'   interval = FALSE,
#'   level = 0.95,
#'   nsim = 1000,
#'   seed = NULL
#' )
#' @export
#' @examples
#' ## Fit and predict heights on the shipped example trees
#' predict_height(fit = fit_height(dbh = example_trees$dbh,
#'                                 ht = example_trees$ht,
#'                                 spcd = example_trees$spcd),
#'                dbh = example_trees$dbh,
#'                spcd = example_trees$spcd)
predict_height <- function(
  fit, dbh, spcd, group = NULL, re_form = "conditional", interval = FALSE,
  level = 0.95, nsim = 1000, seed = NULL
) {
  fit <- .height_validate_fit(fit)
  re_form <- .scalar_character(re_form, "re_form", c("conditional", "population"))
  interval <- .height_validate_interval(interval)
  level <- .height_validate_level(level)
  nsim <- .height_validate_integer(nsim, "nsim")
  if (nsim > 1e6)
    stop("nsim must be at most 1e6.", call. = FALSE)
  seed <- .height_validate_seed(seed)
  measurement_system <- "imperial"
  group_missing <- is.null(group)
  values <- .height_prepare(dbh, spcd, group)
  status <- .height_status(values)
  prediction_rows <- .height_prediction_rows(fit, values$spcd, values$group, status)
  status <- prediction_rows$status
  native_dbh <- .diameter_to_native(values$dbh, measurement_system, "imperial")
  output <- rep(NA_real_, values$size)
  valid <- which(status == 0L)
  for (key in unique(stats::na.omit(prediction_rows$model_key[valid]))) {
    rows <- valid[prediction_rows$model_key[valid] == key]
    random_effect <- if (identical(re_form, "conditional") && !group_missing) {
      prediction_rows$random_effect[rows]
    } else {
      rep(0, length(rows))
    }
    output[rows] <- .height_evaluate(
      native_dbh[rows], fit$internal_fixed_effects[[key]],
      fit$form, random_effect
    )
  }
  .status_warning("predict_height", status, size = values$size)
  output <- .height_from_native(output, measurement_system, "imperial")
  if (!interval) {
    return(output)
  }
  limits <- list(lower = rep(NA_real_, values$size), upper = rep(NA_real_, values$size))
  if (length(valid)) {
    simulation_form <- if (group_missing)
      "population" else re_form
    limits <- .height_simulate_interval(
      fit, native_dbh, valid, prediction_rows, simulation_form,
      level, nsim, seed
    )
  }
  data.frame(fit = output, lower = .height_from_native(
    limits$lower, measurement_system,
    "imperial"
  ), upper = .height_from_native(
    limits$upper,
    measurement_system, "imperial"
  ))
}

.height_completion_dots <- function(dots) {
  if (!length(dots)) {
    return(list(form = "chapman_richards", min_n = 7L, re_form = "conditional"))
  }
  dot_names <- names(dots)
  if (is.null(dot_names) || any(!nzchar(dot_names)) || anyDuplicated(dot_names)) {
    stop("every argument in ... must have a unique non-empty name.", call. = FALSE)
  }
  unknown <- setdiff(dot_names, c("form", "min_n", "re_form"))
  if (length(unknown)) {
    stop("unused argument in ...: ", unknown[[1L]], call. = FALSE)
  }
  form <- if (is.null(dots$form))
    "chapman_richards" else dots$form
  min_n <- if (is.null(dots$min_n))
    7L else dots$min_n
  re_form <- if (is.null(dots$re_form))
    "conditional" else dots$re_form
  list(
    form = .scalar_character(form, "form", .height_forms),
    min_n = .height_validate_integer(
      min_n,
      "min_n"
    ), re_form = .scalar_character(re_form, "re_form", c("conditional", "population"))
  )
}

#' Fill missing heights while retaining measured values
#'
#' Complete nonfinite heights from a fitted diameter-height relationship. Measured heights
#'   are preserved only on rows with valid diameter, height, and species inputs. Invalid finite
#'   rows generate a warning and remain missing.
#'
#' @param dbh Outside bark diameter at breast height, in inches. Accepts numeric values greater
#'   than zero and no greater than 400. Required, with no default. Invalid rows return missing
#'   heights, with warnings for invalid finite inputs.
#' @param ht Measured total height above ground, in feet. Accepts numeric values greater than
#'   zero and no greater than 500, or nonfinite values to be predicted. Required, with no default.
#'   Invalid finite rows remain missing. Nonfinite heights are completed from the supplied fit,
#'   or from a fit to the remaining valid observations.
#' @param spcd Species identifiers as numeric positive whole-number codes within the R integer
#'   range. Required, without a default. Codes select relationships in the supplied fit or group
#'   observations when a new fit is needed.
#' @param group Group identifiers for shared adjustments, supplied as an atomic vector matching
#'   the observations. Defaults to `NULL`, with no separate group assignments. Known groups use
#'   their fitted adjustments for conditional predictions, while unseen groups use the population
#'   relationship.
#' @param fit Height fit returned by [fit_height()], or `NULL`. Defaults to `NULL`, fitting the
#'   available valid heights with the supplied species and groups. Supplying a fit avoids
#'   refitting.
#' @return A numeric vector of total heights in feet, in input order. Measured values on rows
#'   with valid diameter, height, and species inputs are unchanged. Missing predictions remain
#'   `NA` when the fit cannot cover a species or an input
#'   is invalid.
#' @usage
#' complete_heights(
#'   dbh,
#'   ht,
#'   spcd,
#'   group = NULL,
#'   fit = NULL
#' )
#' @export
#' @examples
#' ## Remove stored predictions while preserving measurements
#' library(dplyr)
#'
#' ## Retain the tree rows used in this calculation
#' trees <- example_trees_pnw %>%
#'   mutate(observed = if_else(ht_status == 'predicted', NA_real_, ht))
#'
#' ## Refit and inspect completed heights with the original plot groups
#' head(complete_heights(dbh = trees$dbh,
#'                       ht = trees$observed,
#'                       spcd = trees$spcd,
#'                       group = trees$plot))
complete_heights <- function(dbh, ht, spcd, group = NULL, fit = NULL) {
  measurement_system <- "imperial"
  controls <- .height_completion_dots(list())
  values <- .height_prepare(dbh, spcd, group, ht, include_ht = TRUE)
  if (!is.null(fit)) {
    fit <- .height_validate_fit(fit)
  }
  output <- values$ht
  status <- .height_status(values, include_ht = TRUE)
  invalid <- is.finite(output) & status != 0L
  output[invalid] <- NA_real_
  .status_warning("complete_heights", status, size = values$size)
  missing <- !is.finite(values$ht)
  output[missing] <- NA_real_
  if (!any(missing)) {
    return(output)
  }
  if (is.null(fit)) {
    fit <- fit_height(values$dbh, output, values$spcd, values$group,
      form = controls$form,
      min_n = controls$min_n
    )
  }
  missing_group <- if (is.null(group))
    NULL else values$group[missing]
  output[missing] <- predict_height(fit, values$dbh[missing], values$spcd[missing],
    missing_group,
    re_form = controls$re_form
  )
  output
}
