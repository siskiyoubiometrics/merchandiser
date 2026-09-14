.height_forms <- c(
  "chapman_richards", "curtis", "wykoff", "naslund", "schumacher"
)

.height_breast_height <- 4.5

.height_nlme_chapman <- function(dbh, la, lb, lc) {
  .height_breast_height +
    exp(la) * (1 - exp(-exp(lb) * dbh))^exp(lc)
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
    return(c(
      a = exp(coefficients[["la"]]),
      b = exp(coefficients[["lb"]]),
      c = exp(coefficients[["lc"]])
    ))
  }
  if (identical(form, "wykoff")) {
    return(c(
      a = coefficients[["a"]], b = exp(coefficients[["lb"]]), c = NA_real_
    ))
  }
  c(
    a = exp(coefficients[["la"]]),
    b = exp(coefficients[["lb"]]),
    c = NA_real_
  )
}

.height_evaluate <- function(dbh, coefficients, form, random_effect = 0) {
  if (identical(form, "chapman_richards")) {
    return(.height_nlme_chapman(
      dbh, coefficients[["la"]] + random_effect,
      coefficients[["lb"]], coefficients[["lc"]]
    ))
  }
  if (identical(form, "curtis")) {
    return(.height_nlme_curtis(
      dbh, coefficients[["la"]] + random_effect, coefficients[["lb"]]
    ))
  }
  if (identical(form, "wykoff")) {
    return(.height_nlme_wykoff(
      dbh, coefficients[["a"]] + random_effect, coefficients[["lb"]]
    ))
  }
  if (identical(form, "naslund")) {
    return(.height_nlme_naslund(
      dbh, coefficients[["la"]] + random_effect, coefficients[["lb"]]
    ))
  }
  .height_nlme_schumacher(
    dbh, coefficients[["la"]] + random_effect, coefficients[["lb"]]
  )
}

.height_validate_integer <- function(value, name, minimum = 1L) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < minimum || value > .Machine$integer.max || value != floor(value)) {
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
        level <= 0 || level >= 1) {
    stop("level must be one number strictly between zero and one.", call. = FALSE)
  }
  as.double(level)
}

.height_validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
        seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
    stop("seed must be NULL or one nonnegative integer.", call. = FALSE)
  }
  as.integer(seed)
}

.height_prepare <- function(dbh, spcd, group = NULL, ht = NULL,
                            include_ht = !is.null(ht)) {
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
  status <- .assign_status(status, values$dbh <= 0, 2L)
  if (include_ht) {
    status <- .assign_status(status, values$ht <= 0, 3L)
  }
  valid_species <- values$spcd > 0 & values$spcd <= .Machine$integer.max &
    values$spcd == floor(values$spcd)
  .assign_status(status, !valid_species, 7L)
}

.height_linear_start <- function(response, predictor, intercept_default,
                                 slope_default) {
  fit <- tryCatch(
    stats::lm.fit(cbind(1, predictor), response),
    error = function(error) NULL
  )
  if (is.null(fit) || length(fit$coefficients) != 2L ||
        any(!is.finite(fit$coefficients))) {
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
    base <- c(
      la = log(max(max_excess * 1.1, 1)),
      lb = log(log(2) / median_dbh),
      lc = 0
    )
    return(list(
      base,
      base + c(la = log(1.25), lb = log(0.6), lc = log(0.7)),
      base + c(la = log(1.5), lb = log(1.6), lc = log(1.5))
    ))
  }
  if (identical(form, "curtis")) {
    make_start <- function(b) {
      a <- stats::median(excess * (1 + dbh)^b / dbh)
      c(la = log(max(a, 1e-6)), lb = log(b))
    }
    return(lapply(c(0.5, 0.25, 0.9), make_start))
  }
  if (identical(form, "wykoff")) {
    linear <- .height_linear_start(
      log(excess), 1 / (dbh + 1), log(max_excess), -1
    )
    a <- linear[[1L]]
    b <- max(-linear[[2L]], 0.05)
    return(list(
      c(a = a, lb = log(b)),
      c(a = log(max_excess), lb = log(1)),
      c(a = log(max_excess * 1.2), lb = log(5))
    ))
  }
  if (identical(form, "naslund")) {
    linear <- .height_linear_start(
      dbh / sqrt(excess), dbh, 1, 0.1
    )
    a <- max(linear[[2L]], 1e-4)
    b <- max(linear[[1L]], 1e-4)
    return(list(
      c(la = log(a), lb = log(b)),
      c(la = log(a * 0.7), lb = log(b * 1.5)),
      c(la = log(a * 1.5), lb = log(b * 0.7))
    ))
  }
  linear <- .height_linear_start(log(excess), 1 / dbh, log(max_excess), -1)
  a <- exp(linear[[1L]])
  b <- max(-linear[[2L]], 0.05)
  list(
    c(la = log(a), lb = log(b)),
    c(la = log(max_excess * 1.1), lb = log(median_dbh / 2)),
    c(la = log(max_excess * 1.5), lb = log(median_dbh))
  )
}

.height_nlme_formula <- function(form) {
  switch(form,
    chapman_richards = ht ~ 4.5 +
      exp(la) * (1 - exp(-exp(lb) * dbh))^exp(lc),
    curtis = ht ~ 4.5 + exp(la) * dbh / (1 + dbh)^exp(lb),
    wykoff = ht ~ 4.5 + exp(a - exp(lb) / (dbh + 1)),
    naslund = ht ~ 4.5 + dbh^2 / (exp(la) * dbh + exp(lb))^2,
    schumacher = ht ~ 4.5 + exp(la) * exp(-exp(lb) / dbh)
  )
}

.height_fit_one <- function(data, form, species_label) {
  parameters <- .height_parameter_names(form)
  fixed <- stats::as.formula(paste(paste(parameters, collapse = " + "), "~ 1"))
  random <- stats::as.formula(paste(parameters[[1L]], "~ 1 | group"))
  starts <- .height_start_candidates(data, form)
  last_message <- "unknown nlme failure"
  for (start in starts) {
    fit <- tryCatch(
      suppressWarnings(nlme::nlme(
        model = .height_nlme_formula(form),
        data = data,
        fixed = fixed,
        random = random,
        groups = ~group,
        start = start,
        na.action = stats::na.fail,
        control = nlme::nlmeControl(
          maxIter = 200L, pnlsMaxIter = 30L, msMaxIter = 200L,
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
      return(fit)
    }
  }
  stop(
    "fit_height(): species ", species_label, " with form '", form,
    "' failed to converge after alternate start values: ", last_message,
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
      model_key = model_key,
      spcd = spcd,
      a = natural[["a"]],
      b = natural[["b"]],
      c = natural[["c"]],
      stringsAsFactors = FALSE
    ),
    internal = fixed,
    variance = data.frame(
      model_key = model_key,
      spcd = spcd,
      random_parameter = names(fixed)[[1L]],
      random_effect_sd = standard_deviation[[1L]],
      residual_sd = standard_deviation[[length(standard_deviation)]],
      stringsAsFactors = FALSE
    ),
    random = data.frame(
      model_key = model_key,
      group = row.names(random_effect),
      effect = as.double(random_effect[[1L]]),
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
    "form", "fixed_effects", "variance_components", "groups_seen",
    "n_by_species", "pooled_species", "package_version", "models",
    "model_map", "internal_fixed_effects", "random_effects"
  )
  if (!inherits(fit, "height_fit") || !all(required %in% names(fit))) {
    stop("fit must be a height_fit object returned by fit_height().", call. = FALSE)
  }
  fit
}

#' Fit tree height from diameter and measured heights
#'
#' Fit height from diameter by species, optionally including group effects. Return
#' coefficients, group differences, fit records, and the data used for fitting.
#'
#' @param dbh Required positive finite numeric outside-bark breast-height diameters,
#'   in inches or centimeters. Length one repeats, otherwise one per tree. Omission is an error.
#'
#' Missing or nonfinite values give `na_input`
#'   and are omitted from fitting or return missing predictions. Example: `example_trees$dbh`.
#' @param ht Required numeric measured total heights above ground, in feet or
#'   meters. Length one repeats, otherwise one per tree. Positive finite
#'   values are fitted.
#'
#' Missing or nonfinite heights are omitted, while
#'   nonpositive heights are omitted with a warning. Example: `example_trees_pnw$ht_observed`.
#' @param spcd Required numeric Forest Inventory and Analysis tree inventory
#'   species codes, length one or one per tree. Positive whole numbers within
#'   R's integer range are accepted. Unitless.
#'
#' Omission is an error. Missing
#'   gives `na_input` and invalid codes give `unknown_species`.
#' @param group Atomic plot or stand labels, length one or one per tree. Default
#'   `NULL` treats fitting observations as one group and uses the common
#'   fitted relationship when predicting. Missing or empty groups are omitted
#'   during fitting.
#'
#' Missing or unseen groups use the common relationship
#'   during prediction. Unitless. Example: `group = example_trees_pnw$plot`.
#' @param form One character equation name, `'chapman_richards'` (default),
#'   `'curtis'`, `'wykoff'`, `'naslund'`, or `'schumacher'`. Missing or unknown labels are errors.
#' Unitless.
#'
#' Example: `form = 'wykoff'`.
#' @param min_n One positive whole number within R's integer range. Default `7`
#'   is the minimum retained measurement count for a species-specific fit
#'   and the minimum total count for fitting. Missing or invalid values are
#'   errors.
#'
#' Unitless.
#' @param units One character value, `'imperial'` (default) or `'metric'`. Imperial diameters and
#'   heights use inches and feet. Metric uses
#'   centimeters and meters.
#'
#' Missing or unknown values are errors. The fit is stored in inches and feet. Example: `units =
#' 'metric'`.
#'
#' @details With height \eqn{H} in feet, diameter \eqn{D} in inches, and
#' breast height \eqn{H_b = 4.5} feet, the forms are:
#'
#' * Chapman-Richards: \eqn{H = H_b + a(1 - \exp(-bD))^c}. * Curtis: \eqn{H = H_b + aD/(1+D)^b}. *
#' Wykoff: \eqn{H = H_b + \exp(a - b/(D+1))}.
#'
#' * Näslund: \eqn{H = H_b + D^2/(aD+b)^2}. * Schumacher: \eqn{H = H_b + a\exp(-b/D)}.
#'
#' Positive parameters are fitted on a log scale. This parameterization keeps predicted height
#' at or above breast height at zero diameter wherever the form has a defined value or limit.
#' The first start uses data-derived asymptote and linearized estimates where available.
#'
#' Two alternate starts vary scale, rate, and shape. Failure of a species fit after all starts
#' uses the pooled model and records the species in `convergence_pooled_species`. Failure of
#' the pooled fit is an error naming the pooled species and form.
#'
#' @references Chai Z, Tan W, Li Y, Yan L, Yuan H, Li Z (2018).
#'   "Generalized nonlinear height-diameter models for a *Cryptomeria fortunei*
#'   plantation in the Pingba region of Guizhou Province, China."
#'   *Web Ecology*, 18, 29-35.
#'   \doi{10.5194/we-18-29-2018}.
#'
#' @return An object of class `height_fit`. It retains valid measurements in `data`, with diameter
#' in inches and height in feet. Other fields contain coefficients, group effects, counts, pooling
#' records, and fitted models.
#'
#' [plot.height_fit()] uses the retained measurements.
#' @export
#' @usage
#'
#' ## Call signatures
#' fit_height(dbh, ht, spcd, group = NULL, form = 'chapman_richards', min_n = 7, units =
#'   'imperial')
#' @examples
#' ## Fit the retained inventory heights
#' fit <- fit_height(dbh = example_trees_pnw$dbh,
#'                   ht = example_trees_pnw$ht_observed,
#'                   spcd = example_trees_pnw$spcd)
#'
#' ## Draw the fitted relationship
#' plot(fit)
#' @section Fitted height fields:
#' The returned `height_fit` is a list. `form` names the equation. `data` is a data frame of
#' valid measured rows with numeric `dbh` in inches, `ht` in feet, integer species code `spcd`,
#' and factor `group`.
#'
#' No missing measurement rows remain. `fixed_effects` has integer `spcd`, character
#' `model_key`, logical `pooled`, and numeric `a`, `b`, `c`. These are the coefficients of the
#' equations in Details, using inches and feet.
#'
#' `c` is missing for forms without a third coefficient.
#'
#' `variance_components` has integer `spcd`, character `model_key` and
#' `random_parameter`, numeric `random_effect_sd` on the fitted parameter
#' scale, and numeric `residual_sd` in feet. `random_effects` has character
#' `model_key` and `group` plus numeric `effect` on that parameter scale.
#' `n_by_species` has integer `spcd` and integer `n`, the number of retained
#' measurements. Codes, names, flags, and counts are unitless.
#'
#' `groups_seen` lists character group labels. `pooled_species` lists integer species codes
#' below the count threshold. `convergence_pooled_species` lists those whose separate fit
#' failed.
#'
#' `model_map` maps species to model keys, and `models` holds the fitted R models.
#' `internal_fixed_effects` holds transformed coefficients by model key. `units` is
#' `'imperial'`, `min_n` is the supplied count threshold, and `package_version` records the
#' package version or is missing when unavailable.
#'
#' None is a data-frame list column. Keep the complete fitted object for prediction.
#' @section
#' Status and missing values: Rows with missing measurements, missing grouping labels, nonpositive
#' dimensions, or invalid species are omitted. [status_codes()] identifies these input diagnoses.
#'
#' Diagnoses other than `na_input` produce warnings. These codes are not returned as a table. Too
#' few
#' retained heights or failure of the pooled fit stops the call. Species with too few heights
#' or failed separate fits use the pooled model.
#'
#' Inspect the recorded species lists before prediction. No log-volume status is calculated.
#' @seealso [predict_height()] to estimate heights, [complete_heights()] to fill an inventory,
#' [plot.height_fit()] to inspect measured and fitted heights.
fit_height <- function(dbh, ht, spcd, group = NULL,
                       form = "chapman_richards", min_n = 7,
                       units = "imperial") {
  form <- .scalar_character(form, "form", .height_forms)
  min_n <- .height_validate_integer(min_n, "min_n")
  units <- .validate_units(units)
  values <- .height_prepare(dbh, spcd, group, ht, include_ht = TRUE)
  status <- .height_status(values, include_ht = TRUE, group_required = TRUE)
  .status_warning("fit_height", status, size = values$size)
  keep <- status == 0L
  if (sum(keep) < min_n) {
    stop(
      "fit_height(): at least ", min_n,
      " complete, valid measured heights are required, but found ",
      sum(keep), ".",
      call. = FALSE
    )
  }
  data <- data.frame(
    dbh = .diameter_to_native(values$dbh[keep], units, "imperial"),
    ht = .height_to_native(values$ht[keep], units, "imperial"),
    spcd = as.integer(values$spcd[keep]),
    group = factor(values$group[keep]),
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
    species_fit <- tryCatch(
      .height_fit_one(species_data, form, as.character(species)),
      error = function(error) {
        warning(
          conditionMessage(error), ". The pooled fit was used for this species.",
          call. = FALSE
        )
        NULL
      }
    )
    if (is.null(species_fit)) {
      convergence_pooled <- c(convergence_pooled, species)
    } else {
      models[[key]] <- species_fit
      model_map[[as.character(species)]] <- key
    }
  }

  summaries <- Map(
    function(model, key) {
      species <- if (identical(key, "pooled")) {
        NA_integer_
      } else {
        as.integer(sub("^spcd_", "", key))
      }
      .height_model_summary(model, key, species, form)
    },
    models, names(models)
  )
  model_fixed <- .height_bind_rows(lapply(summaries, `[[`, "fixed"))
  model_variance <- .height_bind_rows(lapply(summaries, `[[`, "variance"))
  random_effects <- .height_bind_rows(lapply(summaries, `[[`, "random"))
  internal_fixed <- lapply(summaries, `[[`, "internal")
  names(internal_fixed) <- names(models)
  active_keys <- unname(model_map[as.character(count_data$spcd)])
  fixed_rows <- match(active_keys, model_fixed$model_key)
  fixed_effects <- data.frame(
    spcd = count_data$spcd,
    model_key = active_keys,
    pooled = active_keys == "pooled",
    a = model_fixed$a[fixed_rows],
    b = model_fixed$b[fixed_rows],
    c = model_fixed$c[fixed_rows],
    stringsAsFactors = FALSE
  )
  variance_rows <- match(active_keys, model_variance$model_key)
  variance_components <- data.frame(
    spcd = count_data$spcd,
    model_key = active_keys,
    random_parameter = model_variance$random_parameter[variance_rows],
    random_effect_sd = model_variance$random_effect_sd[variance_rows],
    residual_sd = model_variance$residual_sd[variance_rows],
    stringsAsFactors = FALSE
  )
  package_version <- tryCatch(tv_version(), error = function(error) NA_character_)

  structure(
    list(
      data = data,
      form = form,
      fixed_effects = fixed_effects,
      variance_components = variance_components,
      groups_seen = sort(unique(as.character(data$group))),
      n_by_species = count_data,
      pooled_species = as.integer(pooled_species),
      package_version = package_version,
      units = "imperial",
      min_n = min_n,
      models = models,
      model_map = model_map,
      internal_fixed_effects = internal_fixed,
      random_effects = random_effects,
      convergence_pooled_species = as.integer(convergence_pooled)
    ),
    class = "height_fit"
  )
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
  model_key[eligible] <- unname(
    fit$model_map[as.character(as.integer(spcd[eligible]))]
  )
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

.height_simulate_interval <- function(fit, dbh, rows, predictions, re_form,
                                      level, nsim, seed) {
  lower <- upper <- rep(NA_real_, length(dbh))
  rng_state <- .height_rng_state(seed)
  on.exit(.height_restore_rng(rng_state), add = TRUE)
  stream <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  probabilities <- c((1 - level) / 2, 1 - (1 - level) / 2)
  for (row in rows) {
    assign(".Random.seed", stream, envir = .GlobalEnv)
    key <- predictions$model_key[[row]]
    variance <- fit$variance_components[
      match(key, fit$variance_components$model_key), , drop = FALSE
    ]
    center <- if (identical(re_form, "conditional")) {
      predictions$random_effect[[row]]
    } else {
      0
    }
    simulated_effect <- stats::rnorm(
      nsim, center, variance$random_effect_sd[[1L]]
    )
    simulated <- .height_evaluate(
      rep(dbh[[row]], nsim), fit$internal_fixed_effects[[key]], fit$form,
      simulated_effect
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

#' Predict tree height
#'
#' Predict total height from a fitted height model. Return a numeric vector or a table with
#' simulated limits.
#'
#' @param fit Required complete `height_fit` from [fit_height()]. No default.
#'   Missing or other objects are errors.
#' @param dbh Required positive finite numeric outside-bark breast-height diameters,
#'   in inches or centimeters. Length one repeats, otherwise one per tree. Omission is an error.
#'
#' Missing or nonfinite values give `na_input`
#'   and are omitted from fitting or return missing predictions. Example: `example_trees$dbh`.
#' @param spcd Required numeric Forest Inventory and Analysis tree inventory
#'   species codes, length one or one per tree. Positive whole numbers within
#'   R's integer range are accepted. Unitless.
#'
#' Omission is an error. Missing
#'   gives `na_input` and invalid codes give `unknown_species`.
#' @param group Atomic plot or stand labels, length one or one per tree. Default
#'   `NULL` treats fitting observations as one group and uses the common
#'   fitted relationship when predicting. Missing or empty groups are omitted
#'   during fitting.
#'
#' Missing or unseen groups use the common relationship
#'   during prediction. Unitless. Example: `group = example_trees_pnw$plot`.
#' @param re_form One character value, `'conditional'` (default) or `'population'`. Conditional
#'   predictions include an estimated difference for a known
#'   fitted group. Population predictions use the common relationship.
#'
#' Missing or unknown labels are errors. Unitless. Example: `'population'`.
#' @param interval One nonmissing logical value, default `FALSE`, returning only
#'   predicted heights. `TRUE` adds simulated limits. Invalid types are
#'   errors.
#'
#' Unitless. Example: `interval = TRUE`.
#' @param level One finite numeric probability strictly between zero and one. The probability
#' selects the central prediction interval. Missing is an error.
#'
#' Unitless.
#' @param nsim One positive whole number within R's integer range. Default
#'   `1000` is the simulation count per predicted tree. Missing or invalid
#'   values are errors.
#'
#' Unitless.
#' @param seed One nonnegative whole number within R's integer range, or `NULL`
#'   (default) to use the current random-number state. A supplied seed makes
#'   simulations reproducible while restoring the caller's random state. Missing is an error.
#'
#' Unitless.
#' @param units One character value, `'imperial'` (default) or `'metric'`. Imperial diameters and
#'   heights use inches and feet. Metric uses
#'   centimeters and meters.
#'
#' Missing or unknown values are errors. The fit is stored in inches and feet. Example: `units =
#' 'metric'`.
#'
#' @return A numeric height vector when `interval = FALSE`. Otherwise, a data
#'   frame with `fit`, `lower`, and `upper` columns.
#' @export
#' @usage
#'
#' ## Call signatures
#' predict_height(fit, dbh, spcd, group = NULL, re_form = 'conditional', interval = FALSE, level
#'   = 0.95, nsim = 1000, seed = NULL, units = 'imperial')
#' @examples
#' ## Fit the retained inventory heights
#' fit <- fit_height(dbh = example_trees_pnw$dbh,
#'                   ht = example_trees_pnw$ht_observed,
#'                   spcd = example_trees_pnw$spcd)
#'
#' ## Predict heights for the complete example trees
#' predict_height(fit = fit, dbh = example_trees$dbh, spcd = example_trees$spcd)
#' @section Fitted height fields:
#' The returned `height_fit` is a list. `form` names the equation. `data` is a data frame of
#' valid measured rows with numeric `dbh` in inches, `ht` in feet, integer species code `spcd`,
#' and factor `group`.
#'
#' No missing measurement rows remain. `fixed_effects` has integer `spcd`, character
#' `model_key`, logical `pooled`, and numeric `a`, `b`, `c`. These are the coefficients of the
#' equations in Details, using inches and feet.
#'
#' `c` is missing for forms without a third coefficient.
#'
#' `variance_components` has integer `spcd`, character `model_key` and
#' `random_parameter`, numeric `random_effect_sd` on the fitted parameter
#' scale, and numeric `residual_sd` in feet. `random_effects` has character
#' `model_key` and `group` plus numeric `effect` on that parameter scale.
#' `n_by_species` has integer `spcd` and integer `n`, the number of retained
#' measurements. Codes, names, flags, and counts are unitless.
#'
#' `groups_seen` lists character group labels. `pooled_species` lists integer species codes
#' below the count threshold. `convergence_pooled_species` lists those whose separate fit
#' failed.
#'
#' `model_map` maps species to model keys, and `models` holds the fitted R models.
#' `internal_fixed_effects` holds transformed coefficients by model key. `units` is
#' `'imperial'`, `min_n` is the supplied count threshold, and `package_version` records the
#' package version or is missing when unavailable.
#'
#' None is a data-frame list column. Keep the complete fitted object for prediction.
#' @section
#' Prediction rules and limits: Conditional prediction uses group differences only when a
#' matching fitted model and group are available. An unseen species has no assigned fit and
#' returns a missing prediction.
#'
#' A pooled species uses its assigned pooled model, including a known pooled group difference
#' for conditional prediction.
#'
#' The interval simulation holds fitted coefficients fixed. It draws group variation and
#' residual error, truncates simulated heights below breast height, and takes the requested
#' central quantiles. It does not include coefficient uncertainty or inventory sampling
#' uncertainty.
#'
#'
#' @section Status and missing values:
#' No status table is returned. `na_input` marks missing or nonfinite required values.
#' `dbh_nonpositive` marks nonpositive diameter. `unknown_species` marks invalid or unfitted
#' species.
#'
#' These predictions are missing.
#'
#' Diagnoses other than `na_input` warn. Missing groups use the common relationship and do not by
#' themselves
#' fail the prediction. Missing predictions do not establish zero volume.
#' @seealso
#' [fit_height()] to estimate a model, [complete_heights()] to retain existing heights,
#' [plot.height_fit()] to inspect the fitted relationship.
predict_height <- function(fit, dbh, spcd, group = NULL,
                           re_form = "conditional", interval = FALSE,
                           level = 0.95, nsim = 1000, seed = NULL,
                           units = "imperial") {
  fit <- .height_validate_fit(fit)
  re_form <- .scalar_character(
    re_form, "re_form", c("conditional", "population")
  )
  interval <- .height_validate_interval(interval)
  level <- .height_validate_level(level)
  nsim <- .height_validate_integer(nsim, "nsim")
  seed <- .height_validate_seed(seed)
  units <- .validate_units(units)
  group_missing <- is.null(group)
  values <- .height_prepare(dbh, spcd, group)
  status <- .height_status(values)
  prediction_rows <- .height_prediction_rows(
    fit, values$spcd, values$group, status
  )
  status <- prediction_rows$status
  native_dbh <- .diameter_to_native(values$dbh, units, "imperial")
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
      native_dbh[rows], fit$internal_fixed_effects[[key]], fit$form,
      random_effect
    )
  }
  .status_warning("predict_height", status, size = values$size)
  output <- .height_from_native(output, units, "imperial")
  if (!interval) {
    return(output)
  }
  limits <- list(
    lower = rep(NA_real_, values$size), upper = rep(NA_real_, values$size)
  )
  if (length(valid)) {
    simulation_form <- if (group_missing) "population" else re_form
    limits <- .height_simulate_interval(
      fit, native_dbh, valid, prediction_rows, simulation_form,
      level, nsim, seed
    )
  }
  data.frame(
    fit = output,
    lower = .height_from_native(limits$lower, units, "imperial"),
    upper = .height_from_native(limits$upper, units, "imperial")
  )
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
  form <- if (is.null(dots$form)) "chapman_richards" else dots$form
  min_n <- if (is.null(dots$min_n)) 7L else dots$min_n
  re_form <- if (is.null(dots$re_form)) "conditional" else dots$re_form
  list(
    form = .scalar_character(form, "form", .height_forms),
    min_n = .height_validate_integer(min_n, "min_n"),
    re_form = .scalar_character(
      re_form, "re_form", c("conditional", "population")
    )
  )
}

#' Complete missing tree heights
#'
#' Replace missing or nonfinite total heights using a fitted height model. Finite recorded
#' heights remain unchanged.
#'
#' @inheritParams fit_height
#' @param fit A complete `height_fit` object, or `NULL` (default) to fit from
#'   the supplied measured heights. Missing or invalid objects are errors. Pass a saved height fit.
#'
#' Fit once outside grouped data operations.
#' @param ... Optional `form` and `min_n` controls used when fitting, and a
#'   `re_form` control used when predicting.
#'
#' @details Inside a grouped `dplyr::mutate()`, `fit = NULL` fits a separate
#' model within every data group. Passing `fit` reuses that fitted model across groups.
#'
#' @return The numeric `ht` vector with only missing or non-finite values
#'   replaced. Existing finite heights are unchanged.
#' @export
#' @usage
#'
#' ## Call signatures
#' complete_heights(dbh, ht, spcd, group = NULL, fit = NULL, ..., units = 'imperial')
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Complete missing heights from retained observations
#' example_trees_pnw %>%
#'   mutate(ht = complete_heights(dbh = dbh, ht = ht_observed, spcd = spcd)) %>%
#'   filter(is.na(ht_observed)) %>%
#'   select(tree, dbh, ht_observed, ht) %>%
#'   slice_head(n = 3)
#' @section Fitting and prediction controls through dots:
#' `form` is one equation name, default `'chapman_richards'`, with alternatives `'curtis'`,
#' `'wykoff'`, `'naslund'`, and `'schumacher'`. `min_n` is one positive whole number, default
#' `7`, setting the retained height-count threshold. Both are used only when fitting here.
#'
#' `re_form` is one character label, default `'conditional'`, using known group differences, or
#' `'population'`, using the common fitted relationship. These controls are unitless. Missing
#' or unknown values are errors.
#'
#' Examples: `form = 'wykoff'`, `min_n = 10`, `re_form = 'population'`.
#' @section Status and missing values:
#' Only missing or nonfinite heights are replaced. Finite heights, including
#' nonpositive values, remain unchanged. Failed predictions remain missing, with the warning
#' behavior of [predict_height()].
#'
#' If no height needs replacement, the supplied heights are returned without fitting. A missing
#' height is not zero volume. Review completed heights before merchandising.
#' @inheritSection fit_height Fitted height fields
#' @seealso [fit_height()] to fit once for an inventory,
#' [predict_height()] for new height estimates, [merchandise()] to calculate completed trees.
complete_heights <- function(dbh, ht, spcd, group = NULL, fit = NULL, ...,
                             units = "imperial") {
  units <- .validate_units(units)
  controls <- .height_completion_dots(list(...))
  values <- .height_prepare(dbh, spcd, group, ht, include_ht = TRUE)
  if (!is.null(fit)) {
    fit <- .height_validate_fit(fit)
  }
  output <- values$ht
  missing <- !is.finite(output)
  output[missing] <- NA_real_
  if (!any(missing)) {
    return(output)
  }
  if (is.null(fit)) {
    fit <- fit_height(
      values$dbh, output, values$spcd, values$group,
      form = controls$form, min_n = controls$min_n, units = units
    )
  }
  missing_group <- if (is.null(group)) NULL else values$group[missing]
  output[missing] <- predict_height(
    fit, values$dbh[missing], values$spcd[missing], missing_group,
    re_form = controls$re_form, units = units
  )
  output
}
