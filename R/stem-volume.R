.stem_section_bounds <- function(from, to, from_dib, from_dob, to_dib, to_dob) {
  lower <- Filter(Negate(is.null), list(height = from, dib = from_dib, dob = from_dob))
  upper <- Filter(Negate(is.null), list(height = to, dib = to_dib, dob = to_dob))
  if (length(lower) > 1L)
    stop("Supply at most one of from, from_dib, and from_dob.", call. = FALSE)
  if (length(upper) > 1L)
    stop("Supply at most one of to, to_dib, and to_dob.", call. = FALSE)
  supplied <- Filter(Negate(is.null), list(
    from = from,
    to = to,
    from_dib = from_dib,
    from_dob = from_dob,
    to_dib = to_dib,
    to_dob = to_dob
  ))
  for (name in names(supplied)) {
    if (!is.numeric(supplied[[name]]))
      stop(name, " must be numeric.", call. = FALSE)
  }
  list(
    lower = if (length(lower)) lower[[1L]] else 0,
    lower_type = if (length(lower)) names(lower) else "stump",
    upper = if (length(upper)) upper[[1L]] else 0,
    upper_type = if (length(upper)) names(upper) else "tip"
  )
}

.bound_type_choices <- c("height", "dib", "dob", "stump", "tip")

.bound_argument_name <- function(prefix, type) {
  if (length(type) == 1L && type %in% c("dib", "dob"))
    paste0(prefix, "_", type) else prefix
}

.prepare_volume_call <- function(
  dbh, ht, model, lower, lower_type, upper,
  upper_type, stump_ht,
  aux
) {
  values <- list(
    dbh = dbh, ht = ht, model = model, lower = lower, lower_type = lower_type,
    upper = upper, upper_type = upper_type
  )
  numeric_names <- c("dbh", "ht", "lower", "upper")
  if (!is.null(stump_ht)) {
    values$stump_ht <- stump_ht
    numeric_names <- c(numeric_names, "stump_ht")
  }
  prepared <- .prepare_vectors(values,
    numeric_names = numeric_names,
    character_names = c(
      "model",
      "lower_type", "upper_type"
    ), aux = aux, aliases = c(
      lower = .bound_argument_name("from", lower_type),
      upper = .bound_argument_name("to", upper_type)
    )
  )
  if (anyNA(prepared$values$lower_type) || any(
    !prepared$values$lower_type %in% .bound_type_choices
  )) {
    stop("lower_type contains an unknown value.", call. = FALSE)
  }
  if (anyNA(prepared$values$upper_type) || any(
    !prepared$values$upper_type %in% .bound_type_choices
  )) {
    stop("upper_type contains an unknown value.", call. = FALSE)
  }
  required <- c("dbh", "ht", "model")
  if (!is.null(stump_ht)) {
    required <- c(required, "stump_ht")
  }
  status <- .input_status(prepared$size, prepared$values, required)
  bound_needs_value <- function(type) type %in% c("height", "dib", "dob")
  missing_lower <- bound_needs_value(prepared$values$lower_type) & !is.finite(
    prepared$values$lower
  )
  missing_upper <- bound_needs_value(prepared$values$upper_type) & !is.finite(
    prepared$values$upper
  )
  status <- .assign_status(status, missing_lower | missing_upper, 1L)
  status <- .assign_status(status, prepared$values$dbh <= 0 | prepared$values$dbh > 400, 2L)
  status <- .assign_status(status, prepared$values$ht <= 0 | prepared$values$ht > 500, 3L)
  lower_height <- prepared$values$lower_type == "height"
  upper_height <- prepared$values$upper_type == "height"
  status <- .assign_status(status, lower_height & (prepared$values$lower < 0 |
                                                     prepared$values$lower >
                                                       prepared$values$ht), 4L)
  status <- .assign_status(status, upper_height & (prepared$values$upper < 0 |
                                                     prepared$values$upper >
                                                       prepared$values$ht), 4L)
  diameter_bound <- prepared$values$lower_type %in% c("dib", "dob") &
    prepared$values$lower <=
      0
  diameter_bound <- diameter_bound | (prepared$values$upper_type %in% c(
    "dib",
    "dob"
  ) & prepared$values$upper <=
    0)
  status <- .assign_status(status, diameter_bound, 5L)
  if (!is.null(stump_ht)) {
    status <- .assign_status(status, prepared$values$stump_ht < 0 |
                               prepared$values$stump_ht >
                                 prepared$values$ht, 4L)
  }
  resolved <- .resolve_models(prepared$values$model, prepared$aux, status)
  c(prepared, list(resolved = resolved))
}

.resolve_one_bound <- function(type, value, model, dbh, ht, stump, aux, rows) {
  result <- list(value = rep(NA_real_, length(type)), status = integer(length(
    type
  )), details = rep(
    NA_character_,
    length(type)
  ))
  height_rows <- type == "height"
  stump_rows <- type == "stump"
  tip_rows <- type == "tip"
  result$value[height_rows] <- value[height_rows]
  result$value[stump_rows] <- stump[stump_rows]
  result$value[tip_rows] <- ht[tip_rows]
  for (basis in c("dib", "dob")) {
    selected <- which(type == basis)
    if (!length(selected)) {
      next
    }
    inverted <- .inverse_group(
      model, basis, dbh[selected], ht[selected], value[selected],
      aux, rows[selected]
    )
    result$value[selected] <- inverted$value
    result$status[selected] <- inverted$status
    result$details[selected] <- inverted$details
  }
  result
}

.resolve_group_bounds <- function(call, model, rows, measurement_system) {
  native_dbh <-
    .diameter_to_native(call$values$dbh[rows], measurement_system, model$measurement_system)
  native_ht <-
    .height_to_native(call$values$ht[rows], measurement_system, model$measurement_system)
  lower_value <- call$values$lower[rows]
  upper_value <- call$values$upper[rows]
  lower_diameter <- call$values$lower_type[rows] %in% c("dib", "dob")
  upper_diameter <- call$values$upper_type[rows] %in% c("dib", "dob")
  lower_value[lower_diameter] <- .diameter_to_native(
    lower_value[lower_diameter],
    measurement_system, model$measurement_system
  )
  upper_value[upper_diameter] <- .diameter_to_native(
    upper_value[upper_diameter],
    measurement_system, model$measurement_system
  )
  lower_height <- call$values$lower_type[rows] == "height"
  upper_height <- call$values$upper_type[rows] == "height"
  lower_value[lower_height] <- .height_to_native(
    lower_value[lower_height], measurement_system,
    model$measurement_system
  )
  upper_value[upper_height] <- .height_to_native(
    upper_value[upper_height], measurement_system,
    model$measurement_system
  )
  stump <- if (!is.null(call$values$stump_ht)) {
    .height_to_native(call$values$stump_ht[rows], measurement_system, model$measurement_system)
  } else {
    rep(model$stump_ht, length(rows))
  }
  lower <- .resolve_one_bound(
    call$values$lower_type[rows], lower_value, model, native_dbh,
    native_ht, stump, call$aux, rows
  )
  upper <- .resolve_one_bound(
    call$values$upper_type[rows], upper_value, model, native_dbh,
    native_ht, stump, call$aux, rows
  )
  list(dbh = native_dbh, ht = native_ht, stump = stump, lower = lower, upper = upper)
}

.gauss_legendre_integral <- function(model, basis, dbh, ht, lower, upper, aux, rows) {
  nodes <- c(
    -0.9061798459386640, -0.5384693101056831, 0, 0.5384693101056831,
    0.9061798459386640
  )
  weights <- c(
    0.2369268850561891, 0.4786286704993665, 0.5688888888888889,
    0.4786286704993665,
    0.2369268850561891
  )
  segment_size <- if (identical(model$measurement_system, "imperial"))
    1 else 0.3048
  all_h <- all_weight <- numeric()
  map <- integer()
  for (tree in seq_along(dbh)) {
    breaks <- seq(lower[[tree]], upper[[tree]], by = segment_size)
    if (!length(breaks) || utils::tail(breaks, 1L) < upper[[tree]]) {
      breaks <- c(breaks, upper[[tree]])
    }
    if (length(breaks) == 1L) {
      breaks <- c(breaks, upper[[tree]])
    }
    for (segment in seq_len(length(breaks) - 1L)) {
      midpoint <- (breaks[[segment]] + breaks[[segment + 1L]]) / 2
      half_width <- (breaks[[segment + 1L]] - breaks[[segment]]) / 2
      all_h <- c(all_h, midpoint + half_width * nodes)
      all_weight <- c(all_weight, half_width * weights)
      map <- c(map, rep(tree, 5L))
    }
  }
  evaluated <- .evaluate_profile_native(
    model, basis, dbh[map], ht[map], all_h, aux,
    rows[map]
  )
  result <- list(
    value = rep(NA_real_, length(dbh)), status = integer(length(dbh)),
    details = rep(
      NA_character_,
      length(dbh)
    )
  )
  divisor <- if (identical(model$measurement_system, "imperial"))
    576 else 40000
  area_volume <- pi * evaluated$value^2 / divisor * all_weight
  for (tree in seq_along(dbh)) {
    selected <- map == tree
    bad <- which(evaluated$status[selected] != 0L)
    if (length(bad)) {
      first <- which(selected)[bad[[1L]]]
      result$status[[tree]] <- evaluated$status[[first]]
      result$details[[tree]] <- evaluated$details[[first]]
    } else {
      result$value[[tree]] <- sum(area_volume[selected])
    }
  }
  result
}

.analytic_integral <- function(model, dbh, ht, lower, upper, aux, rows) {
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = FALSE)$value
    return(.compiled_result(model, 3L, dbh, ht, lower, upper, bark, aux, rows))
  }
  .callback_result(model$volume, list(
    dbh = dbh, ht = ht, lower = lower,
    upper = upper, aux = .subset_aux(
      aux,
      rows, model
    )
  ), length(dbh))
}

.integrate_group <- function(
  model, basis, dbh, ht, lower, upper, aux, rows,
  operation = NULL
) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = identical(basis, "dob") &&
                                !direct_dob)$value
    return(.compiled_result(model, if (is.null(operation)) {
      if (identical(basis, "dib")) 3L else 8L
    } else {
      as.integer(operation)
    }, dbh, ht, lower, upper, bark, aux, rows))
  }
  if (identical(basis, "dib") && isTRUE(model$kernel$has_integral)) {
    return(.analytic_integral(model, dbh, ht, lower, upper, aux, rows))
  }
  if (identical(basis, "dob") && !direct_dob && isTRUE(model$kernel$has_integral)) {
    bark <- .model_bark_ratio(model, aux, rows, require = TRUE)
    result <- list(value = rep(NA_real_, length(dbh)), status = ifelse(bark$invalid, 53L,
                     0L
                   ), details = rep(NA_character_, length(dbh)))
    good <- which(!bark$invalid)
    if (length(good)) {
      inside <- .analytic_integral(
        model, dbh[good], ht[good], lower[good], upper[good],
        aux, rows[good]
      )
      result$value[good] <- inside$value / bark$value[good]^2
      result$status[good] <- inside$status
      result$details[good] <- inside$details
    }
    return(result)
  }
  .gauss_legendre_integral(model, basis, dbh, ht, lower, upper, aux, rows)
}

.stem_volume_impl <- function(
  dbh, ht, model, lower, lower_type, upper, upper_type,
  bark, stump_ht,
  aux, measurement_system, status, function_name = "stem_volume", operation = NULL
) {
  measurement_system <- .validate_units(measurement_system)
  status_requested <- .validate_status(status)
  bark <- .scalar_character(bark, "bark", c("inside", "outside"))
  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_volume_call(
    dbh, ht, model, lower, lower_type, upper, upper_type,
    stump_ht,
    aux
  )
  call$aux <- .set_aux_caller_units(call$aux, measurement_system)
  result_status <- call$resolved$status
  details <- call$resolved$details
  output <- rep(NA_real_, call$size)
  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    group_rows <- call$resolved$groups[[as.character(group_index)]]
    rows <- group_rows[result_status[group_rows] %in% c(0L, 52L)]
    if (!length(rows)) {
      next
    }
    bounds <- .resolve_group_bounds(call, model_object, rows, measurement_system)
    lower_merged <- .merge_kernel_result(result_status, details, rows, bounds$lower)
    result_status <- lower_merged$status
    details <- lower_merged$details
    upper_merged <- .merge_kernel_result(result_status, details, rows, bounds$upper)
    result_status <- upper_merged$status
    details <- upper_merged$details
    empty <- is.finite(bounds$lower$value) & is.finite(bounds$upper$value) &
      bounds$lower$value >=
        bounds$upper$value
    result_status[rows[empty]] <- 6L
    eligible <- result_status[rows] %in% c(0L, 52L, 102L)
    if (!any(eligible)) {
      next
    }
    selected_rows <- rows[eligible]
    integrated <- .integrate_group(
      model_object, if (identical(bark, "inside"))
        "dib" else "dob", bounds$dbh[eligible], bounds$ht[eligible],
      bounds$lower$value[eligible],
      bounds$upper$value[eligible], call$aux, selected_rows, operation
    )
    merged <- .merge_kernel_result(result_status, details, selected_rows, integrated)
    result_status <- merged$status
    details <- merged$details
    output[selected_rows] <- .volume_from_native(
      integrated$value, measurement_system,
      model_object$measurement_system
    )
  }
  output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
  .status_result(output, result_status, status_requested, details, function_name)
}

#' Cubic volume of a stem section
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default. Heights must be greater than zero and at most 500 feet.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param model Registered taper model identifier. Character vector of
#'   registered model identifiers. Default: \code{NULL}.
#'   model NULL selects the shipped default for the species, see [default_taper_models].
#'   Species without a default return status 404.
#' @param from Start height above ground. Numeric vector, feet. Default `NULL` uses `stump_ht`.
#' @param to End height above ground. Numeric vector, feet. Default `NULL` uses the tree tip.
#' @param from_dib Inside bark diameter at the start. Numeric vector, inches, default `NULL`.
#' @param from_dob Outside bark diameter at the start. Numeric vector, inches, default `NULL`.
#' @param to_dib Inside bark diameter at the end. Numeric vector, inches, default `NULL`.
#' @param to_dob Outside bark diameter at the end. Numeric vector, inches, default `NULL`.
#'   Diameter bounds use [height_at_dib()] or [height_at_dob()], including their status conventions.
#'   Supply at most one start argument and at most one end argument.
#' @param inside_bark Choose whether the measurement excludes the bark. Logical scalar, unitless,
#'   default TRUE excludes bark.
#' @param stump_ht Stump height above ground. Numeric
#'   vector, feet. Default: \code{1}.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A data frame with value (solid volume, cubic feet) and status (integer result code),
#'   one row per input row.
#' @usage
#' stem_volume(
#'   dbh,
#'   ht,
#'   spcd,
#'   model = NULL,
#'   from = NULL,
#'   to = NULL,
#'   from_dib = NULL,
#'   from_dob = NULL,
#'   to_dib = NULL,
#'   to_dob = NULL,
#'   inside_bark = TRUE,
#'   stump_ht = 1,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure the whole stem inside bark.
#' stem_volume(dbh = example_trees$dbh[1],
#'             ht = example_trees$ht[1],
#'             spcd = example_trees$spcd[1]) %>%
#'   rename(`volume (cubic feet)` = value)
stem_volume <- function(
  dbh,
  ht,
  spcd,
  model = NULL,
  from = NULL,
  to = NULL,
  from_dib = NULL,
  from_dob = NULL,
  to_dib = NULL,
  to_dob = NULL,
  inside_bark = TRUE,
  stump_ht = 1,
  ...
) {
  if (!is.logical(inside_bark) || length(inside_bark) != 1 || is.na(inside_bark)) {
    stop("inside_bark must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(stump_ht))
    stop("stump_ht must be numeric.", call. = FALSE)
  bounds <- .stem_section_bounds(from, to, from_dib, from_dob, to_dib, to_dob)
  input <- .mc_stem_inputs(spcd, model, list(...), !inside_bark)
  .public_status(.stem_volume_impl(
    dbh,
    ht,
    input$model,
    bounds$lower,
    bounds$lower_type,
    bounds$upper,
    bounds$upper_type,
    if (inside_bark) "inside" else "outside",
    stump_ht,
    input$aux,
    "imperial",
    TRUE
  ))
}
