.bound_type_choices <- c("height", "dib", "dob", "stump", "tip")

.prepare_volume_call <- function(dbh, ht, model, lower, lower_type, upper,
                                 upper_type, stump_ht, aux) {
  values <- list(
    dbh = dbh, ht = ht, model = model, lower = lower,
    lower_type = lower_type, upper = upper, upper_type = upper_type
  )
  numeric_names <- c("dbh", "ht", "lower", "upper")
  if (!is.null(stump_ht)) {
    values$stump_ht <- stump_ht
    numeric_names <- c(numeric_names, "stump_ht")
  }
  prepared <- .prepare_vectors(
    values,
    numeric_names = numeric_names,
    character_names = c("model", "lower_type", "upper_type"),
    aux = aux
  )
  if (anyNA(prepared$values$lower_type) ||
        any(!prepared$values$lower_type %in% .bound_type_choices)) {
    stop("lower_type contains an unknown value.", call. = FALSE)
  }
  if (anyNA(prepared$values$upper_type) ||
        any(!prepared$values$upper_type %in% .bound_type_choices)) {
    stop("upper_type contains an unknown value.", call. = FALSE)
  }
  required <- c("dbh", "ht", "model")
  if (!is.null(stump_ht)) {
    required <- c(required, "stump_ht")
  }
  status <- .input_status(prepared$size, prepared$values, required)
  bound_needs_value <- function(type) type %in% c("height", "dib", "dob")
  missing_lower <- bound_needs_value(prepared$values$lower_type) &
    !is.finite(prepared$values$lower)
  missing_upper <- bound_needs_value(prepared$values$upper_type) &
    !is.finite(prepared$values$upper)
  status <- .assign_status(status, missing_lower | missing_upper, 1L)
  status <- .assign_status(status, prepared$values$dbh <= 0, 2L)
  status <- .assign_status(status, prepared$values$ht <= 0, 3L)
  lower_height <- prepared$values$lower_type == "height"
  upper_height <- prepared$values$upper_type == "height"
  status <- .assign_status(
    status,
    lower_height & (prepared$values$lower < 0 | prepared$values$lower > prepared$values$ht),
    4L
  )
  status <- .assign_status(
    status,
    upper_height & (prepared$values$upper < 0 | prepared$values$upper > prepared$values$ht),
    4L
  )
  diameter_bound <- prepared$values$lower_type %in% c("dib", "dob") &
    prepared$values$lower <= 0
  diameter_bound <- diameter_bound |
    (prepared$values$upper_type %in% c("dib", "dob") & prepared$values$upper <= 0)
  status <- .assign_status(status, diameter_bound, 5L)
  if (!is.null(stump_ht)) {
    status <- .assign_status(
      status,
      prepared$values$stump_ht < 0 | prepared$values$stump_ht > prepared$values$ht,
      4L
    )
  }
  resolved <- .resolve_models(prepared$values$model, prepared$aux, status)
  c(prepared, list(resolved = resolved))
}

.resolve_one_bound <- function(type, value, model, dbh, ht, stump, aux, rows) {
  result <- list(
    value = rep(NA_real_, length(type)),
    status = integer(length(type)),
    details = rep(NA_character_, length(type))
  )
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
      model, basis, dbh[selected], ht[selected], value[selected], aux, rows[selected]
    )
    result$value[selected] <- inverted$value
    result$status[selected] <- inverted$status
    result$details[selected] <- inverted$details
  }
  result
}

.resolve_group_bounds <- function(call, model, rows, units) {
  native_dbh <- .diameter_to_native(call$values$dbh[rows], units, model$units)
  native_ht <- .height_to_native(call$values$ht[rows], units, model$units)
  lower_value <- call$values$lower[rows]
  upper_value <- call$values$upper[rows]
  lower_diameter <- call$values$lower_type[rows] %in% c("dib", "dob")
  upper_diameter <- call$values$upper_type[rows] %in% c("dib", "dob")
  lower_value[lower_diameter] <- .diameter_to_native(
    lower_value[lower_diameter], units, model$units
  )
  upper_value[upper_diameter] <- .diameter_to_native(
    upper_value[upper_diameter], units, model$units
  )
  lower_height <- call$values$lower_type[rows] == "height"
  upper_height <- call$values$upper_type[rows] == "height"
  lower_value[lower_height] <- .height_to_native(
    lower_value[lower_height], units, model$units
  )
  upper_value[upper_height] <- .height_to_native(
    upper_value[upper_height], units, model$units
  )
  stump <- if (!is.null(call$values$stump_ht)) {
    .height_to_native(call$values$stump_ht[rows], units, model$units)
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

.gauss_legendre_integral <- function(model, basis, dbh, ht, lower, upper,
                                     aux, rows) {
  nodes <- c(
    -0.9061798459386640, -0.5384693101056831, 0,
    0.5384693101056831, 0.9061798459386640
  )
  weights <- c(
    0.2369268850561891, 0.4786286704993665, 0.5688888888888889,
    0.4786286704993665, 0.2369268850561891
  )
  segment_size <- if (identical(model$units, "imperial")) 1 else 0.3048
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
    model, basis, dbh[map], ht[map], all_h, aux, rows[map]
  )
  result <- list(
    value = rep(NA_real_, length(dbh)),
    status = integer(length(dbh)),
    details = rep(NA_character_, length(dbh))
  )
  divisor <- if (identical(model$units, "imperial")) 576 else 40000
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
    return(.compiled_result(
      model, 3L, dbh, ht, lower, upper, bark, aux, rows
    ))
  }
  .callback_result(
    model$volume,
    list(
      dbh = dbh, ht = ht, lower = lower, upper = upper,
      aux = .subset_aux(aux, rows, model)
    ),
    length(dbh)
  )
}

.integrate_group <- function(model, basis, dbh, ht, lower, upper, aux, rows,
                             operation = NULL) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(
      model, aux, rows, require = identical(basis, "dob") && !direct_dob
    )$value
    return(.compiled_result(
      model, if (is.null(operation)) {
        if (identical(basis, "dib")) 3L else 8L
      } else {
        as.integer(operation)
      },
      dbh, ht, lower, upper, bark, aux, rows
    ))
  }
  if (identical(basis, "dib") && isTRUE(model$kernel$has_integral)) {
    return(.analytic_integral(model, dbh, ht, lower, upper, aux, rows))
  }
  if (identical(basis, "dob") && !direct_dob &&
        isTRUE(model$kernel$has_integral)) {
    bark <- .model_bark_ratio(model, aux, rows, require = TRUE)
    result <- list(
      value = rep(NA_real_, length(dbh)),
      status = ifelse(bark$invalid, 53L, 0L),
      details = rep(NA_character_, length(dbh))
    )
    good <- which(!bark$invalid)
    if (length(good)) {
      inside <- .analytic_integral(
        model, dbh[good], ht[good], lower[good], upper[good], aux, rows[good]
      )
      result$value[good] <- inside$value / bark$value[good]^2
      result$status[good] <- inside$status
      result$details[good] <- inside$details
    }
    return(result)
  }
  .gauss_legendre_integral(model, basis, dbh, ht, lower, upper, aux, rows)
}

.stem_volume_impl <- function(dbh, ht, model, lower, lower_type, upper,
                              upper_type, bark, stump_ht, aux, units, status,
                              function_name = "stem_volume",
                              operation = NULL) {
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  bark <- .scalar_character(bark, "bark", c("inside", "outside"))
  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_volume_call(
    dbh, ht, model, lower, lower_type, upper, upper_type, stump_ht, aux
  )
  call$aux <- .set_aux_caller_units(call$aux, units)
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
    bounds <- .resolve_group_bounds(call, model_object, rows, units)
    lower_merged <- .merge_kernel_result(
      result_status, details, rows, bounds$lower
    )
    result_status <- lower_merged$status
    details <- lower_merged$details
    upper_merged <- .merge_kernel_result(
      result_status, details, rows, bounds$upper
    )
    result_status <- upper_merged$status
    details <- upper_merged$details
    empty <- is.finite(bounds$lower$value) & is.finite(bounds$upper$value) &
      bounds$lower$value >= bounds$upper$value
    result_status[rows[empty]] <- 6L
    eligible <- result_status[rows] %in% c(0L, 52L, 102L)
    if (!any(eligible)) {
      next
    }
    selected_rows <- rows[eligible]
    integrated <- .integrate_group(
      model_object,
      if (identical(bark, "inside")) "dib" else "dob",
      bounds$dbh[eligible], bounds$ht[eligible], bounds$lower$value[eligible],
      bounds$upper$value[eligible], call$aux, selected_rows, operation
    )
    merged <- .merge_kernel_result(
      result_status, details, selected_rows, integrated
    )
    result_status <- merged$status
    details <- merged$details
    output[selected_rows] <- .volume_from_native(
      integrated$value, units, model_object$units
    )
  }
  output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
  .status_result(output, result_status, status_requested, details, function_name)
}

#' Calculate stem volume between heights or diameter limits
#'
#' Calculate inside- or outside-bark volume between height or diameter bounds. Return one
#' volume per tree, optionally paired with calculation status.
#'
#' @param dbh Required positive finite numeric diameter at breast height
#'   outside bark, in inches for imperial units or centimeters for metric
#'   units. Length one repeats, otherwise supply one value per tree. Omission
#'   is an error.
#'
#' Missing or nonfinite values give `na_input` and a missing result. Example: `example_trees$dbh`.
#' @param ht Required positive finite numeric total height above ground, in
#'   feet for imperial units or meters for metric units. Length one repeats,
#'   otherwise supply one value per tree. Omission is an error.
#'
#' Missing or
#'   nonfinite values give `na_input`. Example: `example_trees$ht`.
#' @param model Required character vector or factor of equation identifiers,
#'   length one or one per tree. Unitless. Omission is an error, missing values
#'   give `na_input`, and unknown identifiers give `unknown_model`.
#'
#' Example:
#'   `example_trees$model`. No replacement equation is chosen here.
#' @param ... Uniquely named equation inputs listed in the corresponding section. Each has length
#'   one or one per tree.
#'
#' Omit optional inputs to use the selected equation's
#'   defaults. Unknown names, invalid types, and invalid finite values stop
#'   the call. Missing supplied inputs give `na_input`, and an omitted required
#'   input gives `missing_input`.
#' @param units One character value, `'imperial'` (default) or `'metric'`. Omission selects inches,
#'   feet, and cubic feet. Metric selects centimeters,
#'   meters, and cubic meters.
#'
#' Missing and unknown values are errors. Example: `units = 'metric'`.
#' @param status One nonmissing logical value, default `FALSE`. `TRUE` returns
#'   numeric `value` and integer `status` columns and suppresses row warnings. Both columns have
#' one row per tree.
#'
#' The code is unitless. Invalid types
#'   or missing flags stop the call. Example: `status = TRUE`.
#' @param lower,upper Numeric boundary values, length one or one per tree. Defaults are `0` for
#'   both. Values are ignored for `'stump'` and `'tip'`.
#'
#' For `'height'`, use feet or meters from zero through total height. For
#'   diameter bounds, use positive inches or centimeters. Missing active
#'   values give `na_input`.
#'
#'
#' @param lower_type,upper_type Character boundary types, length one or one
#'   per tree. Choices are `'height'`, `'dib'`, `'dob'`, `'stump'`, and
#'   `'tip'`. Defaults are `'stump'` below and `'tip'` above.
#'
#' They select
#'   ground-relative height, inside-bark diameter, outside-bark diameter,
#'   equation stump convention, or total height. Missing or unknown types
#'   stop the call. Unitless.
#'
#'
#' @param bark One character value, `'inside'` (default) or `'outside'`,
#'   selecting the volume's bark basis. Missing or unknown labels are errors. Unitless.
#'
#' Example: `bark = 'outside'`.
#' @param stump_ht Numeric stump override, length one or one per tree, in
#'   feet or meters. Default `NULL` uses each equation's stump convention.
#'   Supplied values must be finite and from zero through total height.
#'   Missing gives `na_input` even when the selected bounds do not use stump.
#'
#'
#' @details Diameter bounds become the highest corresponding crossing. The lower resolved height
#' must be strictly below the upper height. Equal bounds return missing volume with `empty_bounds`.
#'
#' This differs from an
#'   empty section deliberately assigned zero in a merchandising result. No product, trim, scale,
#' defect deduction, or price is inferred.
#' @section Numerical methods:
#' Use the equation's volume calculation where supplied. Other equations
#' are integrated with five-point Gauss-Legendre quadrature, an area-weighted
#' numerical sum over sections no longer than one foot. Source equations can
#' use separate diameter and volume relationships, so volume need not equal
#' a numerical integral of the displayed diameter curve.
#' @return Numeric cubic feet or cubic meters per input tree. With
#'   `status = TRUE`, return numeric `value` in those units and integer
#'   unitless `status`. Failed quantities remain missing.
#' @section Equation inputs through dots:
#' Inputs are used only where declared by the selected equation. Numeric
#' inputs accept integers and doubles. Each input has length one or the common
#' number of trees, with units following the call. For a particular model,
#' `get_taper_model(model)$inputs` lists required, optional, and paired inputs.
#'
#' \describe{
#' \item{`bark_ratio`}{Numeric ratio of inside-bark to outside-bark diameter,
#' strictly greater than zero and at most one. Unitless.
#' Omission uses a model ratio where supplied.
#' An explicit missing ratio gives `na_input`, not the default.}
#' \item{`upper_ht1`, `upper_ht2`}{Positive finite numeric upper measurement
#' heights above ground, in feet or meters.
#' Omission supplies no measurement. A required missing height gives `missing_input`
#' when omitted or `na_input` when supplied as missing.}
#' \item{`upper_d1`, `upper_d2`}{Positive finite numeric diameters at the
#' corresponding upper heights, in inches or centimeters.  Supply each diameter with its paired
#' height where the model requires
#' a pair. An incomplete pair gives `missing_input`.}
#' \item{`upper_bark`}{Character `'ib'` or `'ob'` for the upper
#' measurements, unitless. Example: `'ib'`. Omission uses the model
#' convention, which is inside bark for Flewelling upper measurements.
#' A missing supplied label gives `na_input`.}
#' \item{`form_class`}{Positive numeric equation form class, unitless.
#'  Its definition follows the selected equation. Omission
#' uses its default only when optional. Missing supplied values give `na_input`.}
#' \item{`site_index`}{Positive numeric site-index height, in feet or meters.
#'  The equation determines species and reference age.
#' Omission uses its defaults only when optional. Missing gives `na_input`.}
#' \item{`basal_area`}{Positive numeric stand basal area, in square feet per
#' acre or square meters per hectare.  Omission uses the
#' model's defaults only when optional. Missing gives `na_input`.}
#' \item{`decay_class`}{Whole-number class from 1 through 5, unitless.
#'  This is an equation input only where declared, with no
#' universal default. Missing gives `na_input`. The separate biomass function
#' also accepts zero for a live tree.}
#' \item{`cull`}{Numeric percentage from zero through 100.
#' Omission supplies no override. Missing gives `na_input` where declared.
#' This does not create located defect records.}
#' \item{`spcd`}{Positive whole-number Forest Inventory and Analysis tree
#' inventory species code within R's integer range, unitless.
#' Omission skips the species-scope check. Supplied codes can return `na_input`, `unknown_species`,
#' or `species_out_of_scope`.}
#' }
#' @section Status and missing values:
#' Structural errors stop the call. Row failures return missing values unless
#' a retained-value case is explicitly identified below. `na_input` is silent
#' with `status = FALSE`, while other nonzero codes produce warnings.
#' \describe{
#' \item{`ok`}{Calculation completed. The value is usable within the selected equation's scope.}
#' \item{`na_input`}{A required value is missing or nonfinite. Supply the measurement
#'   before using this row.}
#' \item{`dbh_nonpositive`}{Tree diameter is not positive. Correct the diameter and rerun.}
#' \item{`ht_nonpositive`}{Total height is not positive. Correct the height and rerun.}
#' \item{`unknown_species`}{The supplied species code is invalid or unrecognized for this
#'   operation. Correct it.}
#' \item{`unknown_model`}{No equation was found. Check the identifier or register the
#'   local equation.}
#' \item{`missing_input`}{An equation requires an omitted input or complete measurement
#'   pair. Supply it and rerun.}
#' \item{`species_out_of_scope`}{A value is retained for a species outside the declared
#'   scope. Review equation suitability before reporting.}
#' \item{`capability_missing`}{The requested calculation is unavailable, often because
#'   outside-bark information is absent. Supply a supported bark ratio or choose another
#'   equation.}
#' \item{`kernel_error`}{The equation calculation failed to
#'   evaluate. The result is missing. Check the
#'   tree and equation separately.}
#' \item{`h_out_of_range`}{A height is below ground or above total height. Check height
#'   units and bounds.}
#' \item{`diameter_nonpositive`}{The target diameter is not positive. Supply a positive
#'   target or use the tip bound for volume.}
#' \item{`empty_bounds`}{The lower height is not below the upper height. Correct the
#'   interval. The missing result is not a measured zero volume.}
#' \item{`above_tip`}{The requested diameter is smaller than the modeled tip diameter.
#'   Revise the target.}
#' \item{`below_stump`}{The requested diameter exceeds the modeled diameter at stump.
#'   Revise the target or stump convention.}
#' \item{`not_unique`}{The profile has another diameter crossing below the returned
#'   height. The highest crossing is retained. Review whether it matches the intended
#'   utilization limit.}
#' \item{`no_convergence`}{The numerical search did not converge. Review the target and
#'   profile before using the tree.}
#' }
#' Source equation errors retain their library diagnosis. Those values are
#' unavailable.
#'
#' Check the selected equation and its inputs. Zero volume or diameter can be a modeled boundary
#' value. These functions
#' do not select logs, so a zero is not evidence of merchantable zero volume.
#'
#' @seealso [stem_profile()] for volumes at several heights, [merchandise()]
#'   for volume under product specifications, [height_at_dib()] for a height.
#' @export
#' @usage
#'
#' ## Call signatures
#' stem_volume(dbh, ht, model, lower = 0, lower_type = 'stump', upper = 0, upper_type = 'tip',
#'   bark = 'inside', stump_ht = NULL, ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Estimate volume between supplied height limits
#' stem_volume(dbh = example_trees$dbh,
#'             ht = example_trees$ht,
#'             model = example_trees$model,
#'             lower = 10,
#'             lower_type = 'height',
#'             upper = 40,
#'             upper_type = 'height')
stem_volume <- function(dbh, ht, model, lower = 0, lower_type = "stump",
                        upper = 0, upper_type = "tip", bark = "inside",
                        stump_ht = NULL, ..., units = "imperial", status = FALSE) {
  .stem_volume_impl(
    dbh, ht, model, lower, lower_type, upper, upper_type, bark, stump_ht,
    list(...), units, status
  )
}
