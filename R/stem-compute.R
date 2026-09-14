.tv_cpp_kernel_eval_impl <- tv_cpp_kernel_eval_impl
.tv_cpp_duplicate_map_impl <- tv_cpp_duplicate_map_impl

tv_cpp_kernel_eval <- function(kernel_name, operation, dbh, ht, x1, x2,
                               bark_ratio, threads, upper_ht1 = NULL,
                               upper_d1 = NULL, upper_ht2 = NULL,
                               upper_d2 = NULL, upper_bark = NULL,
                               site_index = NULL, basal_area = NULL,
                               form_class = NULL, coefficients = numeric()) {
  size <- length(dbh)
  numeric_default <- function(value) {
    if (is.null(value)) rep(NA_real_, size) else value
  }
  if (is.null(upper_bark)) {
    upper_bark <- integer(size)
  }
  result <- .tv_cpp_kernel_eval_impl(
    kernel_name, operation, dbh, ht, x1, x2, bark_ratio,
    numeric_default(upper_ht1), numeric_default(upper_d1),
    numeric_default(upper_ht2), numeric_default(upper_d2),
    upper_bark, numeric_default(site_index), numeric_default(basal_area),
    numeric_default(form_class), coefficients,
    identical(.treevolume_compat(), "nvel"), threads
  )
  structured <- startsWith(kernel_name, "flewelling:") |
    startsWith(kernel_name, "clark:") |
    startsWith(kernel_name, "r10:") |
    startsWith(kernel_name, "r4mat:") |
    startsWith(kernel_name, "smalltapers:") |
    startsWith(kernel_name, "nsvb:") |
    startsWith(kernel_name, "published:") |
    operation %in% c(6L, 7L, 8L)
  if (structured) result else result$value
}

.callback_result <- function(callback, arguments, size) {
  condition_message <- NULL
  value <- tryCatch(
    withCallingHandlers(
      do.call(callback, arguments),
      condition = function(condition) {
        stop(conditionMessage(condition), call. = FALSE)
      }
    ),
    error = function(error) {
      condition_message <<- conditionMessage(error)
      NULL
    }
  )
  if (!is.null(condition_message)) {
    return(list(
      value = rep(NA_real_, size),
      status = rep(54L, size),
      details = rep(condition_message, size)
    ))
  }
  supplied_status <- attr(value, "status", exact = TRUE)
  if (!is.double(value) || length(value) != size) {
    return(list(
      value = rep(NA_real_, size),
      status = rep(54L, size),
      details = rep("kernel returned the wrong type or length", size)
    ))
  }
  if (is.null(supplied_status)) {
    supplied_status <- integer(size)
  }
  valid_status <- is.numeric(supplied_status) && length(supplied_status) == size &&
    all(is.finite(supplied_status)) && all(supplied_status <= .Machine$integer.max) &&
    all(supplied_status == floor(supplied_status)) &&
    all(supplied_status %in% .tv_status_table$code | supplied_status > 300L)
  if (!valid_status) {
    return(list(
      value = rep(NA_real_, size),
      status = rep(54L, size),
      details = rep("kernel returned an invalid status attribute", size)
    ))
  }
  supplied_status <- as.integer(supplied_status)
  supplied_status[!is.finite(value) & supplied_status == 0L] <- 54L
  details <- rep(NA_character_, size)
  details[!is.finite(value) & supplied_status == 54L] <- "kernel returned a non-finite value"
  value[!is.finite(value)] <- NA_real_
  value[!supplied_status %in% c(0L, 52L, 102L)] <- NA_real_
  list(value = value, status = supplied_status, details = details)
}

.compiled_auxiliary <- function(aux, name, size) {
  if (is.null(aux[[name]])) {
    return(rep(NA_real_, size))
  }
  as.double(aux[[name]])
}

.compiled_result <- function(model, operation, dbh, ht, x1, x2, bark_ratio,
                             aux = list(), rows = seq_along(dbh)) {
  size <- length(dbh)
  native_aux <- .subset_aux(aux, rows, model)
  flewelling <- startsWith(model$kernel$key, "flewelling:")
  native_bark <- if (flewelling && is.null(native_aux$bark_ratio)) {
    rep(0, size)
  } else {
    bark_ratio
  }
  upper_bark <- if (is.null(native_aux$upper_bark)) {
    integer(size)
  } else {
    match(native_aux$upper_bark, c("ob", "ib"), nomatch = 0L)
  }
  inputs <- list(
    dbh = dbh, ht = ht, x1 = x1, x2 = x2, bark_ratio = native_bark,
    upper_ht1 = .compiled_auxiliary(native_aux, "upper_ht1", size),
    upper_d1 = .compiled_auxiliary(native_aux, "upper_d1", size),
    upper_ht2 = .compiled_auxiliary(native_aux, "upper_ht2", size),
    upper_d2 = .compiled_auxiliary(native_aux, "upper_d2", size),
    upper_bark = as.integer(upper_bark),
    site_index = .compiled_auxiliary(native_aux, "site_index", size),
    basal_area = .compiled_auxiliary(native_aux, "basal_area", size),
    form_class = .compiled_auxiliary(native_aux, "form_class", size)
  )
  duplicate_map <- do.call(.tv_cpp_duplicate_map_impl, inputs)
  evaluate_inputs <- inputs
  if (length(duplicate_map)) {
    evaluate_inputs <- lapply(inputs, `[`, duplicate_map$unique)
  }
  evaluated <- do.call(tv_cpp_kernel_eval, c(
    list(kernel_name = model$kernel$key, operation = operation),
    evaluate_inputs,
    list(threads = threads(), coefficients = as.double(model$data))
  ))
  if (is.numeric(evaluated)) {
    evaluated <- list(value = evaluated, status = integer(length(evaluated)))
  }
  if (length(duplicate_map)) {
    evaluated$value <- evaluated$value[duplicate_map$map]
    evaluated$status <- evaluated$status[duplicate_map$map]
  }
  value <- evaluated$value
  status <- evaluated$status
  status[!is.finite(value) & status == 0L] <- 54L
  value[!is.finite(value)] <- NA_real_
  list(
    value = value,
    status = status,
    details = ifelse(status == 54L, "compiled kernel returned a non-finite value", NA_character_)
  )
}

.model_bark_ratio <- function(model, aux, rows, require = FALSE) {
  value <- if (!is.null(aux$bark_ratio)) {
    as.double(aux$bark_ratio[rows])
  } else {
    rep(model$bark_ratio, length(rows))
  }
  invalid <- !is.finite(value) | value <= 0 | value > 1
  if (!require) {
    value[invalid] <- 1
    invalid[] <- FALSE
  }
  list(value = value, invalid = invalid)
}

.merge_kernel_result <- function(status, details, rows, result) {
  current <- status[rows]
  returned <- result$status
  model_before_numeric <- current >= 100L & current < 150L &
    returned >= 50L & returned < 100L & returned != 52L
  replace <- returned != 0L & (
    current == 0L | model_before_numeric |
      current == 52L
  )
  current[replace] <- returned[replace]
  status[rows] <- current
  has_detail <- replace & !is.na(result$details)
  details[rows[has_detail]] <- result$details[has_detail]
  list(status = status, details = details)
}

.evaluate_profile_native <- function(model, basis, dbh, ht, h, aux, rows) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  callback_name <- if (direct_dob) "dob" else "dib"
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(
      model, aux, rows, require = identical(basis, "dob") && !direct_dob
    )
    operation <- if (identical(basis, "dob")) 4L else 1L
    result <- .compiled_result(
      model, operation, dbh, ht, h, rep(0, length(h)), bark$value, aux, rows
    )
  } else {
    result <- .callback_result(
      model[[callback_name]],
      list(dbh = dbh, ht = ht, h = h, aux = .subset_aux(aux, rows, model)),
      length(h)
    )
  }
  if (identical(basis, "dob") && !direct_dob &&
        !identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = TRUE)
    missing <- bark$invalid
    result$value[!missing] <- result$value[!missing] / bark$value[!missing]
    result$value[missing] <- NA_real_
    result$status[missing] <- 53L
    result$details[missing] <- NA_character_
  }
  result
}

.prepare_profile_call <- function(dbh, ht, h, model, aux) {
  prepared <- .prepare_vectors(
    list(dbh = dbh, ht = ht, h = h, model = model),
    numeric_names = c("dbh", "ht", "h"),
    character_names = "model",
    aux = aux
  )
  values <- prepared$values
  status <- .input_status(
    prepared$size, values, c("dbh", "ht", "h", "model")
  )
  status <- .assign_status(status, values$dbh <= 0, 2L)
  status <- .assign_status(status, values$ht <= 0, 3L)
  status <- .assign_status(
    status, values$h < 0 | values$h > values$ht, 4L
  )
  resolved <- .resolve_models(values$model, prepared$aux, status)
  c(prepared, list(resolved = resolved))
}

.diameter_impl <- function(dbh, ht, h, model, aux, units, status, basis,
                           function_name) {
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_profile_call(dbh, ht, h, model, aux)
  call$aux <- .set_aux_caller_units(call$aux, units)
  output <- rep(NA_real_, call$size)
  result_status <- call$resolved$status
  details <- call$resolved$details
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
    native_dbh <- .diameter_to_native(call$values$dbh[rows], units, model_object$units)
    native_ht <- .height_to_native(call$values$ht[rows], units, model_object$units)
    native_h <- .height_to_native(call$values$h[rows], units, model_object$units)
    evaluated <- .evaluate_profile_native(
      model_object, basis, native_dbh, native_ht, native_h, call$aux, rows
    )
    merged <- .merge_kernel_result(result_status, details, rows, evaluated)
    result_status <- merged$status
    details <- merged$details
    output[rows] <- .diameter_from_native(
      evaluated$value, units, model_object$units
    )
  }
  output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
  .status_result(output, result_status, status_requested, details, function_name)
}

#' Calculate stem diameter inside bark at a height
#'
#' Return inside-bark diameter at each supplied stem height.
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
#' @param h Required finite numeric height above ground, from zero through
#'   `ht`, inclusive. Feet in imperial units or meters in metric units. Length one repeats,
#' otherwise it must match the tree inputs.
#'
#' Omission
#'   is an error. Missing or nonfinite values give `na_input`.
#'
#' @details The equation is evaluated without log-scaling rounding. For
#'   outside-bark diameter, a direct outside-bark equation takes precedence.
#'   Otherwise inside-bark diameter is divided by an available bark ratio,
#'   the constant ratio of inside-bark to outside-bark diameter.
#'
#' @return Numeric diameter in inches or centimeters, one per input tree.
#'   With `status = TRUE`, a data frame has numeric `value` in those units
#'   and integer unitless `status`. Failed values are `NA`.
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
#' \item{`h_out_of_range`}{A height is below ground or above total height. Check height
#'   units and bounds.}
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
#' }
#' Source equation errors retain their library diagnosis.
#'
#' Those values are unavailable. Check the selected equation and its inputs. Zero volume or
#' diameter can be a modeled boundary value.
#'
#' These functions
#' do not select logs, so a zero is not evidence of merchantable zero volume.
#' @seealso [height_at_dib()] to find height from diameter, [stem_volume()]
#'   for volume between heights, [get_taper_model()] for the equation inputs.
#' @export
#' @usage
#'
#' ## Call signatures
#' dib(dbh, ht, h, model, ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Estimate diameters at the supplied height
#' dib(dbh = example_trees$dbh,
#'     ht = example_trees$ht,
#'     h = 20,
#'     model = example_trees$model)
dib <- function(dbh, ht, h, model, ..., units = "imperial", status = FALSE) {
  .diameter_impl(
    dbh, ht, h, model, list(...), units, status, "dib", "dib"
  )
}

#' Calculate stem diameter outside bark at a height
#'
#' Return outside-bark diameter at each supplied stem height. The model must provide outside-
#' bark diameter or an applicable bark ratio.
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
#' @param h Required finite numeric height above ground, from zero through
#'   `ht`, inclusive. Feet in imperial units or meters in metric units. Length one repeats,
#' otherwise it must match the tree inputs.
#'
#' Omission
#'   is an error. Missing or nonfinite values give `na_input`.
#'
#' @details The equation is evaluated without log-scaling rounding. For
#'   outside-bark diameter, a direct outside-bark equation takes precedence.
#'   Otherwise inside-bark diameter is divided by an available bark ratio,
#'   the constant ratio of inside-bark to outside-bark diameter.
#'
#' @return Numeric diameter in inches or centimeters, one per input tree.
#'   With `status = TRUE`, a data frame has numeric `value` in those units
#'   and integer unitless `status`. Failed values are `NA`.
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
#' \item{`h_out_of_range`}{A height is below ground or above total height. Check height
#'   units and bounds.}
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
#' }
#' Source equation errors retain their library diagnosis.
#'
#' Those values are unavailable. Check the selected equation and its inputs. Zero volume or
#' diameter can be a modeled boundary value.
#'
#' These functions
#' do not select logs, so a zero is not evidence of merchantable zero volume.
#' @seealso [height_at_dob()] to find height from diameter, [stem_volume()]
#'   for volume between heights, [get_taper_model()] for the equation inputs.
#' @export
#' @usage
#'
#' ## Call signatures
#' dob(dbh, ht, h, model, ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Estimate diameters at the supplied height
#' dob(dbh = example_trees$dbh,
#'     ht = example_trees$ht,
#'     h = 20,
#'     model = example_trees$model)
dob <- function(dbh, ht, h, model, ..., units = "imperial", status = FALSE) {
  .diameter_impl(
    dbh, ht, h, model, list(...), units, status, "dob", "dob"
  )
}

.inverse_grid_step <- function(units) {
  one_inch <- if (identical(units, "imperial")) 1 / 12 else 0.0254
  one_inch / 16
}

.refine_crossing_brackets <- function(model, basis, dbh, ht, target, aux, rows,
                                      lower, upper, lower_value,
                                      max_iterations = 200L) {
  count <- length(lower)
  result <- rep(NA_real_, count)
  status <- integer(count)
  details <- rep(NA_character_, count)
  active <- rep(TRUE, count)
  for (iteration in seq_len(max_iterations)) {
    work <- which(active)
    if (!length(work)) {
      break
    }
    midpoint <- (lower[work] + upper[work]) / 2
    midpoint_result <- .evaluate_profile_native(
      model, basis, dbh[work], ht[work], midpoint, aux, rows[work]
    )
    bad <- midpoint_result$status != 0L
    if (any(bad)) {
      bad_rows <- work[bad]
      status[bad_rows] <- midpoint_result$status[bad]
      details[bad_rows] <- midpoint_result$details[bad]
      active[bad_rows] <- FALSE
    }
    good_rows <- work[!bad]
    if (length(good_rows)) {
      midpoint_good <- midpoint[!bad]
      value_good <- midpoint_result$value[!bad] - target[good_rows]
      exact <- value_good == 0
      if (any(exact)) {
        exact_rows <- good_rows[exact]
        lower[exact_rows] <- midpoint_good[exact]
        upper[exact_rows] <- midpoint_good[exact]
      }
      nonexact_rows <- good_rows[!exact]
      if (length(nonexact_rows)) {
        nonexact_values <- value_good[!exact]
        left <- lower_value[nonexact_rows] * nonexact_values <= 0
        upper[nonexact_rows[left]] <- midpoint_good[!exact][left]
        lower[nonexact_rows[!left]] <- midpoint_good[!exact][!left]
        lower_value[nonexact_rows[!left]] <- nonexact_values[!left]
      }
      now <- upper[good_rows] - lower[good_rows] <= 1e-4
      result[good_rows[now]] <- (upper[good_rows[now]] + lower[good_rows[now]]) / 2
      active[good_rows[now]] <- FALSE
    }
  }
  status[active] <- 103L
  list(value = result, status = status, details = details)
}

.numeric_inverse <- function(model, basis, dbh, ht, target, aux, rows,
                             max_iterations = 200L) {
  if (identical(basis, "dib") && identical(model$kernel$type, "compiled") &&
        startsWith(model$kernel$key, "smalltapers:")) {
    bark <- .model_bark_ratio(model, aux, rows, require = FALSE)$value
    return(.compiled_result(
      model, 5L, dbh, ht, target, rep(0, length(target)), bark, aux, rows
    ))
  }
  tree_count <- length(dbh)
  stump <- rep(model$stump_ht, tree_count)
  grid_step <- .inverse_grid_step(model$units)
  interval_count <- pmax(1L, as.integer(ceiling(pmax(ht - stump, 0) / grid_step)))
  grid_count <- interval_count + 1L
  map <- rep(seq_len(tree_count), grid_count)
  offset <- sequence(grid_count) - 1L
  grid_h <- stump[map] + offset * grid_step
  grid_end <- cumsum(grid_count)
  grid_start <- c(1L, utils::head(grid_end, -1L) + 1L)
  grid_h[grid_end] <- ht
  flat_rows <- rows[map]
  profile <- .evaluate_profile_native(
    model, basis, dbh[map], ht[map], grid_h, aux, flat_rows
  )
  result <- rep(NA_real_, tree_count)
  status <- integer(tree_count)
  details <- rep(NA_character_, tree_count)
  exact_roots <- vector("list", tree_count)
  bracket_tree <- bracket_lower <- bracket_upper <- bracket_value <- vector(
    "list", tree_count
  )

  for (tree in seq_len(tree_count)) {
    positions <- seq.int(grid_start[[tree]], grid_end[[tree]])
    tree_status <- profile$status[positions]
    if (any(tree_status != 0L)) {
      bad <- which(tree_status != 0L)[[1L]]
      status[[tree]] <- tree_status[[bad]]
      details[[tree]] <- profile$details[positions[[bad]]]
      next
    }
    difference <- profile$value[positions] - target[[tree]]
    exact <- which(difference == 0)
    changes <- if (length(difference) > 1L) {
      left <- utils::head(difference, -1L)
      right <- utils::tail(difference, -1L)
      which((left < 0 & right > 0) | (left > 0 & right < 0))
    } else {
      integer()
    }
    crossing_count <- length(exact) + length(changes)
    if (!crossing_count) {
      if (target[[tree]] < profile$value[positions[[length(positions)]]]) {
        status[[tree]] <- 100L
      } else if (target[[tree]] > profile$value[positions[[1L]]]) {
        status[[tree]] <- 101L
      } else {
        status[[tree]] <- 103L
      }
      next
    }
    exact_roots[[tree]] <- grid_h[positions[exact]]
    if (length(changes)) {
      bracket_tree[[tree]] <- rep(tree, length(changes))
      bracket_lower[[tree]] <- grid_h[positions[changes]]
      bracket_upper[[tree]] <- grid_h[positions[changes + 1L]]
      bracket_value[[tree]] <- difference[changes]
    }
    if (crossing_count > 1L) {
      status[[tree]] <- 102L
    }
  }

  bracket_tree <- unlist(bracket_tree, use.names = FALSE)
  if (length(bracket_tree)) {
    bracket_lower <- unlist(bracket_lower, use.names = FALSE)
    bracket_upper <- unlist(bracket_upper, use.names = FALSE)
    bracket_value <- unlist(bracket_value, use.names = FALSE)
    refined <- .refine_crossing_brackets(
      model, basis, dbh[bracket_tree], ht[bracket_tree], target[bracket_tree],
      aux, rows[bracket_tree], bracket_lower, bracket_upper, bracket_value,
      max_iterations
    )
    for (tree in unique(bracket_tree)) {
      selected <- which(bracket_tree == tree)
      failed <- selected[refined$status[selected] != 0L]
      if (length(failed)) {
        first <- failed[[1L]]
        status[[tree]] <- refined$status[[first]]
        details[[tree]] <- refined$details[[first]]
        result[[tree]] <- NA_real_
      } else {
        roots <- c(exact_roots[[tree]], refined$value[selected])
        result[[tree]] <- max(roots)
      }
    }
  }
  exact_only <- which(lengths(exact_roots) > 0L &
                        !seq_len(tree_count) %in% bracket_tree)
  for (tree in exact_only) {
    result[[tree]] <- max(exact_roots[[tree]])
  }
  list(value = result, status = status, details = details)
}

.analytic_inverse <- function(model, dbh, ht, target, aux, rows) {
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = FALSE)$value
    return(.compiled_result(
      model, 2L, dbh, ht, target, rep(0, length(target)), bark, aux, rows
    ))
  }
  .callback_result(
    model$height_at_dib,
    list(dbh = dbh, ht = ht, dib = target, aux = .subset_aux(aux, rows, model)),
    length(target)
  )
}

.analytic_discovered_inverse <- function(model, dbh, ht, target, aux, rows) {
  analytic <- .analytic_inverse(model, dbh, ht, target, aux, rows)
  discovered <- .numeric_inverse(model, "dib", dbh, ht, target, aux, rows)
  use_discovery <- analytic$status %in% c(0L, 52L, 102L) &
    discovered$status != 0L
  analytic$status[use_discovery] <- discovered$status[use_discovery]
  analytic$details[use_discovery] <- discovered$details[use_discovery]
  usable <- analytic$status %in% c(0L, 52L, 102L) &
    discovered$status %in% c(0L, 102L)
  analytic$value[!usable] <- NA_real_
  analytic
}

.inverse_group <- function(model, basis, dbh, ht, target, aux, rows) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(
      model, aux, rows, require = identical(basis, "dob") && !direct_dob
    )$value
    return(.compiled_result(
      model, if (identical(basis, "dib")) 6L else 7L,
      dbh, ht, target, rep(model$stump_ht, length(target)), bark, aux, rows
    ))
  }
  if (identical(basis, "dob") && !direct_dob) {
    bark <- .model_bark_ratio(model, aux, rows, require = TRUE)
    if (any(bark$invalid)) {
      result <- list(
        value = rep(NA_real_, length(target)),
        status = ifelse(bark$invalid, 53L, 0L),
        details = rep(NA_character_, length(target))
      )
      good <- which(!bark$invalid)
      if (!length(good)) {
        return(result)
      }
      inside <- if (isTRUE(model$kernel$has_inverse)) {
        .analytic_discovered_inverse(
          model, dbh[good], ht[good], target[good] * bark$value[good], aux, rows[good]
        )
      } else {
        .numeric_inverse(
          model, "dib", dbh[good], ht[good], target[good] * bark$value[good],
          aux, rows[good]
        )
      }
      result$value[good] <- inside$value
      result$status[good] <- inside$status
      result$details[good] <- inside$details
      return(result)
    }
    target <- target * bark$value
    basis <- "dib"
  }
  if (identical(basis, "dib") && isTRUE(model$kernel$has_inverse)) {
    return(.analytic_discovered_inverse(model, dbh, ht, target, aux, rows))
  }
  .numeric_inverse(model, basis, dbh, ht, target, aux, rows)
}

.height_impl <- function(dbh, ht, diameter, model, aux, units, status, basis,
                         function_name) {
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_vectors(
    list(dbh = dbh, ht = ht, diameter = diameter, model = model),
    numeric_names = c("dbh", "ht", "diameter"),
    character_names = "model",
    aux = aux
  )
  result_status <- .input_status(
    call$size, call$values, c("dbh", "ht", "diameter", "model")
  )
  result_status <- .assign_status(result_status, call$values$dbh <= 0, 2L)
  result_status <- .assign_status(result_status, call$values$ht <= 0, 3L)
  result_status <- .assign_status(result_status, call$values$diameter <= 0, 5L)
  resolved <- .resolve_models(call$values$model, call$aux, result_status)
  call$aux <- .set_aux_caller_units(call$aux, units)
  result_status <- resolved$status
  details <- resolved$details
  output <- rep(NA_real_, call$size)
  for (group_index in seq_along(resolved$dictionary)) {
    model_object <- resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    group_rows <- resolved$groups[[as.character(group_index)]]
    rows <- group_rows[result_status[group_rows] %in% c(0L, 52L)]
    if (!length(rows)) {
      next
    }
    native_dbh <- .diameter_to_native(call$values$dbh[rows], units, model_object$units)
    native_ht <- .height_to_native(call$values$ht[rows], units, model_object$units)
    native_target <- .diameter_to_native(
      call$values$diameter[rows], units, model_object$units
    )
    inverted <- .inverse_group(
      model_object, basis, native_dbh, native_ht, native_target, call$aux, rows
    )
    merged <- .merge_kernel_result(result_status, details, rows, inverted)
    result_status <- merged$status
    details <- merged$details
    output[rows] <- .height_from_native(
      inverted$value, units, model_object$units
    )
  }
  output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
  .status_result(output, result_status, status_requested, details, function_name)
}

#' Find the highest height at a diameter inside bark
#'
#' Return the highest modeled height at the supplied inside-bark diameter. Search from the
#' model's stump height to the tip.
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
#' @param dib Required positive finite numeric target diameter, in inches
#'   or centimeters. Length one repeats, otherwise it must match the trees. Omission is an error.
#'
#' Missing gives `na_input`, nonpositive values give
#'   `diameter_nonpositive`. Zero is not a request for the tip.
#'
#' @details A profile can cross the same diameter at several heights. The
#'   highest crossing is retained and `not_unique` identifies another crossing. Outside-bark
#' targets use a direct profile where available.
#'
#' Otherwise
#'   the bark ratio converts the target to its inside-bark counterpart.
#' @section Numerical methods:
#' The search checks a grid spaced at one-sixteenth inch in height and
#' refines crossing intervals by bisection. It uses the equation's direct
#' inverse where supported. Compatibility settings and regional equations
#' can retain source-specific search behavior.
#'
#' No log-length rounding is
#' applied to the returned height.
#' @return Numeric height above ground in feet or meters, one per input tree.
#'   With `status = TRUE`, the data frame contains numeric `value` in those
#'   units and integer unitless `status`. Failed heights are `NA`.
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
#' \item{`diameter_nonpositive`}{The target diameter is not positive. Supply a positive
#'   target or use the tip bound for volume.}
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
#' @seealso [dib()] for diameter at height, [stem_profile()] to inspect
#'   crossings, [stem_volume()] to use diameter limits directly.
#' @export
#' @usage
#'
#' ## Call signatures
#' height_at_dib(dbh, ht, dib, model, ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Find the height of the supplied stem diameter
#' height_at_dib(dbh = example_trees$dbh,
#'               ht = example_trees$ht,
#'               dib = 6,
#'               model = example_trees$model)
height_at_dib <- function(dbh, ht, dib, model, ..., units = "imperial",
                          status = FALSE) {
  .height_impl(
    dbh, ht, dib, model, list(...), units, status, "dib", "height_at_dib"
  )
}

#' Find the highest height at a diameter outside bark
#'
#' Return the highest modeled height at the supplied outside-bark diameter. Search from the
#' model's stump height to the tip.
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
#' @param dob Required positive finite numeric target diameter, in inches
#'   or centimeters. Length one repeats, otherwise it must match the trees. Omission is an error.
#'
#' Missing gives `na_input`, nonpositive values give
#'   `diameter_nonpositive`. Zero is not a request for the tip.
#'
#' @details A profile can cross the same diameter at several heights. The
#'   highest crossing is retained and `not_unique` identifies another crossing. Outside-bark
#' targets use a direct profile where available.
#'
#' Otherwise
#'   the bark ratio converts the target to its inside-bark counterpart.
#' @section Numerical methods:
#' The search checks a grid spaced at one-sixteenth inch in height and
#' refines crossing intervals by bisection. It uses the equation's direct
#' inverse where supported. Compatibility settings and regional equations
#' can retain source-specific search behavior.
#'
#' No log-length rounding is
#' applied to the returned height.
#' @return Numeric height above ground in feet or meters, one per input tree.
#'   With `status = TRUE`, the data frame contains numeric `value` in those
#'   units and integer unitless `status`. Failed heights are `NA`.
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
#' \item{`diameter_nonpositive`}{The target diameter is not positive. Supply a positive
#'   target or use the tip bound for volume.}
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
#' @seealso [dob()] for diameter at height, [stem_profile()] to inspect
#'   crossings, [stem_volume()] to use diameter limits directly.
#' @export
#' @usage
#'
#' ## Call signatures
#' height_at_dob(dbh, ht, dob, model, ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Find the height of the supplied stem diameter
#' height_at_dob(dbh = example_trees$dbh,
#'               ht = example_trees$ht,
#'               dob = 6,
#'               model = example_trees$model)
height_at_dob <- function(dbh, ht, dob, model, ..., units = "imperial",
                          status = FALSE) {
  .height_impl(
    dbh, ht, dob, model, list(...), units, status, "dob", "height_at_dob"
  )
}
