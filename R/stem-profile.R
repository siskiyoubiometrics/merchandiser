.profile_common_inputs <- function(dbh, ht, model, step, id, lower, lower_type,
                                   upper, upper_type, stump_ht, aux) {
  aux <- .capture_aux(aux)
  values <- list(
    dbh = dbh, ht = ht, model = model, lower = lower,
    lower_type = lower_type, upper = upper, upper_type = upper_type
  )
  if (!is.null(step)) {
    values$step <- step
  }
  if (!is.null(id)) {
    values$id <- id
  }
  if (!is.null(stump_ht)) {
    values$stump_ht <- stump_ht
  }
  size <- .common_size(c(values, aux))
  values <- Map(
    function(value, name) {
      if (identical(name, "model") &&
            (length(value) == 1L || is.factor(value))) {
        return(value)
      }
      .recycle_common(value, size, name)
    },
    values, names(values)
  )
  aux <- Map(
    function(value, name) .recycle_common(value, size, name),
    aux, names(aux)
  )
  if (is.null(step)) {
    values$step <- NULL
  }
  if (is.null(id)) {
    values$id <- seq_len(size)
  }
  list(size = size, values = values, aux = aux)
}

.empty_profile <- function(id) {
  data.frame(
    id = id,
    h = numeric(length(id)),
    dib = numeric(length(id)),
    dob = numeric(length(id)),
    cum_volume_ib = numeric(length(id)),
    cum_volume_ob = numeric(length(id)),
    status = integer(length(id)),
    stringsAsFactors = FALSE
  )
}

.compiled_profile_result <- function(model, tree, height, initial_status,
                                     dbh, ht, lower, aux, rows) {
  size <- length(rows)
  native_aux <- .subset_aux(aux, rows, model)
  bark <- .model_bark_ratio(
    model, aux, rows, require = !isTRUE(model$kernel$has_dob)
  )$value
  if (startsWith(model$kernel$key, "flewelling:") &&
        is.null(native_aux$bark_ratio)) {
    bark[] <- 0
  }
  upper_bark <- if (is.null(native_aux$upper_bark)) {
    integer(size)
  } else {
    match(native_aux$upper_bark, c("ob", "ib"), nomatch = 0L)
  }
  tv_cpp_profile_eval_impl(
    model$kernel$key, as.integer(tree), height, as.integer(initial_status),
    dbh, ht, lower, bark,
    .compiled_auxiliary(native_aux, "upper_ht1", size),
    .compiled_auxiliary(native_aux, "upper_d1", size),
    .compiled_auxiliary(native_aux, "upper_ht2", size),
    .compiled_auxiliary(native_aux, "upper_d2", size),
    as.integer(upper_bark),
    .compiled_auxiliary(native_aux, "site_index", size),
    .compiled_auxiliary(native_aux, "basal_area", size),
    .compiled_auxiliary(native_aux, "form_class", size),
    as.double(model$data),
    identical(.treevolume_compat(), "nvel"),
    threads()
  )
}

#' Calculate stem diameters and volume at a series of heights
#'
#' Materialize diameters and cumulative volumes at a sequence of stem heights. Return a profile
#' data frame with one row per tree and height.
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
#' @param status One nonmissing logical value, default `FALSE`. The table
#'   always has a status column. `TRUE` suppresses row warnings.
#'
#' Invalid types
#'   or missing flags stop the call. Unitless. Example: `status = TRUE`.
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
#' @param step Positive finite numeric spacing, length one or one per tree. It uses inches for
#'   imperial calls and centimeters for metric calls. Default `NULL` selects 1 inch or 2.54
#'   centimeters.
#'
#' Missing, nonfinite,
#'   and nonpositive values are errors.
#' @param id Atomic identifiers, length one or one per tree. Default `NULL`
#'   assigns row numbers. Missing values are errors.
#'
#' Unitless. Example:
#'   `id = example_trees$tree`. Duplicate identifiers are accepted but then
#'   several trees share a label, so use distinct identifiers for plotting.
#'
#' @details Heights begin at the resolved lower limit, advance by `step`,
#'   and include the upper limit even when the last spacing is shorter. Cumulative volume starts at
#' the lower limit, not necessarily ground. The lower limit's cumulative value is zero for an
#' available volume.
#'
#' Rows are ordered by identifier, then height. Diameter and volume values
#'   are not rounded to log-scaling increments.
#' @return A `stem_profile` data frame with a `units` attribute and the fields. The table always
#' includes `status`. `status = TRUE` suppresses row warnings, and
#' `FALSE` is the default.
#' @section Profile table fields:
#' \describe{
#' \item{`id`}{Input atomic tree label, unitless, for example `'example-1'`.}
#' \item{`h`}{Numeric height above ground in feet or meters. Failed trees
#' can have a missing height. }
#' \item{`dib`, `dob`}{Numeric inside-bark and outside-bark diameters,
#' respectively, in inches or centimeters. Unavailable values are missing.}
#' \item{`cum_volume_ib`, `cum_volume_ob`}{Numeric cumulative inside-bark
#' and outside-bark volume from the lower limit, in cubic feet or cubic
#' meters. The first available value is zero. Missing values are unavailable.}
#' \item{`status`}{Integer unitless calculation code. Zero means completion.
#' A failed tree retains a diagnostic row. Check quantity availability as
#' well as this code because outside-bark failure can coexist with a diameter.}
#' }
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
#' @section Numerical methods:
#' Diameters and volumes use the selected equation's calculations. The
#' number of heights is `ceiling((upper - lower) / spacing) + 1`, with all
#' three quantities expressed in the same height unit. The spacing argument
#' must first be converted from inches to feet or centimeters to meters.
#' @seealso [plot.stem_profile()] to draw the table, [dib()] for one diameter,
#'   [stem_volume()] for one interval volume.
#' @export
#' @usage
#'
#' ## Call signatures
#' stem_profile(dbh, ht, model, step = NULL, id = NULL, lower = 0, lower_type = 'stump', upper =
#'   0, upper_type = 'tip', ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Calculate profiles for the example trees
#' profile <- stem_profile(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         model = example_trees$model,
#'                         id = example_trees$tree)
#'
#' ## Draw the selected tree
#' plot(profile, tree = 5)
stem_profile <- function(dbh, ht, model, step = NULL, id = NULL, lower = 0,
                         lower_type = "stump", upper = 0, upper_type = "tip",
                         ..., units = "imperial", status = FALSE) {
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  common <- .profile_common_inputs(
    dbh, ht, model, step, id, lower, lower_type, upper, upper_type,
    NULL, list(...)
  )
  if (!common$size) {
    return(structure(.empty_profile(common$values$id),
                     class = c("stem_profile", "data.frame"), units = units))
  }
  actual_step <- if (is.null(common$values$step)) {
    rep(if (identical(units, "imperial")) 1 else 2.54, common$size)
  } else {
    common$values$step
  }
  if (!is.numeric(actual_step) || any(!is.finite(actual_step)) || any(actual_step <= 0)) {
    stop("step must contain finite positive numbers.", call. = FALSE)
  }
  if (!is.atomic(common$values$id) || anyNA(common$values$id)) {
    stop("id must be an atomic vector without missing values.", call. = FALSE)
  }

  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_volume_call(
    common$values$dbh, common$values$ht, common$values$model,
    common$values$lower, common$values$lower_type, common$values$upper,
    common$values$upper_type, common$values$stump_ht, common$aux
  )
  call$aux <- .set_aux_caller_units(call$aux, units)
  tree_status <- call$resolved$status
  tree_details <- call$resolved$details
  tree_group <- integer(common$size)
  native_dbh <- native_ht <- native_lower <- native_upper <- rep(NA_real_, common$size)
  native_step <- rep(NA_real_, common$size)

  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    group_rows <- call$resolved$groups[[as.character(group_index)]]
    rows <- group_rows[tree_status[group_rows] %in% c(0L, 52L)]
    if (!length(rows)) {
      next
    }
    bounds <- .resolve_group_bounds(call, model_object, rows, units)
    lower_merged <- .merge_kernel_result(tree_status, tree_details, rows, bounds$lower)
    tree_status <- lower_merged$status
    tree_details <- lower_merged$details
    upper_merged <- .merge_kernel_result(tree_status, tree_details, rows, bounds$upper)
    tree_status <- upper_merged$status
    tree_details <- upper_merged$details
    empty <- is.finite(bounds$lower$value) & is.finite(bounds$upper$value) &
      bounds$lower$value >= bounds$upper$value
    tree_status[rows[empty]] <- 6L
    tree_group[rows] <- group_index
    native_dbh[rows] <- bounds$dbh
    native_ht[rows] <- bounds$ht
    native_lower[rows] <- bounds$lower$value
    native_upper[rows] <- bounds$upper$value
    native_step[rows] <- .step_to_native_height(
      actual_step[rows], units, model_object$units
    )
  }

  grid <- as.data.frame(tv_cpp_profile_grid_impl(
    native_lower, native_upper, native_step, as.integer(tree_status), threads()
  ), stringsAsFactors = FALSE)
  grid$dib <- grid$dob <- grid$cum_volume_ib <- grid$cum_volume_ob <- NA_real_
  grid_details <- tree_details[grid$tree]

  valid_grid <- which(grid$status %in% c(0L, 52L, 102L))
  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    selected <- valid_grid[
      tree_group[grid$tree[valid_grid]] == group_index
    ]
    if (!length(selected)) {
      next
    }
    trees <- grid$tree[selected]
    if (identical(model_object$kernel$type, "compiled")) {
      group_rows <- call$resolved$groups[[as.character(group_index)]]
      evaluated <- .compiled_profile_result(
        model_object, match(trees, group_rows), grid$native_h[selected],
        grid$status[selected], native_dbh[group_rows], native_ht[group_rows],
        native_lower[group_rows], call$aux, group_rows
      )
      grid$dib[selected] <- .diameter_from_native(
        evaluated$dib, units, model_object$units
      )
      grid$dob[selected] <- .diameter_from_native(
        evaluated$dob, units, model_object$units
      )
      grid$cum_volume_ib[selected] <- .volume_from_native(
        evaluated$cum_volume_ib, units, model_object$units
      )
      grid$cum_volume_ob[selected] <- .volume_from_native(
        evaluated$cum_volume_ob, units, model_object$units
      )
      grid$status[selected] <- evaluated$status
      failed <- selected[evaluated$status == 54L]
      grid_details[failed] <- "compiled kernel returned a non-finite value"
      next
    }
    inside <- .evaluate_profile_native(
      model_object, "dib", native_dbh[trees], native_ht[trees],
      grid$native_h[selected], call$aux, trees
    )
    outside <- .evaluate_profile_native(
      model_object, "dob", native_dbh[trees], native_ht[trees],
      grid$native_h[selected], call$aux, trees
    )
    inside_merged <- .merge_kernel_result(grid$status, grid_details, selected, inside)
    grid$status <- inside_merged$status
    grid_details <- inside_merged$details
    outside_merged <- .merge_kernel_result(grid$status, grid_details, selected, outside)
    grid$status <- outside_merged$status
    grid_details <- outside_merged$details
    grid$dib[selected] <- .diameter_from_native(
      inside$value, units, model_object$units
    )
    grid$dob[selected] <- .diameter_from_native(
      outside$value, units, model_object$units
    )

    at_lower <- grid$native_h[selected] == native_lower[trees] &
      grid$status[selected] %in% c(0L, 52L, 102L)
    grid$cum_volume_ib[selected[at_lower]] <- 0
    grid$cum_volume_ob[selected[at_lower]] <- 0
    integrate_selected <- selected[
      !at_lower & grid$status[selected] %in% c(0L, 52L, 102L)
    ]
    if (length(integrate_selected)) {
      integrate_trees <- grid$tree[integrate_selected]
      inside_volume <- .integrate_group(
        model_object, "dib", native_dbh[integrate_trees], native_ht[integrate_trees],
        native_lower[integrate_trees], grid$native_h[integrate_selected],
        call$aux, integrate_trees
      )
      outside_volume <- .integrate_group(
        model_object, "dob", native_dbh[integrate_trees], native_ht[integrate_trees],
        native_lower[integrate_trees], grid$native_h[integrate_selected],
        call$aux, integrate_trees
      )
      volume_merged <- .merge_kernel_result(
        grid$status, grid_details, integrate_selected, inside_volume
      )
      grid$status <- volume_merged$status
      grid_details <- volume_merged$details
      outside_volume_merged <- .merge_kernel_result(
        grid$status, grid_details, integrate_selected, outside_volume
      )
      grid$status <- outside_volume_merged$status
      grid_details <- outside_volume_merged$details
      grid$cum_volume_ib[integrate_selected] <- .volume_from_native(
        inside_volume$value, units, model_object$units
      )
      grid$cum_volume_ob[integrate_selected] <- .volume_from_native(
        outside_volume$value, units, model_object$units
      )
    }
  }

  result <- data.frame(
    id = common$values$id[grid$tree],
    h = rep(NA_real_, nrow(grid)),
    dib = grid$dib,
    dob = grid$dob,
    cum_volume_ib = grid$cum_volume_ib,
    cum_volume_ob = grid$cum_volume_ob,
    status = as.integer(grid$status),
    stringsAsFactors = FALSE
  )
  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    selected <- tree_group[grid$tree] == group_index & is.finite(grid$native_h)
    result$h[selected] <- .height_from_native(
      grid$native_h[selected], units, model_object$units
    )
  }
  ordering <- order(result$id, result$h, na.last = TRUE)
  result <- result[ordering, , drop = FALSE]
  rownames(result) <- NULL
  if (!status_requested) {
    .status_warning(
      "stem_profile", grid$status, grid_details, common$size, tree = grid$tree
    )
  }
  structure(result, class = c("stem_profile", "data.frame"), units = units)
}
