.profile_common_inputs <- function(
  dbh, ht, model, step, id, lower, lower_type,
  upper, upper_type,
  stump_ht, aux
) {
  aux <- .capture_aux(aux)
  values <- list(
    dbh = dbh, ht = ht, model = model, lower = lower, lower_type = lower_type,
    upper = upper, upper_type = upper_type
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
  size <- .common_size(c(values, aux), c(
    id = "tree_id", lower = .bound_argument_name("from", lower_type),
    upper = .bound_argument_name("to", upper_type)
  ))
  values <- Map(function(value, name) {
    if (identical(name, "model") && (length(value) == 1L || is.factor(value))) {
      return(value)
    }
    .recycle_common(value, size, name)
  }, values, names(values))
  aux <- Map(function(value, name) .recycle_common(value, size, name), aux, names(aux))
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
    tree_id = id, h = numeric(length(id)), dib = numeric(length(id)),
    dob = numeric(length(id)),
    cum_volume_ib = numeric(length(id)), cum_volume_ob = numeric(length(id)),
    status = integer(length(id)),
    stringsAsFactors = FALSE
  )
}

.compiled_profile_result <- function(
  model, tree, height, initial_status, dbh, ht,
  lower, aux,
  rows
) {
  size <- length(rows)
  native_aux <- .subset_aux(aux, rows, model)
  bark <- .model_bark_ratio(model, aux, rows, require = !isTRUE(model$kernel$has_dob))$value
  if (startsWith(model$kernel$key, "flewelling:") && is.null(native_aux$bark_ratio)) {
    bark[] <- 0
  }
  upper_bark <- if (is.null(native_aux$upper_bark)) {
    integer(size)
  } else {
    match(native_aux$upper_bark, c("ob", "ib"), nomatch = 0L)
  }
  tv_cpp_profile_eval_impl(
    model$kernel$key, as.integer(tree), height, as.integer(
      initial_status
    ),
    dbh, ht, lower, bark, .compiled_auxiliary(native_aux, "upper_ht1", size),
    .compiled_auxiliary(
      native_aux,
      "upper_d1", size
    ), .compiled_auxiliary(native_aux, "upper_ht2", size),
    .compiled_auxiliary(
      native_aux,
      "upper_d2", size
    ), as.integer(upper_bark), .compiled_auxiliary(
      native_aux,
      "site_index",
      size
    ), .compiled_auxiliary(native_aux, "basal_area", size),
    .compiled_auxiliary(
      native_aux,
      "form_class", size
    ), as.double(model$data), identical(.treevolume_compat(), "nvel"),
    threads()
  )
}

#' Sample diameters and cumulative volumes along stems
#'
#' Evaluate a taper equation at regular height intervals, including the final bound. Invalid
#'   trees retain a row with missing measurements and a status. The plot method distinguishes
#'   inside and outside bark profiles.
#'
#' @param tree_id Unique, nonmissing identifiers for the input trees, as an atomic vector.
#'   Required, without a default. Repeated output rows retain these identifiers.
#' @param dbh Outside bark diameter at breast height, in inches. Accepts numeric values greater
#'   than zero and no greater than 400. Required, with no default. Invalid measurement rows
#'   return missing results with a status.
#' @param ht Total height above ground, in feet. Accepts numeric values greater than zero and no
#'   greater than 500. Required, with no default. Measurement heights and section bounds must
#'   fall within the tree.
#' @param spcd Species identifier as a numeric vector of positive whole-number codes. Required,
#'   with no default. The species must be recognized for species properties and within the
#'   selected equation's scope.
#' @param model Taper equation identifier as a character vector. Defaults to `NULL`, selecting
#'   the stored species default. A supplied identifier overrides that choice. Scalar identifiers
#'   recycle across trees.
#' @param step Spacing between profile measurements, in feet. Accepts finite numeric values of at
#'   least 0.05, either scalar or one per tree. Defaults to `0.5`. The exact upper bound is
#'   included even when it does not fall on the spacing.
#' @param from Lower section height above ground, in feet. Accepts numeric values from zero
#'   through total height. Defaults to `NULL`, using `stump_ht`. Supply at most one of `from`,
#'   `from_dib`, and `from_dob`.
#' @param to Upper section height above ground, in feet. Accepts numeric values from zero through
#'   total height. Defaults to `NULL`, using the tip. Supply at most one of `to`, `to_dib`, and
#'   `to_dob`. The resulting upper bound must exceed the lower bound.
#' @param from_dib Lower section boundary specified by inside bark diameter, in inches. Accepts
#'   positive numeric values. Defaults to `NULL`, leaving this diameter bound unset. It locates
#'   the boundary independently of the volume bark basis and cannot accompany another lower
#'   bound.
#' @param from_dob Lower section boundary specified by outside bark diameter, in inches. Accepts
#'   positive numeric values. Defaults to `NULL`, leaving this diameter bound unset. It locates
#'   the boundary independently of the volume bark basis and cannot accompany another lower
#'   bound.
#' @param to_dib Upper section boundary specified by inside bark diameter, in inches. Accepts
#'   positive numeric values. Defaults to `NULL`, leaving this diameter bound unset. It locates
#'   the boundary independently of the volume bark basis and cannot accompany another upper
#'   bound.
#' @param to_dob Upper section boundary specified by outside bark diameter, in inches. Accepts
#'   positive numeric values. Defaults to `NULL`, leaving this diameter bound unset. It locates
#'   the boundary independently of the volume bark basis and cannot accompany another upper
#'   bound.
#' @param stump_ht Stump height above ground, in feet. Accepts finite, nonnegative numeric values
#'   below total height. Defaults to `1`. Used as the lower section bound unless another bound is
#'   supplied.
#' @param ... Additional named inputs accepted by the selected model, with none supplied by
#'   default. Numeric inputs must be finite: positive `upper_ht1`, `upper_ht2`, and `site_index`
#'   use feet, positive `upper_d1` and `upper_d2` use inches, and positive `basal_area` uses
#'   square feet per acre. `form_class` accepts positive numbers. `bark_ratio` is inside diameter
#'   divided by outside diameter, greater than zero and no greater than one. `decay_class`
#'   accepts whole numbers from 1 through 5 and `cull` accepts percentages from 0 through 100.
#'   `upper_bark` accepts `'ib'` or `'ob'`. Upper heights and diameters must be supplied in
#'   pairs. Only inputs declared by the selected model are accepted.
#' @return A `stem_profile` data frame with `tree_id` (identifier), `h` (feet above ground),
#'   `dib` and `dob` (inches), `cum_volume_ib` and `cum_volume_ob` (cubic feet above the lower
#'   bound), and `status` (integer code). Trees remain in input order, with ascending heights
#'   within each tree.
#' @usage
#' stem_profile(
#'   tree_id,
#'   dbh,
#'   ht,
#'   spcd,
#'   model = NULL,
#'   step = 0.5,
#'   from = NULL,
#'   to = NULL,
#'   from_dib = NULL,
#'   from_dob = NULL,
#'   to_dib = NULL,
#'   to_dob = NULL,
#'   stump_ht = 1,
#'   ...
#' )
#' @export
#' @examples
#' ## Sample the first example tree
#' profile <- stem_profile(tree_id = example_trees$tree_id[1],
#'                         dbh = example_trees$dbh[1],
#'                         ht = example_trees$ht[1],
#'                         spcd = example_trees$spcd[1],
#'                         model = example_trees$model[1],
#'                         step = 5)
#'
#' ## Draw the sampled profile
#' plot(profile)
stem_profile <- function(
  tree_id,
  dbh,
  ht,
  spcd,
  model = NULL,
  step = 0.5,
  from = NULL,
  to = NULL,
  from_dib = NULL,
  from_dob = NULL,
  to_dib = NULL,
  to_dob = NULL,
  stump_ht = 1,
  ...
) {
  supplied <- list(dbh = dbh, ht = ht, step = step)
  if (!is.null(model))
    supplied$model <- model
  for (name in names(supplied)) {
    if (!length(supplied[[name]]))
      stop(name, " must not be empty.", call. = FALSE)
  }
  if (!is.numeric(stump_ht))
    stop("stump_ht must be numeric.", call. = FALSE)
  bounds <- .stem_section_bounds(from, to, from_dib, from_dob, to_dib, to_dob)
  measurement_system <- "imperial"
  status_requested <- TRUE
  input <- .mc_stem_inputs(spcd, model, list(...), TRUE)
  model <- input$model
  .mc_require_atomic_id(tree_id, "tree_id")
  common <- .profile_common_inputs(
    dbh, ht, model, step, tree_id, bounds$lower, bounds$lower_type, bounds$upper,
    bounds$upper_type, stump_ht, input$aux
  )
  if (anyDuplicated(common$values$id))
    stop("tree_id must be unique.", call. = FALSE)
  if (!common$size) {
    return(structure(.empty_profile(common$values$id),
      class = c(
        "stem_profile",
        "data.frame"
      ),
      measurement_system = measurement_system
    ))
  }
  actual_step <- common$values$step
  if (!is.numeric(actual_step) || any(!is.finite(actual_step)) || any(actual_step < 0.05)) {
    stop("step must contain finite numbers of at least 0.05 feet.", call. = FALSE)
  }
  if (!is.atomic(common$values$id) || anyNA(common$values$id)) {
    stop("tree_id must be an atomic vector without missing values.", call. = FALSE)
  }

  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_volume_call(
    common$values$dbh, common$values$ht,
    common$values$model, common$values$lower,
    common$values$lower_type, common$values$upper, common$values$upper_type,
    common$values$stump_ht,
    common$aux
  )
  call$aux <- .set_aux_caller_units(call$aux, measurement_system)
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
    bounds <- .resolve_group_bounds(call, model_object, rows, measurement_system)
    lower_merged <- .merge_kernel_result(tree_status, tree_details, rows, bounds$lower)
    tree_status <- lower_merged$status
    tree_details <- lower_merged$details
    upper_merged <- .merge_kernel_result(tree_status, tree_details, rows, bounds$upper)
    tree_status <- upper_merged$status
    tree_details <- upper_merged$details
    empty <- is.finite(bounds$lower$value) & is.finite(bounds$upper$value) &
      bounds$lower$value >=
        bounds$upper$value
    tree_status[rows[empty]] <- 6L
    tree_group[rows] <- group_index
    native_dbh[rows] <- bounds$dbh
    native_ht[rows] <- bounds$ht
    native_lower[rows] <- bounds$lower$value
    native_upper[rows] <- bounds$upper$value
    native_step[rows] <- .step_to_native_height(
      actual_step[rows], measurement_system,
      model_object$measurement_system
    )
  }

  grid <- as.data.frame(tv_cpp_profile_grid_impl(
    native_lower, native_upper,
    native_step, as.integer(tree_status),
    threads()
  ), stringsAsFactors = FALSE)
  grid$dib <- grid$dob <- grid$cum_volume_ib <- grid$cum_volume_ob <- NA_real_
  grid_details <- tree_details[grid$tree]

  valid_grid <- which(grid$status %in% c(0L, 52L, 102L))
  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    selected <- valid_grid[tree_group[grid$tree[valid_grid]] == group_index]
    if (!length(selected)) {
      next
    }
    trees <- grid$tree[selected]
    if (identical(model_object$kernel$type, "compiled")) {
      group_rows <- call$resolved$groups[[as.character(group_index)]]
      evaluated <- .compiled_profile_result(
        model_object, match(trees, group_rows),
        grid$native_h[selected],
        grid$status[selected], native_dbh[group_rows], native_ht[group_rows],
        native_lower[group_rows],
        call$aux, group_rows
      )
      grid$dib[
        selected
      ] <-
        .diameter_from_native(
          evaluated$dib, measurement_system, model_object$measurement_system
        )
      grid$dob[
        selected
      ] <-
        .diameter_from_native(
          evaluated$dob, measurement_system, model_object$measurement_system
        )
      grid$cum_volume_ib[selected] <- .volume_from_native(
        evaluated$cum_volume_ib, measurement_system,
        model_object$measurement_system
      )
      grid$cum_volume_ob[selected] <- .volume_from_native(
        evaluated$cum_volume_ob, measurement_system,
        model_object$measurement_system
      )
      grid$status[selected] <- evaluated$status
      failed <- selected[evaluated$status == 54L]
      grid_details[failed] <- "compiled kernel returned a non-finite value"
      next
    }
    inside <- .evaluate_profile_native(
      model_object, "dib", native_dbh[trees],
      native_ht[trees],
      grid$native_h[selected], call$aux, trees
    )
    outside <- .evaluate_profile_native(
      model_object, "dob", native_dbh[trees],
      native_ht[trees],
      grid$native_h[selected], call$aux, trees
    )
    inside_merged <- .merge_kernel_result(grid$status, grid_details, selected, inside)
    grid$status <- inside_merged$status
    grid_details <- inside_merged$details
    outside_merged <- .merge_kernel_result(grid$status, grid_details, selected, outside)
    grid$status <- outside_merged$status
    grid_details <- outside_merged$details
    grid$dib[
      selected
    ] <-
      .diameter_from_native(inside$value, measurement_system, model_object$measurement_system)
    grid$dob[
      selected
    ] <-
      .diameter_from_native(
        outside$value, measurement_system, model_object$measurement_system
      )

    at_lower <- grid$native_h[selected] == native_lower[trees] & grid$status[selected] %in%
      c(0L, 52L, 102L)
    grid$cum_volume_ib[selected[at_lower]] <- 0
    grid$cum_volume_ob[selected[at_lower]] <- 0
    integrate_selected <- selected[!at_lower & grid$status[selected] %in% c(0L, 52L, 102L)]
    if (length(integrate_selected)) {
      integrate_trees <- grid$tree[integrate_selected]
      inside_volume <- .integrate_group(
        model_object, "dib", native_dbh[integrate_trees],
        native_ht[integrate_trees], native_lower[integrate_trees],
        grid$native_h[integrate_selected],
        call$aux, integrate_trees
      )
      outside_volume <- .integrate_group(
        model_object, "dob", native_dbh[integrate_trees],
        native_ht[integrate_trees], native_lower[integrate_trees],
        grid$native_h[integrate_selected],
        call$aux, integrate_trees
      )
      volume_merged <- .merge_kernel_result(
        grid$status, grid_details, integrate_selected,
        inside_volume
      )
      grid$status <- volume_merged$status
      grid_details <- volume_merged$details
      outside_volume_merged <- .merge_kernel_result(
        grid$status, grid_details,
        integrate_selected,
        outside_volume
      )
      grid$status <- outside_volume_merged$status
      grid_details <- outside_volume_merged$details
      grid$cum_volume_ib[integrate_selected] <- .volume_from_native(
        inside_volume$value,
        measurement_system, model_object$measurement_system
      )
      grid$cum_volume_ob[integrate_selected] <- .volume_from_native(
        outside_volume$value,
        measurement_system, model_object$measurement_system
      )
    }
  }

  result <- data.frame(
    tree_id = common$values$id[grid$tree], h = rep(NA_real_, nrow(grid)),
    dib = grid$dib, dob = grid$dob, cum_volume_ib = grid$cum_volume_ib,
    cum_volume_ob = grid$cum_volume_ob,
    status = as.integer(grid$status), stringsAsFactors = FALSE
  )
  for (group_index in seq_along(call$resolved$dictionary)) {
    model_object <- call$resolved$models[[group_index]]
    if (is.null(model_object)) {
      next
    }
    selected <- tree_group[grid$tree] == group_index & is.finite(grid$native_h)
    result$h[selected] <- .height_from_native(
      grid$native_h[selected], measurement_system,
      model_object$measurement_system
    )
  }
  ordering <- order(match(result$tree_id, common$values$id), result$h, na.last = TRUE)
  result <- result[ordering, , drop = FALSE]
  rownames(result) <- NULL
  if (!status_requested) {
    .status_warning("stem_profile", grid$status, grid_details, common$size,
      tree = grid$tree
    )
  }
  structure(.public_status(result), class = c("stem_profile", "data.frame"))
}
