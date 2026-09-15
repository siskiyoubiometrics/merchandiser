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

#' Diameters along a tree stem
#'
#' @param tree_id Identifier for matching profile rows to input trees. Atomic vector, unique
#'   tree identifiers.
#'   Required, with no default.
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
#' @param step The spacing sets the distance between points on the stem profile. Numeric vector,
#'   feet, at least 0.05. Default: \code{0.5}, on the half foot grid.
#' @param from Start height above ground. Numeric vector, feet. Default `NULL` uses `stump_ht`.
#' @param to End height above ground. Numeric vector, feet. Default `NULL` uses the tree tip.
#' @param from_dib Inside bark diameter at the start. Numeric vector, inches, default `NULL`.
#' @param from_dob Outside bark diameter at the start. Numeric vector, inches, default `NULL`.
#' @param to_dib Inside bark diameter at the end. Numeric vector, inches, default `NULL`.
#' @param to_dob Outside bark diameter at the end. Numeric vector, inches, default `NULL`.
#'   Diameter bounds use [height_at_dib()] or [height_at_dob()], including their status conventions.
#'   Supply at most one start argument and at most one end argument.
#' @param stump_ht Stump height above ground. Numeric vector, feet, default `1`.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A `stem_profile` data frame ordered by input tree and height, with columns:
#'   * `tree_id`: tree identifier.
#'   * `h`: height above ground, feet.
#'   * `dib`, `dob`: diameter inside and outside bark, inches.
#'   * `status`: integer result code.
#'   * `cum_volume_ib`, `cum_volume_ob`: cumulative volume inside and outside bark, cubic feet.
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
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure and show the first example stem
#' stem_profile(tree_id = example_trees$tree_id[1],
#'              dbh = example_trees$dbh[1],
#'              ht = example_trees$ht[1],
#'              spcd = example_trees$spcd[1]) %>%
#'   transmute(`height (feet)` = h,
#'             `diameter inside bark (inches)` = dib,
#'             `diameter outside bark (inches)` = dob) %>%
#'   head(n = 3)
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
