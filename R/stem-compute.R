.tv_cpp_kernel_eval_impl <- tv_cpp_kernel_eval_impl
.tv_cpp_duplicate_map_impl <- tv_cpp_duplicate_map_impl

tv_cpp_kernel_eval <- function(
  kernel_name, operation, dbh, ht, x1, x2, bark_ratio, threads,
  upper_ht1 = NULL, upper_d1 = NULL, upper_ht2 = NULL, upper_d2 = NULL, upper_bark = NULL,
  site_index = NULL, basal_area = NULL, form_class = NULL, coefficients = numeric()
) {
  size <- length(dbh)
  numeric_default <- function(value) {
    if (is.null(value))
      rep(NA_real_, size) else value
  }
  if (is.null(upper_bark)) {
    upper_bark <- integer(size)
  }
  result <- .tv_cpp_kernel_eval_impl(
    kernel_name, operation, dbh, ht, x1, x2,
    bark_ratio, numeric_default(upper_ht1),
    numeric_default(upper_d1), numeric_default(upper_ht2), numeric_default(
      upper_d2
    ), upper_bark,
    numeric_default(site_index), numeric_default(basal_area), numeric_default(form_class),
    coefficients, identical(.treevolume_compat(), "nvel"), threads
  )
  structured <- startsWith(kernel_name, "flewelling:") | startsWith(kernel_name, "clark:") |
    startsWith(kernel_name, "r10:") | startsWith(kernel_name, "r4mat:") |
    startsWith(
      kernel_name,
      "smalltapers:"
    ) | startsWith(kernel_name, "nsvb:") | startsWith(
    kernel_name,
    "published:"
  ) |
    operation %in% c(6L, 7L, 8L)
  if (structured)
    result else result$value
}

.callback_result <- function(callback, arguments, size) {
  condition_message <- NULL
  value <- tryCatch(withCallingHandlers(do.call(callback, arguments),
    condition = function(condition) {
      stop(conditionMessage(condition), call. = FALSE)
    }
  ), error = function(error) {
    condition_message <<- conditionMessage(error)
    NULL
  })
  if (!is.null(condition_message)) {
    return(list(value = rep(NA_real_, size), status = rep(54L, size), details = rep(
      condition_message,
      size
    )))
  }
  supplied_status <- attr(value, "status", exact = TRUE)
  if (!is.double(value) || length(value) != size) {
    return(list(value = rep(NA_real_, size), status = rep(54L, size), details = rep(
      "kernel returned the wrong type or length",
      size
    )))
  }
  if (is.null(supplied_status)) {
    supplied_status <- integer(size)
  }
  valid_status <- is.numeric(supplied_status) && length(supplied_status) == size &&
    all(is.finite(supplied_status)) &&
    all(supplied_status <= .Machine$integer.max) && all(supplied_status == floor(
    supplied_status
  )) &&
    all(supplied_status %in% .tv_status_table$code | supplied_status > 300L)
  if (!valid_status) {
    return(list(value = rep(NA_real_, size), status = rep(54L, size), details = rep(
      "kernel returned an invalid status attribute",
      size
    )))
  }
  supplied_status <- as.integer(supplied_status)
  supplied_status[!is.finite(value) & supplied_status == 0L] <- 54L
  details <- rep(NA_character_, size)
  details[!is.finite(value) & supplied_status == 54L] <-
    "kernel returned a non-finite value"
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

.compiled_result <- function(
  model, operation, dbh, ht, x1, x2, bark_ratio,
  aux = list(), rows = seq_along(dbh)
) {
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
    upper_ht1 = .compiled_auxiliary(
      native_aux,
      "upper_ht1", size
    ), upper_d1 = .compiled_auxiliary(
      native_aux, "upper_d1",
      size
    ), upper_ht2 = .compiled_auxiliary(
      native_aux,
      "upper_ht2", size
    ), upper_d2 = .compiled_auxiliary(
      native_aux, "upper_d2",
      size
    ), upper_bark = as.integer(upper_bark),
    site_index = .compiled_auxiliary(native_aux, "site_index", size),
    basal_area = .compiled_auxiliary(
      native_aux,
      "basal_area", size
    ), form_class = .compiled_auxiliary(native_aux, "form_class", size)
  )
  duplicate_map <- do.call(.tv_cpp_duplicate_map_impl, inputs)
  evaluate_inputs <- inputs
  if (length(duplicate_map)) {
    evaluate_inputs <- lapply(inputs, `[`, duplicate_map$unique)
  }
  evaluated <- do.call(tv_cpp_kernel_eval, c(
    list(
      kernel_name = model$kernel$key,
      operation = operation
    ),
    evaluate_inputs, list(threads = threads(), coefficients = as.double(model$data))
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
  list(value = value, status = status, details = ifelse(status == 54L,
    "compiled kernel returned a non-finite value",
    NA_character_
  ))
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
  model_before_numeric <- current >= 100L & current < 150L & returned >= 50L &
    returned < 100L &
    returned != 52L
  replace <- returned != 0L & (current == 0L | model_before_numeric | current == 52L)
  current[replace] <- returned[replace]
  status[rows] <- current
  has_detail <- replace & !is.na(result$details)
  details[rows[has_detail]] <- result$details[has_detail]
  list(status = status, details = details)
}

.evaluate_profile_native <- function(model, basis, dbh, ht, h, aux, rows) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  callback_name <- if (direct_dob)
    "dob" else "dib"
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = identical(basis, "dob") &&
                                !direct_dob)
    operation <- if (identical(basis, "dob"))
      4L else 1L
    result <- .compiled_result(
      model, operation, dbh, ht, h, rep(0, length(h)), bark$value,
      aux, rows
    )
  } else {
    result <- .callback_result(model[[callback_name]], list(
      dbh = dbh, ht = ht,
      h = h, aux = .subset_aux(
        aux,
        rows, model
      )
    ), length(h))
  }
  if (identical(basis, "dob") && !direct_dob && !identical(model$kernel$type, "compiled")) {
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
  prepared <- .prepare_vectors(list(dbh = dbh, ht = ht, h = h, model = model),
    numeric_names = c(
      "dbh",
      "ht", "h"
    ), character_names = "model", aux = aux
  )
  values <- prepared$values
  status <- .input_status(prepared$size, values, c("dbh", "ht", "h", "model"))
  status <- .assign_status(status, values$dbh <= 0 | values$dbh > 400, 2L)
  status <- .assign_status(status, values$ht <= 0 | values$ht > 500, 3L)
  status <- .assign_status(status, values$h < 0 | values$h > values$ht, 4L)
  resolved <- .resolve_models(values$model, prepared$aux, status)
  c(prepared, list(resolved = resolved))
}

.diameter_impl <-
  function(dbh, ht, h, model, aux, measurement_system, status, basis, function_name) {
    measurement_system <- .validate_units(measurement_system)
    status_requested <- .validate_status(status)
    generation <- .registry_enter()
    on.exit(.registry_exit(generation), add = TRUE)
    call <- .prepare_profile_call(dbh, ht, h, model, aux)
    call$aux <- .set_aux_caller_units(call$aux, measurement_system)
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
      native_dbh <-
        .diameter_to_native(
          call$values$dbh[rows], measurement_system, model_object$measurement_system
        )
      native_ht <-
        .height_to_native(
          call$values$ht[rows], measurement_system, model_object$measurement_system
        )
      native_h <-
        .height_to_native(
          call$values$h[rows], measurement_system, model_object$measurement_system
        )
      evaluated <- .evaluate_profile_native(
        model_object, basis, native_dbh,
        native_ht, native_h,
        call$aux, rows
      )
      merged <- .merge_kernel_result(result_status, details, rows, evaluated)
      result_status <- merged$status
      details <- merged$details
      output[
        rows
      ] <-
        .diameter_from_native(
          evaluated$value, measurement_system, model_object$measurement_system
        )
    }
    output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
    .status_result(output, result_status, status_requested, details, function_name)
  }

#' Diameter inside bark at a height
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default.
#' @param h The measurement height is the distance above the ground. Numeric vector, feet.
#'   Required, with no default. Heights must be greater than zero and at most 500 feet.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param model Registered taper model identifier. Character vector of
#'   registered model identifiers. Default: \code{NULL}.
#'   model NULL selects the shipped default for the species, see [default_taper_models].
#'   Species without a default return status 404.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A data frame with value (diameter inside bark, inches) and status (integer result
#'   code), one row per input row.
#' @usage
#' dib(
#'   dbh,
#'   ht,
#'   h,
#'   spcd,
#'   model = NULL,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure diameter inside bark at breast height.
#' dib(dbh = example_trees$dbh[1],
#'     ht = example_trees$ht[1],
#'     h = 4.5,
#'     spcd = example_trees$spcd[1]) %>%
#'   rename(`diameter inside bark (inches)` = value)
dib <- function(dbh, ht, h, spcd, model = NULL, ...) {
  input <- .mc_stem_inputs(spcd, model, list(...), FALSE)
  .public_status(
    .diameter_impl(dbh, ht, h, input$model, input$aux, "imperial", TRUE, "dib", "dib")
  )
}

#' Diameter outside bark at a height
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default.
#' @param h The measurement height is the distance above the ground. Numeric vector, feet.
#'   Required, with no default. Heights must be greater than zero and at most 500 feet.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param model Registered taper model identifier. Character vector of
#'   registered model identifiers. Default: \code{NULL}.
#'   model NULL selects the shipped default for the species, see [default_taper_models].
#'   Species without a default return status 404.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A data frame with value (diameter outside bark, inches) and status (integer result
#'   code), one row per input row.
#' @usage
#' dob(
#'   dbh,
#'   ht,
#'   h,
#'   spcd,
#'   model = NULL,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure diameter outside bark at breast height.
#' dob(dbh = example_trees$dbh[1],
#'     ht = example_trees$ht[1],
#'     h = 4.5,
#'     spcd = example_trees$spcd[1]) %>%
#'   rename(`diameter outside bark (inches)` = value)
dob <- function(dbh, ht, h, spcd, model = NULL, ...) {
  input <- .mc_stem_inputs(spcd, model, list(...), TRUE)
  .public_status(
    .diameter_impl(dbh, ht, h, input$model, input$aux, "imperial", TRUE, "dob", "dob")
  )
}

.inverse_grid_step <- function(measurement_system) {
  one_inch <- if (identical(measurement_system, "imperial"))
    1 / 12 else 0.0254
  one_inch / 16
}

.refine_crossing_brackets <- function(
  model, basis, dbh, ht, target, aux, rows,
  lower, upper,
  lower_value, max_iterations = 200L
) {
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
      model, basis, dbh[work], ht[work], midpoint,
      aux, rows[work]
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
      now <- upper[good_rows] - lower[good_rows] <= 1e-04
      result[good_rows[now]] <- (upper[good_rows[now]] + lower[good_rows[now]]) / 2
      active[good_rows[now]] <- FALSE
    }
  }
  status[active] <- 103L
  list(value = result, status = status, details = details)
}

.numeric_inverse <- function(
  model, basis, dbh, ht, target, aux, rows,
  max_iterations = 200L
) {
  if (identical(basis, "dib") && identical(model$kernel$type, "compiled") &&
    startsWith(
      model$kernel$key,
      "smalltapers:"
    )) {
    bark <- .model_bark_ratio(model, aux, rows, require = FALSE)$value
    return(.compiled_result(
      model, 5L, dbh, ht, target, rep(0, length(target)), bark, aux,
      rows
    ))
  }
  tree_count <- length(dbh)
  stump <- rep(model$stump_ht, tree_count)
  grid_step <- .inverse_grid_step(model$measurement_system)
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
    model, basis, dbh[map], ht[map], grid_h, aux,
    flat_rows
  )
  result <- rep(NA_real_, tree_count)
  status <- integer(tree_count)
  details <- rep(NA_character_, tree_count)
  exact_roots <- vector("list", tree_count)
  bracket_tree <- bracket_lower <- bracket_upper <- bracket_value <- vector(
    "list",
    tree_count
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
      model, basis, dbh[bracket_tree], ht[bracket_tree],
      target[bracket_tree], aux, rows[bracket_tree], bracket_lower, bracket_upper,
      bracket_value,
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
  exact_only <- which(lengths(exact_roots) > 0L & !seq_len(tree_count) %in% bracket_tree)
  for (tree in exact_only) {
    result[[tree]] <- max(exact_roots[[tree]])
  }
  list(value = result, status = status, details = details)
}

.analytic_inverse <- function(model, dbh, ht, target, aux, rows) {
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = FALSE)$value
    return(.compiled_result(
      model, 2L, dbh, ht, target, rep(0, length(target)), bark, aux,
      rows
    ))
  }
  .callback_result(model$height_at_dib, list(
    dbh = dbh, ht = ht, dib = target,
    aux = .subset_aux(
      aux,
      rows, model
    )
  ), length(target))
}

.analytic_discovered_inverse <- function(model, dbh, ht, target, aux, rows) {
  analytic <- .analytic_inverse(model, dbh, ht, target, aux, rows)
  discovered <- .numeric_inverse(model, "dib", dbh, ht, target, aux, rows)
  use_discovery <- analytic$status %in% c(0L, 52L, 102L) & discovered$status != 0L
  analytic$status[use_discovery] <- discovered$status[use_discovery]
  analytic$details[use_discovery] <- discovered$details[use_discovery]
  usable <- analytic$status %in% c(0L, 52L, 102L) & discovered$status %in% c(0L, 102L)
  analytic$value[!usable] <- NA_real_
  analytic
}

.inverse_group <- function(model, basis, dbh, ht, target, aux, rows) {
  direct_dob <- identical(basis, "dob") && isTRUE(model$kernel$has_dob)
  if (identical(model$kernel$type, "compiled")) {
    bark <- .model_bark_ratio(model, aux, rows, require = identical(basis, "dob") &&
                                !direct_dob)$value
    return(.compiled_result(
      model, if (identical(basis, "dib")) 6L else 7L, dbh, ht, target,
      rep(model$stump_ht, length(target)), bark, aux, rows
    ))
  }
  if (identical(basis, "dob") && !direct_dob) {
    bark <- .model_bark_ratio(model, aux, rows, require = TRUE)
    if (any(bark$invalid)) {
      result <- list(value = rep(NA_real_, length(target)), status = ifelse(bark$invalid,
                       53L, 0L
                     ), details = rep(NA_character_, length(target)))
      good <- which(!bark$invalid)
      if (!length(good)) {
        return(result)
      }
      inside <- if (isTRUE(model$kernel$has_inverse)) {
        .analytic_discovered_inverse(
          model, dbh[good], ht[good], target[good] *
            bark$value[good],
          aux, rows[good]
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

.height_impl <- function(
  dbh, ht, diameter, model, aux, measurement_system, status, basis,
  function_name
) {
  measurement_system <- .validate_units(measurement_system)
  status_requested <- .validate_status(status)
  generation <- .registry_enter()
  on.exit(.registry_exit(generation), add = TRUE)
  call <- .prepare_vectors(list(
    dbh = dbh, ht = ht, diameter = diameter,
    model = model
  ), numeric_names = c(
    "dbh",
    "ht", "diameter"
  ), character_names = "model", aux = aux, aliases = c(diameter = basis))
  result_status <- .input_status(call$size, call$values, c(
    "dbh", "ht", "diameter",
    "model"
  ))
  result_status <- .assign_status(result_status, call$values$dbh <= 0 | call$values$dbh > 400, 2L)
  result_status <- .assign_status(result_status, call$values$ht <= 0 | call$values$ht > 500, 3L)
  result_status <- .assign_status(result_status, call$values$diameter <= 0, 5L)
  resolved <- .resolve_models(call$values$model, call$aux, result_status)
  call$aux <- .set_aux_caller_units(call$aux, measurement_system)
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
    native_dbh <-
      .diameter_to_native(
        call$values$dbh[rows], measurement_system, model_object$measurement_system
      )
    native_ht <-
      .height_to_native(
        call$values$ht[rows], measurement_system, model_object$measurement_system
      )
    native_target <- .diameter_to_native(
      call$values$diameter[rows], measurement_system,
      model_object$measurement_system
    )
    inverted <- .inverse_group(
      model_object, basis, native_dbh, native_ht, native_target,
      call$aux, rows
    )
    merged <- .merge_kernel_result(result_status, details, rows, inverted)
    result_status <- merged$status
    details <- merged$details
    output[
      rows
    ] <-
      .height_from_native(inverted$value, measurement_system, model_object$measurement_system)
  }
  output[!result_status %in% c(0L, 52L, 102L)] <- NA_real_
  .status_result(output, result_status, status_requested, details, function_name)
}

#' Height to a diameter inside bark
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default.
#' @param dib The target diameter identifies the point to locate on the stem. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param model Registered taper model identifier. Character vector of
#'   registered model identifiers. Default: \code{NULL}.
#'   model NULL selects the shipped default for the species, see [default_taper_models].
#'   Species without a default return status 404.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A data frame with value (height above ground, feet) and status (integer result code),
#'   one row per input row.
#' @usage
#' height_at_dib(
#'   dbh,
#'   ht,
#'   dib,
#'   spcd,
#'   model = NULL,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Find the height of a six-inch inside-bark diameter.
#' height_at_dib(dbh = example_trees$dbh[1],
#'               ht = example_trees$ht[1],
#'               dib = 6,
#'               spcd = example_trees$spcd[1],
#'               model = example_trees$model[1]) %>%
#'   rename(`height (feet)` = value)
height_at_dib <- function(dbh, ht, dib, spcd, model = NULL, ...) {
  input <- .mc_stem_inputs(spcd, model, list(...), FALSE)
  .public_status(.height_impl(
    dbh, ht, dib, input$model, input$aux, "imperial", TRUE, "dib",
    "height_at_dib"
  ))
}

#' Height to a diameter outside bark
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default.
#' @param dob The target diameter identifies the point to locate on the stem. Numeric vector,
#'   inches, greater than zero and at most 400. Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param model Registered taper model identifier. Character vector of
#'   registered model identifiers. Default: \code{NULL}.
#'   model NULL selects the shipped default for the species, see [default_taper_models].
#'   Species without a default return status 404.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return A data frame with value (height above ground, feet) and status (integer result code),
#'   one row per input row.
#' @usage
#' height_at_dob(
#'   dbh,
#'   ht,
#'   dob,
#'   spcd,
#'   model = NULL,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Find the height of a six-inch outside-bark diameter.
#' height_at_dob(dbh = example_trees$dbh[1],
#'               ht = example_trees$ht[1],
#'               dob = 6,
#'               spcd = example_trees$spcd[1],
#'               model = example_trees$model[1]) %>%
#'   rename(`height (feet)` = value)
height_at_dob <- function(dbh, ht, dob, spcd, model = NULL, ...) {
  input <- .mc_stem_inputs(spcd, model, list(...), TRUE)
  .public_status(.height_impl(
    dbh, ht, dob, input$model, input$aux, "imperial", TRUE, "dob",
    "height_at_dob"
  ))
}
