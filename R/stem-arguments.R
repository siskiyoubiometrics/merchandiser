.reserved_aux_names <- c("status", "units", "id")

.treevolume_compat <- function() {
  value <- getOption("merchandiser.compat", "port")
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% c("port", "nvel")) {
    stop(
      "option merchandiser.compat must be one of: port, nvel.",
      call. = FALSE
    )
  }
  value
}

.auxiliary_table <- data.frame(
  name = c(
    "form_class", "upper_ht1", "upper_d1", "upper_ht2", "upper_d2",
    "site_index", "basal_area", "bark_ratio", "decay_class", "cull",
    "upper_bark", "spcd"
  ),
  type = c(rep("numeric", 10L), "character", "numeric"),
  dimension = c(
    "dimensionless", "height", "diameter", "height", "diameter",
    "height", "area", "dimensionless", "dimensionless", "dimensionless",
    "dimensionless", "dimensionless"
  ),
  domain = c(
    "positive", "positive", "positive", "positive", "positive",
    "positive", "positive", "fraction", "integer_1_5", "percentage",
    "bark_basis", "positive_integer"
  ),
  stringsAsFactors = FALSE
)

.capture_aux <- function(dots) {
  if (!is.list(dots)) {
    stop("auxiliary inputs must be supplied through ... as vectors.", call. = FALSE)
  }
  if (!length(dots)) {
    return(dots)
  }
  dot_names <- names(dots)
  if (is.null(dot_names) || any(!nzchar(dot_names)) || anyDuplicated(dot_names)) {
    stop("every auxiliary input must have a unique non-empty name.", call. = FALSE)
  }
  reserved <- intersect(dot_names, .reserved_aux_names)
  if (length(reserved)) {
    stop("reserved auxiliary name: ", reserved[[1L]], call. = FALSE)
  }
  if (any(!vapply(dots, is.atomic, logical(1)))) {
    stop("auxiliary inputs must be atomic vectors.", call. = FALSE)
  }
  dots
}

.common_size <- function(values) {
  lengths <- vapply(values, length, integer(1))
  if (!length(lengths)) {
    return(1L)
  }
  if (any(lengths == 0L)) {
    if (all(lengths %in% c(0L, 1L))) {
      return(0L)
    }
    stop("only size-one vectors can be recycled. Zero cannot match a larger size.",
      call. = FALSE
    )
  }
  non_scalar <- unique(lengths[lengths != 1L])
  if (length(non_scalar) > 1L) {
    stop("only size-one vectors can be recycled to the common size.", call. = FALSE)
  }
  if (length(non_scalar)) non_scalar else 1L
}

.recycle_common <- function(value, size, name) {
  if (length(value) == size) {
    return(value)
  }
  if (length(value) == 1L || (!size && !length(value))) {
    return(rep(value, size))
  }
  stop(name, " must have size one or the common size.", call. = FALSE)
}

.prepare_vectors <- function(values, numeric_names, character_names, aux) {
  aux <- .capture_aux(aux)
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
  for (name in numeric_names) {
    if (!is.numeric(values[[name]])) {
      stop(name, " must be numeric.", call. = FALSE)
    }
    values[[name]] <- as.double(values[[name]])
  }
  for (name in character_names) {
    is_model_factor <- identical(name, "model") && is.factor(values[[name]])
    if (!is.character(values[[name]]) && !is_model_factor) {
      stop(name, " must be character.", call. = FALSE)
    }
  }
  list(size = size, values = values, aux = aux)
}

.input_status <- function(size, values, required_names) {
  status <- integer(size)
  if (!size) {
    return(status)
  }
  missing <- rep(FALSE, size)
  for (name in required_names) {
    value <- values[[name]]
    if (is.numeric(value)) {
      missing <- missing | !is.finite(value)
    } else {
      missing <- missing | is.na(value)
    }
  }
  status[missing] <- 1L
  status
}

.assign_status <- function(status, condition, code, eligible = status == 0L) {
  rows <- which(eligible & condition)
  status[rows] <- as.integer(code)
  status
}

.dictionary_groups <- function(ids, size = length(ids)) {
  if (length(ids) == 1L) {
    return(list(
      dictionary = as.character(ids),
      encoded = 1L,
      groups = stats::setNames(list(seq_len(size)), "1")
    ))
  }
  if (is.factor(ids)) {
    factor_levels <- levels(ids)
    present <- vapply(factor_levels, function(level) {
      any(ids == level, na.rm = TRUE)
    }, logical(1))
    dictionary <- factor_levels[present]
    groups <- lapply(dictionary, function(level) which(ids == level))
    if (anyNA(ids)) {
      dictionary <- c(dictionary, NA_character_)
      groups <- c(groups, list(which(is.na(ids))))
    }
    names(groups) <- as.character(seq_along(groups))
    return(list(dictionary = dictionary, encoded = ids, groups = groups))
  } else {
    dictionary <- unique(ids)
    encoded <- match(ids, dictionary)
  }
  list(
    dictionary = dictionary,
    encoded = encoded,
    groups = split(seq_along(ids), encoded)
  )
}

.auxiliary_spec <- function(name) {
  .auxiliary_table[match(name, .auxiliary_table$name), , drop = FALSE]
}

.auxiliary_missing <- function(value) {
  is.na(value) | (is.numeric(value) & !is.finite(value))
}

.auxiliary_domain_invalid <- function(value, domain) {
  switch(domain,
    positive = value <= 0,
    fraction = value <= 0 | value > 1,
    integer_1_5 = value != floor(value) | value < 1 | value > 5,
    percentage = value < 0 | value > 100,
    bark_basis = !value %in% c("ib", "ob"),
    positive_integer = value != floor(value) | value <= 0 |
      value > .Machine$integer.max,
    rep(FALSE, length(value))
  )
}

.validate_auxiliary_values <- function(name, value, rows, status) {
  spec <- .auxiliary_spec(name)
  if (!nrow(spec)) {
    stop("auxiliary input is not in the centralized table: ", name, call. = FALSE)
  }
  selected <- value[rows]
  expected_type <- spec$type[[1L]]
  type_ok <- if (identical(expected_type, "numeric")) {
    is.numeric(selected)
  } else {
    is.character(selected)
  }
  if (!type_ok) {
    stop(
      "auxiliary ", name, " has ", length(rows), " values of the wrong type.",
      call. = FALSE
    )
  }
  missing <- .auxiliary_missing(selected)
  missing_rows <- rows[missing]
  status[missing_rows[status[missing_rows] %in% c(0L, 52L)]] <- 1L
  invalid <- !missing & .auxiliary_domain_invalid(selected, spec$domain[[1L]])
  invalid_count <- sum(invalid)
  if (invalid_count) {
    stop(
      "auxiliary ", name, " has ", invalid_count, " out-of-domain ",
      if (invalid_count == 1L) "value." else "values.",
      call. = FALSE
    )
  }
  status
}

.validate_group_aux <- function(model, aux, rows, status) {
  for (name in model$inputs$required) {
    if (is.null(aux[[name]])) {
      status[rows[status[rows] == 0L]] <- 51L
    }
  }
  declared <- c(model$inputs$required, model$inputs$optional)
  for (name in intersect(declared, names(aux))) {
    status <- .validate_auxiliary_values(name, aux[[name]], rows, status)
  }
  for (pair in model$inputs$pairs) {
    supplied <- vapply(pair, function(name) !is.null(aux[[name]]), logical(1))
    if (any(supplied) && !all(supplied)) {
      status[rows[status[rows] == 0L]] <- 51L
    }
  }
  status
}

.resolve_models <- function(ids, aux, status) {
  grouping <- .dictionary_groups(ids, length(status))
  models <- vector("list", length(grouping$dictionary))
  details <- rep(NA_character_, length(status))
  declared_aux <- "spcd"
  if (!is.null(aux$spcd)) {
    if (!is.numeric(aux$spcd)) {
      stop(
        "auxiliary spcd has ", length(status), " values of the wrong type.",
        call. = FALSE
      )
    }
    missing <- .auxiliary_missing(aux$spcd)
    status[status == 0L & missing] <- 1L
    valid_integer <- aux$spcd > 0 & aux$spcd <= .Machine$integer.max &
      aux$spcd == floor(aux$spcd)
    unknown <- !missing & !valid_integer
    status[status == 0L & unknown] <- 7L
  }
  for (group_index in seq_along(grouping$dictionary)) {
    rows <- grouping$groups[[as.character(group_index)]]
    id <- grouping$dictionary[[group_index]]
    entry <- if (!is.na(id) && nzchar(id)) .registry_lookup(id) else NULL
    if (is.null(entry)) {
      status[rows[status[rows] == 0L]] <- 50L
      if (!is.na(id) && nzchar(id)) {
        details[rows] <- .nvel_identifier_resolution(id)$reason
      }
      next
    }
    model <- entry$model
    models[[group_index]] <- model
    status <- .validate_group_aux(model, aux, rows, status)
    declared_aux <- union(
      declared_aux,
      c(model$inputs$required, model$inputs$optional)
    )
    if (!is.null(aux$spcd)) {
      species <- aux$spcd[rows]
      invalid <- .auxiliary_missing(species)
      valid_integer <- species > 0 & species <= .Machine$integer.max &
        species == floor(species)
      unknown <- !invalid & !valid_integer
      if (length(model$species)) {
        outside <- !invalid & !unknown & !species %in% model$species
        status[rows[status[rows] == 0L & outside]] <- 52L
      }
    }
  }
  unknown_aux <- setdiff(names(aux), declared_aux)
  if (length(unknown_aux)) {
    stop("undeclared auxiliary input: ", unknown_aux[[1L]], call. = FALSE)
  }
  list(
    dictionary = grouping$dictionary,
    encoded = grouping$encoded,
    groups = grouping$groups,
    models = models,
    status = status,
    details = details
  )
}

.area_to_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  imperial_to_metric <- 0.09290304 / 0.40468564224
  if (identical(caller_units, "metric")) {
    value / imperial_to_metric
  } else {
    value * imperial_to_metric
  }
}

.auxiliary_to_native <- function(value, dimension, caller_units, native_units) {
  switch(dimension,
    height = .height_to_native(value, caller_units, native_units),
    diameter = .diameter_to_native(value, caller_units, native_units),
    area = .area_to_native(value, caller_units, native_units),
    value
  )
}

.set_aux_caller_units <- function(aux, units) {
  attr(aux, "caller_units") <- units
  aux
}

.subset_aux <- function(aux, rows, model) {
  keep <- intersect(names(aux), c(
    model$inputs$required, model$inputs$optional, "spcd", "bark_ratio"
  ))
  caller_units <- attr(aux, "caller_units", exact = TRUE)
  if (is.null(caller_units)) {
    caller_units <- model$units
  }
  result <- lapply(aux[keep], `[`, rows)
  for (name in intersect(keep, .auxiliary_table$name)) {
    dimension <- .auxiliary_spec(name)$dimension[[1L]]
    result[[name]] <- .auxiliary_to_native(
      result[[name]], dimension, caller_units, model$units
    )
  }
  result
}

.diameter_to_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) value / 2.54 else value * 2.54
}

.diameter_from_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) value * 2.54 else value / 2.54
}

.height_to_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) value / 0.3048 else value * 0.3048
}

.height_from_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) value * 0.3048 else value / 0.3048
}

.volume_from_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) {
    value * 0.028316846592
  } else {
    value / 0.028316846592
  }
}

.weight_from_native <- function(value, caller_units, native_units) {
  if (identical(caller_units, native_units)) {
    return(value)
  }
  if (identical(caller_units, "metric")) value * 0.45359237 else value / 0.45359237
}

.step_to_native_height <- function(value, caller_units, native_units) {
  height_in_caller <- if (identical(caller_units, "imperial")) value / 12 else value / 100
  .height_to_native(height_in_caller, caller_units, native_units)
}
