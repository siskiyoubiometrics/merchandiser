.tv_registry <- local({
  registry <- new.env(parent = emptyenv())
  registry$models <- list()
  registry$patterns <- data.frame()
  registry$generation <- 0L
  registry$active <- 0L
  registry
})

.nvel_identifier_pattern <- function(pattern) {
  characters <- strsplit(pattern, "", fixed = TRUE)[[1L]]
  paste0(
    "^", paste0(ifelse(characters %in% c("?", "*"), ".", characters), collapse = ""),
    "$"
  )
}

.nvel_identifier_row <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) != 10L || grepl(
    "[?*]",
    id
  ))
    return(NULL)
  catalog <- .nvel_reference("nvel_identifier_catalog.csv")
  literal <- !grepl("[?*]", catalog$voleq)
  exact <- which(literal & catalog$voleq == id)
  if (length(exact))
    return(catalog[exact[[1L]], , drop = FALSE])

  patterns <- which(!literal)
  matched <- patterns[vapply(catalog$voleq[patterns], function(pattern) {
    grepl(.nvel_identifier_pattern(pattern), id)
  }, logical(1))]
  if (!length(matched))
    return(NULL)
  # Match the most specific inventory rule. This mirrors the dispatch gates: P01DEM0*** is
  # a direct-volume equation even though its middle mnemonic also matches the broader
  # ???DEM???? profile rule, while ???F33???? wins over the Flewelling umbrella ???F??????
  # rule.
  wildcards <- nchar(gsub("[^?*]", "", catalog$voleq[matched]))
  catalog[matched[which.min(wildcards)], , drop = FALSE]
}

.nvel_family_model <- function(id, family) {
  species <- suppressWarnings(as.integer(substr(id, 8L, 10L)))
  model <- switch(family,
    flewelling_2pt = .flewelling_model(id, family, species),
    flewelling_3pt = .flewelling_model(
      id,
      family, species
    ),
    clark_r8 = .clark_model(id, family, species),
    clark_r9 = .clark_model(
      id,
      family, species
    ),
    r10_taper = .r10r4_model(id, family, species),
    r4_driver = .r10r4_model(
      id,
      family, species
    ),
    r1_taper = .smalltaper_model(id, family, species),
    r2_taper = .smalltaper_model(
      id,
      family, species
    ),
    r5_taper = .smalltaper_model(id, family, species),
    r12_taper = .smalltaper_model(
      id,
      family, species
    ),
    blm_taper = .smalltaper_model(id, family, species),
    behre_taper = .smalltaper_model(
      id,
      family, species
    ),
    nsvb = .nsvb_model(id, species),
    NULL
  )
  if (is.null(model) || !tv_cpp_kernel_exists(model$kernel$key))
    return(NULL)
  model
}

.nvel_identifier_resolution <- function(id) {
  row <- .nvel_identifier_row(id)
  if (is.null(row) && is.character(id) && length(id) == 1L && !is.na(id) && grepl(
    "^NVB[0M][0-9]{6}P$",
    id
  )) {
    model <- .nvel_family_model(id, "nsvb")
    if (!is.null(model)) {
      return(list(model = model, family = "nsvb", reason = NA_character_))
    }
  }
  if (is.null(row)) {
    return(list(model = NULL, family = NA_character_, reason = "unknown model id"))
  }
  family <- row$family[[1L]]
  profile <- isTRUE(row$profile_based[[1L]])
  if (!profile || identical(family, "direct_volume")) {
    return(list(
      model = NULL, family = "direct_volume",
      reason = "direct volume equations are not yet ported"
    ))
  }
  model <- .nvel_family_model(id, family)
  if (is.null(model)) {
    return(list(
      model = NULL, family = family,
      reason = "identifier is recognized, but its profile kernel is unavailable"
    ))
  }
  list(model = model, family = family, reason = NA_character_)
}

.registry_lookup <- function(id) {
  entry <- .tv_registry$models[[id]]
  if (!is.null(entry)) {
    return(entry)
  }
  model <- .nvel_identifier_resolution(id)$model
  if (is.null(model)) {
    return(NULL)
  }
  list(model = model, owner_package = "merchandiser", package_version = tryCatch(
    as.character(utils::packageVersion("merchandiser")),
    error = function(error) NA_character_
  ), generation = .tv_registry$generation, sealed = TRUE)
}

.registry_entry <- function(model, owner_package, sealed) {
  .tv_registry$generation <- .tv_registry$generation + 1L
  package_version <- tryCatch(as.character(utils::packageVersion(owner_package)),
    error = function(error) NA_character_
  )
  list(
    model = model, owner_package = owner_package, package_version = package_version,
    generation = .tv_registry$generation,
    sealed = sealed
  )
}

.registry_enter <- function() {
  generation <- .tv_registry$generation
  .tv_registry$active <- .tv_registry$active + 1L
  generation
}

.registry_exit <- function(generation) {
  .tv_registry$active <- max(0L, .tv_registry$active - 1L)
  if (!identical(generation, .tv_registry$generation)) {
    stop("the model registry changed during evaluation.", call. = FALSE)
  }
  invisible(NULL)
}

.check_registry_mutable <- function() {
  if (.tv_registry$active > 0L) {
    stop("the model registry cannot change during evaluation.", call. = FALSE)
  }
}

.register_package_model <- function(model) {
  .check_registry_mutable()
  model <- .validate_taper_model(model)
  if (!is.null(.tv_registry$models[[model$id]])) {
    stop("model id is already registered: ", model$id, call. = FALSE)
  }
  entry <- .registry_entry(model, "merchandiser", TRUE)
  .tv_registry$models[[model$id]] <- entry
  invisible(model)
}

#' Make a local taper equation available for this analysis
#'
#' @param x The taper specification supplies the equation to register. A taper_model object.
#'   Required, with no default.
#' @return The validated taper_model object, invisibly, after registration.
#' @usage
#' register_taper_model(
#'   x
#' )
#' @export
#' @examples
#' ## Copy an example tree's model
#' local_model <- get_taper_model(model = example_trees$model[1])
#'
#' ## Name the local copy
#' local_model$id <- 'example.register'
#'
#' ## Register the copy
#' register_taper_model(x = local_model)
#'
#' ## Remove and report the local registration
#' print(unregister_taper_model(model = local_model$id))
register_taper_model <- function(
  x
) {
  .check_registry_mutable()
  model <- .validate_taper_model(x)
  if (!grepl("^[A-Za-z][A-Za-z0-9_-]*[.][A-Za-z0-9_.-]+$", model$id)) {
    stop("private model ids must be namespaced, for example client.name.", call. = FALSE)
  }
  if (!is.null(.tv_registry$models[[model$id]])) {
    stop("model id is already registered: ", model$id, call. = FALSE)
  }
  owner <- utils::packageName(env = parent.frame())
  if (is.null(owner)) {
    owner <- strsplit(model$id, ".", fixed = TRUE)[[1L]][[1L]]
  }
  entry <- .registry_entry(model, owner, FALSE)
  .tv_registry$models[[model$id]] <- entry
  invisible(model)
}

#' Remove a local equation from the current analysis
#'
#' @param model The identifier names the taper model in the registry. Character scalar model
#'   identifier. Required, with no default.
#' @return TRUE, invisibly, after removing the private model.
#' @usage
#' unregister_taper_model(
#'   model
#' )
#' @export
#' @examples
#' ## Copy an example tree's model
#' local_model <- get_taper_model(model = example_trees$model[1])
#'
#' ## Name the local copy
#' local_model$id <- 'example.remove'
#'
#' ## Register the copy
#' register_taper_model(x = local_model)
#'
#' ## Remove and report the local registration
#' print(unregister_taper_model(model = local_model$id))
unregister_taper_model <- function(
  model
) {
  .check_registry_mutable()
  model <- .scalar_character(model, "model")
  entry <- .tv_registry$models[[model]]
  if (is.null(entry)) {
    stop("unknown model id: ", model, call. = FALSE)
  }
  if (isTRUE(entry$sealed)) {
    stop("built-in models cannot be unregistered: ", model, call. = FALSE)
  }
  .tv_registry$models[[model]] <- NULL
  .tv_registry$generation <- .tv_registry$generation + 1L
  invisible(TRUE)
}

#' Inspect a taper equation and its required inputs
#'
#' @param model The identifier names the taper model in the registry. Character scalar model
#'   identifier. Required, with no default.
#' @return A taper_model list containing id, form, kernel, inputs, spcd, stump_ht,
#'   bark_ratio (diameter ratio), source (provenance), and internal equation metadata.
#'   `measurement_system` identifies the internal units.
#'   `stump_ht` uses feet for imperial models and meters for metric models.
#' @usage
#' get_taper_model(
#'   model
#' )
#' @export
#' @examples
#' ## Inspect the model used for an example tree.
#' get_taper_model(model = example_trees$model[1])
get_taper_model <- function(
  model
) {
  model <- .scalar_character(model, "model")
  entry <- .registry_lookup(model)
  if (is.null(entry)) {
    resolution <- .nvel_identifier_resolution(model)
    stop(resolution$reason, ": ", model, call. = FALSE)
  }
  entry$model
}

#' Check whether taper equations are available
#'
#' @param model The identifier names the taper model in the registry. Character vector of model
#'   identifiers. Required, with no default.
#' @return A logical vector, TRUE where a registered taper model is available.
#' @usage
#' has_taper_model(
#'   model
#' )
#' @export
#' @examples
#' ## Check availability of the example trees' models.
#' has_taper_model(model = example_trees$model)
has_taper_model <- function(
  model
) {
  if (!is.character(model)) {
    stop("model must be a character vector.", call. = FALSE)
  }
  valid <- !is.na(model) & nzchar(model)
  result <- rep(FALSE, length(model))
  result[valid] <- vapply(model[valid], function(value) {
    !is.null(.registry_lookup(value))
  }, logical(1))
  result
}

.model_capability_values <- function(model) {
  c(
    has_dob = isTRUE(model$kernel$has_dob) || is.finite(model$bark_ratio),
    has_inverse = isTRUE(model$kernel$has_inverse),
    has_integral = isTRUE(model$kernel$has_integral)
  )
}

.empty_models <- function() {
  data.frame(
    id = character(), form = character(), kernel = character(),
    spcd_scope = character(),
    has_dob = logical(), has_inverse = logical(), has_integral = logical(),
    oracle_verified = logical(),
    measurement_system = character(), stump_ht = double(), owner_package = character(),
    source = character(),
    generation = integer(), stringsAsFactors = FALSE
  )
}

#' Find available taper equations by form or species
#'
#' @param form Equation forms used by the taper models. Character vector. Default: \code{NULL}.
#' @param spcd The species codes limit the trees this model covers. Numeric vector of species
#'   codes. Default: \code{NULL}.
#' @return A data frame describing registered models:
#'   * Identification: `id`, `form`, `kernel`, `spcd_scope`.
#'   * Logical capabilities: `has_dob`, `has_inverse`, `has_integral`.
#'   * Verification: `oracle_verified`, logical.
#'   * Internal units: `measurement_system` and `stump_ht` in that system's height unit.
#'   * Registration: `owner_package`, `source`, `generation` (revision number).
#' @usage
#' taper_models(
#'   form = NULL,
#'   spcd = NULL
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## List models covering an example species
#' models <- taper_models(spcd = example_trees$spcd[1])
#'
#' ## Show model identifiers and capabilities
#' models %>%
#'   select(id, form, has_dob, has_inverse, has_integral) %>%
#'   head(n = 3)
taper_models <- function(
  form = NULL,
  spcd = NULL
) {
  if (!is.null(form) && (!is.character(form) || anyNA(form))) {
    stop("form must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.null(spcd) && (!is.numeric(spcd) || any(!is.finite(spcd)) || any(
    spcd <=
      0
  ) || any(spcd > .Machine$integer.max) || any(spcd != floor(spcd)))) {
    stop("spcd must be NULL or a positive integer-valued vector.", call. = FALSE)
  }
  entries <- .tv_registry$models
  if (!length(entries)) {
    return(.empty_models())
  }
  keep <- vapply(entries, function(entry) {
    model <- entry$model
    family_ok <- is.null(form) || model$form %in% form
    species_ok <- is.null(spcd) || !length(model$spcd) || any(
      model$spcd %in% spcd
    )
    family_ok && species_ok
  }, logical(1))
  entries <- entries[keep]
  if (!length(entries)) {
    return(.empty_models())
  }
  rows <- lapply(entries, function(entry) {
    model <- entry$model
    capabilities <- .model_capability_values(model)
    data.frame(
      id = model$id, form = model$form, kernel = model$kernel$type,
      spcd_scope = if (length(model$spcd)) {
        paste(model$spcd, collapse = ",")
      } else {
        "unrestricted"
      }, has_dob = unname(capabilities[["has_dob"]]), has_inverse = unname(
        capabilities[["has_inverse"]]
      ),
      has_integral = unname(capabilities[["has_integral"]]), oracle_verified = attr(model,
        "oracle_verified",
        exact = TRUE
      ), measurement_system = model$measurement_system, stump_ht = model$stump_ht,
      owner_package = entry$owner_package, source = model$source,
      generation = entry$generation,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(result$id), , drop = FALSE]
}


.model_capabilities <- function(id) {
  if (!is.character(id)) {
    stop("id must be a character vector.", call. = FALSE)
  }
  values <- lapply(id, function(value) {
    if (is.na(value) || !nzchar(value) || is.null(.registry_lookup(value))) {
      return(c(has_dob = NA, has_inverse = NA, has_integral = NA))
    }
    .model_capability_values(.registry_lookup(value)$model)
  })
  matrix_values <- if (length(values)) {
    do.call(rbind, values)
  } else {
    matrix(logical(), nrow = 0L, ncol = 3L)
  }
  data.frame(
    id = id, has_dob = as.logical(matrix_values[, 1L]),
    has_inverse = as.logical(matrix_values[
      ,
      2L
    ]), has_integral = as.logical(matrix_values[, 3L]), stringsAsFactors = FALSE
  )
}

.mapping_common_size <- function(values) {
  names(values)[names(values) == "id"] <- "model"
  .common_size(values)
}

.recycle_mapping <- function(x, size) {
  if (length(x) == size)
    x else rep(x, size)
}

.empty_model_problems <- function() {
  data.frame(
    row = integer(), id = character(), status = integer(), problem = character(),
    input = character(),
    stringsAsFactors = FALSE
  )
}

#' Check equation assignments and required tree measurements
#'
#' @param model The identifier names the taper model in the registry. Character vector of model
#'   identifiers. Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Default: \code{NULL}.
#' @param ... Extra tree measurements allow the model inputs to be checked. Named numeric
#'   vectors, diameters in inches and heights in feet. No extra measurements by default.
#' @return A data frame with row (input row number), id (model identifier), status (integer status
#'   code), problem (problem name), and input (affected argument name). An empty table means no
#'   problems were found.
#' @usage
#' check_taper_models(
#'   model,
#'   spcd = NULL,
#'   ...
#' )
#' @export
#' @examples
#' ## Check the example model identifiers.
#' check_taper_models(model = example_trees$model)
check_taper_models <- function(
  model,
  spcd = NULL,
  ...
) {
  id <- model
  aux <- list(...)
  if (!is.character(id) && !is.factor(id)) {
    stop("model must be a character vector or factor.", call. = FALSE)
  }
  if (!is.null(spcd) && !is.numeric(spcd)) {
    stop("spcd must be NULL or numeric.", call. = FALSE)
  }
  aux <- .capture_aux(aux)
  values <- c(list(id = id), if (!is.null(spcd)) list(spcd = spcd), aux)
  size <- .mapping_common_size(values)
  if (!size) {
    return(.empty_model_problems())
  }
  values <- Map(function(value, name) {
    if (identical(name, "id") && (length(value) == 1L || is.factor(value))) {
      return(value)
    }
    .recycle_mapping(value, size)
  }, values, names(values))
  ids <- values$id
  species <- values$spcd
  aux_values <- values[setdiff(names(values), c("id", "spcd"))]
  problems <- list()
  add_problem <- function(row, model_id, code, problem, input = "") {
    problems[[length(problems) + 1L]] <<- data.frame(
      row = row, id = model_id, status = code,
      problem = problem, input = input, stringsAsFactors = FALSE
    )
  }
  grouping <- .dictionary_groups(ids, size)
  declared_aux <- character()
  for (group_index in seq_along(grouping$dictionary)) {
    model_id <- grouping$dictionary[[group_index]]
    entry <- if (!is.na(model_id) && nzchar(model_id)) {
      .registry_lookup(model_id)
    } else {
      NULL
    }
    if (is.null(entry)) {
      next
    }
    model <- entry$model
    declared <- c(model$inputs$required, model$inputs$optional)
    declared_aux <- union(declared_aux, declared)
    rows <- grouping$groups[[as.character(group_index)]]
    for (input in intersect(declared, names(aux_values))) {
      .validate_auxiliary_values(input, aux_values[[input]], rows, integer(size))
    }
  }
  unknown_aux <- setdiff(names(aux_values), declared_aux)
  if (length(unknown_aux)) {
    stop("undeclared auxiliary input: ", unknown_aux[[1L]], call. = FALSE)
  }
  for (row in seq_len(size)) {
    model_id <- as.character(if (length(ids) == 1L) ids[[1L]] else ids[[row]])
    if (is.na(model_id)) {
      add_problem(row, model_id, 1L, "na_input", "model")
      next
    }
    entry <- if (nzchar(model_id)) {
      .registry_lookup(model_id)
    } else {
      NULL
    }
    if (is.null(entry)) {
      add_problem(row, model_id, 50L, "unknown_model")
      next
    }
    model <- entry$model
    if (!is.null(species)) {
      species_value <- species[[row]]
      if (is.na(species_value) || !is.finite(species_value)) {
        add_problem(row, model_id, 1L, "na_input", "spcd")
      } else if (species_value <= 0 || species_value > .Machine$integer.max ||
                   species_value !=
                     floor(species_value)) {
        add_problem(row, model_id, 7L, "unknown_species", "spcd")
      } else if (length(model$spcd) && !species_value %in% model$spcd) {
        add_problem(row, model_id, 52L, "species_out_of_scope", "spcd")
      }
    }
    declared <- c(model$inputs$required, model$inputs$optional)
    for (input in model$inputs$required) {
      if (is.null(aux_values[[input]])) {
        add_problem(row, model_id, 51L, "missing_input", input)
      }
    }
    for (input in intersect(declared, names(aux_values))) {
      input_value <- aux_values[[input]][[row]]
      if (.auxiliary_missing(input_value)) {
        add_problem(row, model_id, 1L, "na_input", input)
      } else if (.auxiliary_domain_invalid(input_value, .auxiliary_spec(input)$domain[[1L]])) {
        add_problem(row, model_id, 55L, "auxiliary_out_of_domain", input)
      }
    }
    for (pair in model$inputs$pairs) {
      supplied <- vapply(pair, function(input) !is.null(aux_values[[input]]), logical(1))
      if (any(supplied) && !all(supplied)) {
        add_problem(row, model_id, 51L, "missing_input", paste(pair, collapse = "+"))
      }
    }
  }
  if (!length(problems))
    .empty_model_problems() else do.call(rbind, problems)
}

#' Record the equations registered for this analysis
#'
#' @return A data frame with id (model identifier), owner_package (registering package),
#'   package_version (package version), and generation (registry revision number).
#' @usage
#' taper_manifest()
#' @export
#' @examples
#' ## Record the registered models
#' manifest <- taper_manifest()
#'
#' ## Show a small registry selection
#' head(manifest, n = 3)
taper_manifest <- function() {
  entries <- .tv_registry$models
  result <- data.frame(id = names(entries), owner_package = vapply(
    entries, `[[`, character(1),
    "owner_package"
  ), package_version = vapply(
    entries, function(entry) entry$package_version,
    character(1)
  ), generation = vapply(entries, `[[`, integer(1), "generation"), stringsAsFactors = FALSE)
  result[order(result$id), , drop = FALSE]
}
