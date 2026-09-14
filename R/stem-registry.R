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
    "^",
    paste0(ifelse(characters %in% c("?", "*"), ".", characters), collapse = ""),
    "$"
  )
}

.nvel_identifier_row <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
        nchar(id) != 10L || grepl("[?*]", id)) return(NULL)
  catalog <- .nvel_reference("nvel_identifier_catalog.csv")
  literal <- !grepl("[?*]", catalog$voleq)
  exact <- which(literal & catalog$voleq == id)
  if (length(exact)) return(catalog[exact[[1L]], , drop = FALSE])

  patterns <- which(!literal)
  matched <- patterns[vapply(catalog$voleq[patterns], function(pattern) {
    grepl(.nvel_identifier_pattern(pattern), id)
  }, logical(1))]
  if (!length(matched)) return(NULL)
  # Match the most specific inventory rule. This mirrors the dispatch gates:
  # P01DEM0*** is a direct-volume equation even though its middle mnemonic
  # also matches the broader ???DEM???? profile rule, while ???F33???? wins
  # over the Flewelling umbrella ???F?????? rule.
  wildcards <- nchar(gsub("[^?*]", "", catalog$voleq[matched]))
  catalog[matched[which.min(wildcards)], , drop = FALSE]
}

.nvel_family_model <- function(id, family) {
  species <- suppressWarnings(as.integer(substr(id, 8L, 10L)))
  model <- switch(family,
    flewelling_2pt = .flewelling_model(id, family, species),
    flewelling_3pt = .flewelling_model(id, family, species),
    clark_r8 = .clark_model(id, family, species),
    clark_r9 = .clark_model(id, family, species),
    r10_taper = .r10r4_model(id, family, species),
    r4_driver = .r10r4_model(id, family, species),
    r1_taper = .smalltaper_model(id, family, species),
    r2_taper = .smalltaper_model(id, family, species),
    r5_taper = .smalltaper_model(id, family, species),
    r12_taper = .smalltaper_model(id, family, species),
    blm_taper = .smalltaper_model(id, family, species),
    behre_taper = .smalltaper_model(id, family, species),
    nsvb = .nsvb_model(id, species),
    NULL
  )
  if (is.null(model) || !tv_cpp_kernel_exists(model$kernel$key)) return(NULL)
  model
}

.nvel_identifier_resolution <- function(id) {
  row <- .nvel_identifier_row(id)
  if (is.null(row) && is.character(id) && length(id) == 1L && !is.na(id) &&
        grepl("^NVB[0M][0-9]{6}P$", id)) {
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
  list(
    model = model,
    owner_package = "merchandiser",
    package_version = tryCatch(
      as.character(utils::packageVersion("merchandiser")),
      error = function(error) NA_character_
    ),
    generation = .tv_registry$generation,
    sealed = TRUE
  )
}

.registry_entry <- function(model, owner_package, sealed) {
  .tv_registry$generation <- .tv_registry$generation + 1L
  package_version <- tryCatch(
    as.character(utils::packageVersion(owner_package)),
    error = function(error) NA_character_
  )
  list(
    model = model,
    owner_package = owner_package,
    package_version = package_version,
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
  model <- validate_taper_model(model)
  if (!is.null(.tv_registry$models[[model$id]])) {
    stop("model id is already registered: ", model$id, call. = FALSE)
  }
  entry <- .registry_entry(model, "merchandiser", TRUE)
  .tv_registry$models[[model$id]] <- entry
  invisible(model)
}

#' Make a local taper equation available for this analysis
#'
#' Register a local taper model for the current session. Package registrations are protected.
#'
#' @param model Required `taper_model` list with the fields. No default. Missing and
#'   other objects are errors.
#' @return The validated model, invisibly. Registration changes equation availability for
#'   this session.
#' @details A local name must begin with a letter, contain a dot, and use letters, digits,
#'   underscores, periods, or hyphens in the permitted parts. For example, `client.name` is
#'   valid. Existing names cannot be replaced.
#'
#' Save the model separately and register it
#'   again in a new session. Registration is prohibited during an active equation
#'   calculation.
#' @section Status and missing values:
#' No tree status codes are returned. Invalid inputs stop the call. Repair
#' the model or identifier before continuing.
#'
#' A successful check or lookup
#' does not establish tree volume.
#' @seealso [new_taper_model()] to define an equation, [has_taper_model()] to
#'   check availability, [dib()] to calculate diameter.
#' @export
#' @usage
#'
#' ## Call signatures
#' register_taper_model(model)
#' @examples
#' ## Fit a model to the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements, form = 'max_burkhart', units = 'imperial')
#'
#' ## Register fitted coefficients for the session
#' register_taper_model(model = as_taper_model(x = fit, id = 'example.registered'))
#'
#' ## Check the session registration
#' has_taper_model(id = 'example.registered')
#'
#' ## Remove the session registration
#' unregister_taper_model(id = 'example.registered')
#' @inheritSection new_taper_model Taper model fields
#' @inheritSection new_taper_model Equation function inputs and returns
register_taper_model <- function(model) {
  .check_registry_mutable()
  model <- validate_taper_model(model)
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
#' Remove a local model registration from the current session.
#'
#' @param id Required single nonempty character identifier of a registered local equation. No
#'   default. Missing or unknown identifiers are errors.
#'
#' Unitless. Example:
#'   `'example.remove'`.
#' @return Logical `TRUE`, invisibly, after successful removal. Unitless.
#' @details Removal affects only this session. Saved result plots or price comparisons can
#'   need the removed model again. Register the saved object before rerunning those
#'   calculations.
#'
#' Removal is prohibited while equation evaluation is active.
#' @section Status and missing values:
#' No tree status codes are returned. Invalid inputs stop the call. Repair
#' the model or identifier before continuing.
#'
#' A successful check or lookup
#' does not establish tree volume.
#' @seealso [new_taper_model()] to define an equation, [has_taper_model()] to
#'   check availability, [dib()] to calculate diameter.
#' @export
#' @usage
#'
#' ## Call signatures
#' unregister_taper_model(id)
#' @examples
#' ## Fit a model to the shipped stem measurements
#' fit <- fit_taper(data = example_stem_measurements, form = 'max_burkhart', units = 'imperial')
#'
#' ## Register fitted coefficients for the session
#' register_taper_model(model = as_taper_model(x = fit, id = 'example.registered'))
#'
#' ## Check the session registration
#' has_taper_model(id = 'example.registered')
#'
#' ## Remove the session registration
#' unregister_taper_model(id = 'example.registered')
unregister_taper_model <- function(id) {
  .check_registry_mutable()
  id <- .scalar_character(id, "id")
  entry <- .tv_registry$models[[id]]
  if (is.null(entry)) {
    stop("unknown model id: ", id, call. = FALSE)
  }
  if (isTRUE(entry$sealed)) {
    stop("built-in models cannot be unregistered: ", id, call. = FALSE)
  }
  .tv_registry$models[[id]] <- NULL
  .tv_registry$generation <- .tv_registry$generation + 1L
  invisible(TRUE)
}

#' Inspect a taper equation and its required inputs
#'
#' Return a resolved taper model, including its inputs, units, capabilities, and source record.
#'
#' @param id Required single nonempty character identifier. No default. Missing or unknown
#'   values stop the call.
#'
#' Unitless.
#' @return A `taper_model` list with the fields.
#' @details The source-test attribute describes which identifiers were exercised against
#'   saved reference results. It is not a rating of regional prediction accuracy. Inspect the
#'   required inputs and bark conventions before applying an equation to tree measurements.
#' @section Status and missing values:
#' No tree status codes are returned. Invalid inputs stop the call. Repair
#' the model or identifier before continuing.
#'
#' A successful check or lookup
#' does not establish tree volume.
#' @seealso [new_taper_model()] to define an equation, [has_taper_model()] to
#'   check availability, [dib()] to calculate diameter.
#' @export
#' @usage
#'
#' ## Call signatures
#' get_taper_model(id)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the equation assigned to an example tree
#' get_taper_model(id = first(x = example_trees$model))
#' @inheritSection new_taper_model Taper model fields
#' @inheritSection new_taper_model Equation function inputs and returns
get_taper_model <- function(id) {
  id <- .scalar_character(id, "id")
  entry <- .registry_lookup(id)
  if (is.null(entry)) {
    resolution <- .nvel_identifier_resolution(id)
    stop(resolution$reason, ": ", id, call. = FALSE)
  }
  entry$model
}

#' Check whether taper equations are available
#'
#' Return a logical vector indicating whether each equation identifier resolves.
#'
#' @param id Required character vector of any length, including zero. Unitless, with no default.
#'   Omission or a noncharacter value is an error.
#'
#' Example: `example_trees$model`. Missing identifiers return `FALSE`.
#' @return A logical vector in input order, without names added. `TRUE`
#'   indicates a supported equation can be found. All values are unitless.
#' @details Some supported source identifiers are resolved when requested
#'   and need not occur in the list returned by [taper_models()].
#' @section Status and missing values:
#' No codes or warnings are generated for unknown identifiers. `FALSE`
#' means unavailable, not zero volume. Check the spelling or register a
#' local equation before calculation.
#' @seealso [check_taper_models()] to diagnose input problems,
#'   [get_taper_model()] to inspect an available equation.
#' @export
#' @usage
#'
#' ## Call signatures
#' has_taper_model(id)
#' @examples
#' ## Check availability of the inventory equations
#' has_taper_model(id = example_trees$model)
has_taper_model <- function(id) {
  if (!is.character(id)) {
    stop("id must be a character vector.", call. = FALSE)
  }
  valid <- !is.na(id) & nzchar(id)
  result <- rep(FALSE, length(id))
  result[valid] <- vapply(id[valid], function(value) {
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
    id = character(), family = character(), kernel = character(),
    species_scope = character(), has_dob = logical(), has_inverse = logical(),
    has_integral = logical(), oracle_verified = logical(), units = character(),
    stump_ht = double(), owner_package = character(), source = character(),
    generation = integer(),
    stringsAsFactors = FALSE
  )
}

#' Find available taper equations by family or species
#'
#' List registered taper models, optionally filtered by family or species.
#'
#' @param family Optional character vector of family names. Default `NULL`
#'   includes all families. Missing names are errors.
#'
#' Unknown names match no
#'   family. Unitless. Example: `family = 'demo'`.
#' @param species Optional numeric vector of positive whole-number inventory
#'   species codes within R's integer range. Default `NULL` includes all
#'   species. Missing or invalid codes are errors.
#'
#' Unitless.
#' @return A data frame with one row per matching registered equation, sorted
#'   by `id`, and the columns. No matches return zero rows.
#' @section Equation list fields:
#' All fields have one value per model and are supplied by the package. `id`, `family`, `kernel`,
#' `species_scope`, `units`, `owner_package`, and
#' `source` are character. They give the equation name, family, R or compiled
#' calculation, comma-separated species codes or `'unrestricted'`, declared
#' units, registering package, and source text, respectively.
#'
#' Labels are
#' unitless. An empty source means no citation was supplied.
#'
#' Logical `has_dob` says a direct outside-bark equation or default bark ratio is available.
#' Logical `has_inverse` and `has_integral` indicate direct height and volume functions.
#' `FALSE` for these does not rule out numerical search or integration.
#'
#' `oracle_verified` is logical: `TRUE` means checked against source-library test cases,
#' `FALSE` means not exercised, and `NA` means that comparison does not apply. It does not rate
#' forestry accuracy. `stump_ht` is numeric height in the model's feet or meters.
#'
#' Integer `generation` records the session registration sequence, not a model revision.
#' Example equation `F00FW2W202` uses imperial units. There are no list columns.
#' @details
#' Family and species filters are both applied.
#'
#' This list does not enumerate every supported source identifier that can be resolved on
#' demand.
#' @section Status and missing values:
#' No tree calculation codes are returned. A zero-
#' row result means no listed model matched the filters. It does not establish whether an
#' unlisted source identifier is supported.
#'
#' Use [has_taper_model()] to check that name.
#' @seealso [get_taper_model()] to review inputs,
#' [check_taper_models()] to check a tree-to-equation assignment, [taper_manifest()] to save a
#' record.
#' @export
#' @usage
#'
#' ## Call signatures
#' taper_models(family = NULL, species = NULL)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Find equations for species in the shipped tree list
#' taper_models(species = example_trees$spcd) %>%
#'   select(id, family, has_dob, has_integral) %>%
#'   slice_head(n = 3)
taper_models <- function(family = NULL, species = NULL) {
  if (!is.null(family) && (!is.character(family) || anyNA(family))) {
    stop("family must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.null(species) &&
        (!is.numeric(species) || any(!is.finite(species)) || any(species <= 0) ||
           any(species > .Machine$integer.max) || any(species != floor(species)))) {
    stop("species must be NULL or a positive integer-valued vector.", call. = FALSE)
  }
  entries <- .tv_registry$models
  if (!length(entries)) {
    return(.empty_models())
  }
  keep <- vapply(entries, function(entry) {
    model <- entry$model
    family_ok <- is.null(family) || model$family %in% family
    species_ok <- is.null(species) || !length(model$species) ||
      any(model$species %in% species)
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
      id = model$id,
      family = model$family,
      kernel = model$kernel$type,
      species_scope = if (length(model$species)) {
        paste(model$species, collapse = ",")
      } else {
        "unrestricted"
      },
      has_dob = unname(capabilities[["has_dob"]]),
      has_inverse = unname(capabilities[["has_inverse"]]),
      has_integral = unname(capabilities[["has_integral"]]),
      oracle_verified = attr(model, "oracle_verified", exact = TRUE),
      units = model$units,
      stump_ht = model$stump_ht,
      owner_package = entry$owner_package,
      source = model$source,
      generation = entry$generation,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(result$id), , drop = FALSE]
}

#' Check which diameter and volume calculations an equation supplies
#'
#' Return flags for outside-bark diameter, inverse height, and integral capabilities.
#'
#' @param id Required character vector, of any length including zero. No default. Omission and
#'   other types are errors.
#'
#' Missing, empty, or
#'   unknown names produce missing flags. Unitless. Example: `example_trees$model`.
#' @return A data frame with character `id` in input order and logical
#'   `has_dob`, `has_inverse`, and `has_integral`. All columns are unitless. `has_dob` means a
#' direct outside-bark equation or default bark ratio is
#'   present.
#'
#' `has_inverse` means a direct height function is present. `has_integral` means a direct volume
#' function is present. Flags for
#'   unresolved identifiers are `NA`.
#' @details A false inverse or integral flag permits numerical calculation
#'   where supported. A false outside-bark flag can be resolved by supplying
#'   a bark ratio to a model that accepts it. These flags do not describe
#'   equation suitability for a species or stand.
#' @section Status and missing values:
#' No tree codes are returned. Missing flags require checking the identifier
#' before calculation. False flags mean an absent direct calculation, not a
#' numeric zero result.
#' @seealso [has_taper_model()] for availability alone, [get_taper_model()]
#'   for input requirements, [stem_volume()] to calculate volume.
#' @export
#' @usage
#'
#' ## Call signatures
#' model_capabilities(id)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect capabilities of the stored inventory equations
#' model_capabilities(id = example_trees$model) %>%
#'   distinct()
model_capabilities <- function(id) {
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
    id = id,
    has_dob = as.logical(matrix_values[, 1L]),
    has_inverse = as.logical(matrix_values[, 2L]),
    has_integral = as.logical(matrix_values[, 3L]),
    stringsAsFactors = FALSE
  )
}

.mapping_common_size <- function(values) {
  lengths <- vapply(values, length, integer(1))
  if (any(lengths == 0L)) {
    if (all(lengths %in% c(0L, 1L))) {
      return(0L)
    }
    stop("only size-one vectors can be recycled.", call. = FALSE)
  }
  sizes <- unique(lengths[lengths != 1L])
  if (length(sizes) > 1L) {
    stop("only size-one vectors can be recycled.", call. = FALSE)
  }
  if (length(sizes)) sizes else 1L
}

.recycle_mapping <- function(x, size) {
  if (length(x) == size) x else rep(x, size)
}

.empty_model_problems <- function() {
  data.frame(
    row = integer(), id = character(), code = integer(), problem = character(),
    input = character(), stringsAsFactors = FALSE
  )
}

#' Check equation assignments and required tree measurements
#'
#' Return diagnoses for unresolved identifiers, species scope, and required auxiliary inputs.
#'
#' @param id Required character vector or factor of equation names. Length
#'   one repeats, otherwise one per tree. Missing gives `na_input`.
#'
#' Unitless. Example: `example_trees$model`. Omission is an error.
#' @param spcd Numeric inventory species codes, length one or one per tree. Default `NULL` skips
#'   the scope check. Missing gives `na_input`.
#'
#' Positive
#'   whole numbers within integer range are accepted. Unitless.
#' @param aux Named list of equation inputs listed in the corresponding section. Default `NULL`
#'   supplies none. Each vector has length one or one per tree.
#'    Missing supplied values give `na_input`.
#'
#' @return A data frame of mapping problems, with zero rows when the mapping
#'   is clean.
#' @export
#' @usage
#'
#' ## Call signatures
#' check_taper_models(id, spcd = NULL, aux = NULL)
#' @examples
#' ## Check equations and species before calculation
#' check_taper_models(id = example_trees$model, spcd = example_trees$spcd)
#' @section Returned problems:
#' Integer `row` identifies the input row, and character `id` retains the equation name. Integer
#' `code` and character `problem` identify the diagnosis. Character `input` identifies affected
#' fields.
#'
#' All are unitless. Empty `input` means a
#' general equation problem.
#'
#' Several rows can describe the same input tree.
#' @section Status and missing values:
#' `na_input`
#' means a missing identifier, species, or supplied equation input. `unknown_species` means an
#' invalid
#' species code. `unknown_model` means an unavailable equation.
#'
#' `missing_input` means an omitted required input or incomplete pair. `species_out_of_scope` means
#' a species outside
#' the equation's declared scope.
#'
#' `species_out_of_scope` permits a value later, but review suitability. No rows means these checks
#' passed,
#' not that volume is known. Invalid field types, out-of-domain equation inputs, and names
#' undeclared by every selected model stop the call instead of returning problem rows.
#' @inheritSection dib Equation inputs through dots
#' @seealso [has_taper_model()] for
#' availability alone, [get_taper_model()] for equation details, [merchandise()] for log
#' selection.
check_taper_models <- function(id, spcd = NULL, aux = NULL) {
  if (!is.character(id) && !is.factor(id)) {
    stop("id must be a character vector or factor.", call. = FALSE)
  }
  if (!is.null(spcd) && !is.numeric(spcd)) {
    stop("spcd must be NULL or numeric.", call. = FALSE)
  }
  if (is.null(aux)) {
    aux <- list()
  }
  aux <- .capture_aux(aux)
  values <- c(list(id = id), if (!is.null(spcd)) list(spcd = spcd), aux)
  size <- .mapping_common_size(values)
  if (!size) {
    return(.empty_model_problems())
  }
  values <- Map(function(value, name) {
    if (identical(name, "id") &&
          (length(value) == 1L || is.factor(value))) {
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
      row = row, id = model_id, code = code, problem = problem, input = input,
      stringsAsFactors = FALSE
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
      add_problem(row, model_id, 1L, "na_input", "id")
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
                   species_value != floor(species_value)) {
        add_problem(row, model_id, 7L, "unknown_species", "spcd")
      } else if (length(model$species) &&
                   !species_value %in% model$species) {
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
      }
    }
    for (pair in model$inputs$pairs) {
      supplied <- vapply(pair, function(input) !is.null(aux_values[[input]]), logical(1))
      if (any(supplied) && !all(supplied)) {
        add_problem(row, model_id, 51L, "missing_input", paste(pair, collapse = "+"))
      }
    }
  }
  if (!length(problems)) .empty_model_problems() else do.call(rbind, problems)
}

#' Record the equations registered for this analysis
#'
#' Return registered equation identifiers, owning packages, versions, and registration order.
#'
#' @return A data frame sorted by equation identifier, with the fields.
#' @section Equation record fields:
#' `id`, `owner_package`, and `package_version` are character fields naming
#' the equation, registering package or local name prefix, and installed
#' package version. Integer `generation` records the session registration
#' sequence. All fields are unitless, with one value per registered equation.
#'
#' A local equation can have missing `package_version` because its prefix
#' is not an installed package. Fields are generated, not user defaults. There are no list columns.
#'
#' Save the complete table, preserving column order.
#'
#' @details This record includes explicitly registered equations. Supported
#' identifiers resolved on demand may not appear. Save local equation objects and input data
#' separately to reproduce an analysis.
#' @section Status and missing values:
#' No calculation
#' codes are returned. Missing package versions are retained for local equations.
#'
#' They do not indicate failed trees.
#' @seealso [check_taper_manifest()] to compare a saved
#' record, [assumptions()] for choices used in a result.
#' @export
#' @usage
#'
#' ## Call signatures
#' taper_manifest()
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the current registration record
#' taper_manifest() %>%
#'   slice_head(n = 3)
taper_manifest <- function() {
  entries <- .tv_registry$models
  result <- data.frame(
    id = names(entries),
    owner_package = vapply(entries, `[[`, character(1), "owner_package"),
    package_version = vapply(entries, function(entry) entry$package_version, character(1)),
    generation = vapply(entries, `[[`, integer(1), "generation"),
    stringsAsFactors = FALSE
  )
  result[order(result$id), , drop = FALSE]
}

#' Check whether the registered equations match a saved record
#'
#' Compare a saved registration record with the current registry. Return added, missing, or
#' changed registrations.
#'
#' @param manifest Required data frame previously returned by
#'   [taper_manifest()], with the fields in exactly that order. No default. Missing objects
#' or incorrect columns are errors.
#'
#' Example: `saved` below. Preserve missing package versions in the record.
#' @return A data frame with character `id` naming the affected equation and
#'   character `problem`, one of `'added'`, `'missing'`, or `'changed'`.
#'   Both fields are unitless. Matching records produce zero rows.
#' @section Equation record fields:
#' `id`, `owner_package`, and `package_version` are character fields naming
#' the equation, registering package or local name prefix, and installed
#' package version. Integer `generation` records the session registration
#' sequence. All fields are unitless, with one value per registered equation.
#'
#' A local equation can have missing `package_version` because its prefix
#' is not an installed package. Fields are generated, not user defaults. There are no list columns.
#'
#' Save the complete table, preserving column order.
#'
#' @details Comparison uses identifier, ownership, package version, and session
#' registration sequence. It does not compare coefficients or source data. Re-registering an
#' identical model can change the recorded sequence.
#' @section Status and missing values:
#' No
#' tree codes are returned.
#'
#' Added equations were absent from the saved record, missing equations are unavailable now,
#' and changed equations have one or more different recorded fields. Review each change before
#' claiming reproducibility. Matching missing package versions do not compare model contents.
#' @seealso [taper_manifest()] to save a record, [register_taper_model()] to restore a local
#' equation from its separately saved object.
#' @export
#' @usage
#'
#' ## Call signatures
#' check_taper_manifest(manifest)
#' @examples
#' ## Save the equation registration record
#' manifest <- taper_manifest()
#'
#' ## Compare the current registry with the saved record
#' check_taper_manifest(manifest = manifest)
check_taper_manifest <- function(manifest) {
  required <- c("id", "owner_package", "package_version", "generation")
  if (!is.data.frame(manifest) || !identical(names(manifest), required)) {
    stop("manifest must be returned by taper_manifest().", call. = FALSE)
  }
  current <- taper_manifest()
  ids <- union(manifest$id, current$id)
  rows <- lapply(ids, function(id) {
    old <- manifest[manifest$id == id, , drop = FALSE]
    new <- current[current$id == id, , drop = FALSE]
    if (!nrow(old)) {
      return(data.frame(id = id, problem = "added", stringsAsFactors = FALSE))
    }
    if (!nrow(new)) {
      return(data.frame(id = id, problem = "missing", stringsAsFactors = FALSE))
    }
    old_values <- unname(as.character(old[1L, required]))
    new_values <- unname(as.character(new[1L, required]))
    if (!identical(old_values, new_values)) {
      return(data.frame(id = id, problem = "changed", stringsAsFactors = FALSE))
    }
    NULL
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    data.frame(id = character(), problem = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
}
