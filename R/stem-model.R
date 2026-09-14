.tv_class_version <- "0.3"

.nvel_unverified_ids <- c(
  "I15FW2W017",
  "A16CURW042", "A16CURW242", "A16DEMW042", "A16DEMW242",
  "A32CURW042", "A32CURW242", "A32DEMW042", "A32DEMW242",
  "A61CURW042", "A61CURW242", "A61DEMW042", "A61DEMW242"
)

.nvel_oracle_verified <- function(id) {
  !id %in% .nvel_unverified_ids
}

.scalar_character <- function(x, name, choices = NULL, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
        (!allow_empty && !nzchar(x))) {
    stop(name, " must be one non-missing character value.", call. = FALSE)
  }
  if (!is.null(choices) && !x %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}

.callback_formals <- list(
  dib = c("dbh", "ht", "h", "aux"),
  dob = c("dbh", "ht", "h", "aux"),
  height_at_dib = c("dbh", "ht", "dib", "aux"),
  volume = c("dbh", "ht", "lower", "upper", "aux")
)

.validate_callback <- function(callback, name) {
  if (is.null(callback)) {
    return(invisible(NULL))
  }
  if (!is.function(callback)) {
    stop(name, " must be a function or NULL.", call. = FALSE)
  }
  actual <- names(formals(callback))
  expected <- .callback_formals[[name]]
  if (!identical(actual, expected)) {
    stop(name, " must have formals ", paste(expected, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.normalize_species_codes <- function(species) {
  invalid <- !is.numeric(species) || !is.null(dim(species)) ||
    anyNA(species) || any(!is.finite(species)) ||
    any(species <= 0) || any(species != floor(species)) ||
    any(species > .Machine$integer.max)
  if (invalid) {
    stop(
      paste(
        "species must contain positive whole-number FIA species codes.",
        "Supply an integer or double vector within R's integer range."
      ),
      call. = FALSE
    )
  }
  as.integer(species)
}

.validation_state <- function() {
  global_names <- ls(.GlobalEnv, all.names = TRUE)
  list(
    global_names = global_names,
    global_values = if (length(global_names)) {
      mget(global_names, envir = .GlobalEnv, inherits = FALSE)
    } else {
      list()
    },
    options = options(),
    rng_exists = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    rng = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
  )
}

.validation_effects <- function(state) {
  after_names <- ls(.GlobalEnv, all.names = TRUE)
  ordinary_before <- setdiff(state$global_names, ".Random.seed")
  ordinary_after <- setdiff(after_names, ".Random.seed")
  global_changed <- !setequal(ordinary_before, ordinary_after)
  shared <- intersect(ordinary_before, ordinary_after)
  if (!global_changed && length(shared)) {
    global_changed <- any(!vapply(shared, function(name) {
      identical(
        get(name, envir = .GlobalEnv, inherits = FALSE),
        state$global_values[[name]]
      )
    }, logical(1)))
  }
  rng_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  rng_changed <- !identical(rng_exists, state$rng_exists) ||
    (rng_exists && !identical(
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), state$rng
    ))
  list(
    global = global_changed,
    options = !identical(options(), state$options),
    rng = rng_changed
  )
}

.restore_validation_state <- function(state) {
  after_names <- ls(.GlobalEnv, all.names = TRUE)
  added <- setdiff(after_names, state$global_names)
  if (length(added)) {
    rm(list = added, envir = .GlobalEnv)
  }
  if (length(state$global_names)) {
    for (name in state$global_names) {
      current <- if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
        get(name, envir = .GlobalEnv, inherits = FALSE)
      } else {
        NULL
      }
      if (!exists(name, envir = .GlobalEnv, inherits = FALSE) ||
            !identical(current, state$global_values[[name]])) {
        assign(name, state$global_values[[name]], envir = .GlobalEnv)
      }
    }
  }
  added_options <- setdiff(names(options()), names(state$options))
  if (length(added_options)) {
    options(stats::setNames(
      rep(list(NULL), length(added_options)), added_options
    ))
  }
  options(state$options)
  invisible(NULL)
}

.run_validation_probe <- function(callback, arguments) {
  state <- .validation_state()
  warnings <- messages <- character()
  callback_error <- NULL
  generation <- .registry_enter()
  value <- tryCatch(
    withCallingHandlers(
      do.call(callback, arguments),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(error) {
      callback_error <<- conditionMessage(error)
      NULL
    }
  )
  effects <- .validation_effects(state)
  .restore_validation_state(state)
  .registry_exit(generation)
  if (length(warnings)) {
    stop("callback produced a warning during validation: ", warnings[[1L]],
      call. = FALSE
    )
  }
  if (length(messages)) {
    stop("callback produced a message during validation: ", messages[[1L]],
      call. = FALSE
    )
  }
  if (effects$rng) {
    stop("callback changed RNG state during validation.", call. = FALSE)
  }
  if (effects$options) {
    stop("callback changed options during validation.", call. = FALSE)
  }
  if (effects$global) {
    stop("callback wrote to the global environment during validation.", call. = FALSE)
  }
  if (!is.null(callback_error)) {
    stop("callback raised a condition during validation: ", callback_error,
      call. = FALSE
    )
  }
  value
}

.validate_inputs <- function(inputs) {
  if (!is.list(inputs) || !identical(names(inputs), c("required", "optional", "pairs"))) {
    stop("inputs must contain required, optional, and pairs in that order.", call. = FALSE)
  }
  if (!is.character(inputs$required) || !is.character(inputs$optional) ||
        anyNA(inputs$required) || anyNA(inputs$optional) ||
        any(!nzchar(c(inputs$required, inputs$optional)))) {
    stop("required and optional inputs must be non-missing names.", call. = FALSE)
  }
  declared <- c(inputs$required, inputs$optional)
  if (anyDuplicated(declared)) {
    stop("auxiliary input names must be unique.", call. = FALSE)
  }
  unknown <- setdiff(declared, .auxiliary_table$name)
  if (length(unknown)) {
    stop("auxiliary input is not in the centralized table: ", unknown[[1L]],
      call. = FALSE
    )
  }
  if (!is.list(inputs$pairs) ||
        any(!vapply(inputs$pairs, is.character, logical(1))) ||
        any(vapply(inputs$pairs, length, integer(1)) < 2L) ||
        any(!unlist(inputs$pairs, use.names = FALSE) %in% declared)) {
    stop("each input pair must contain at least two declared input names.", call. = FALSE)
  }
  inputs
}

new_stem_model_unchecked <- function(id, family, kernel, dib = NULL, dob = NULL,
                                     height_at_dib = NULL, volume = NULL,
                                     inputs = list(required = character(),
                                                   optional = character(), pairs = list()),
                                     units = "imperial", species = integer(), stump_ht = 1,
                                     bark_ratio = NA_real_, source = "", notes = "",
                                     data = list(), oracle_verified = NA,
                                     class_version = .tv_class_version) {
  structure(
    list(
      id = id,
      family = family,
      kernel = kernel,
      dib = dib,
      dob = dob,
      height_at_dib = height_at_dib,
      volume = volume,
      inputs = inputs,
      units = units,
      species = species,
      stump_ht = stump_ht,
      bark_ratio = bark_ratio,
      source = source,
      notes = notes,
      data = data,
      class_version = class_version
    ),
    class = "taper_model",
    oracle_verified = oracle_verified
  )
}

#' Define a taper equation written as an R function
#'
#' Construct a taper model from R callbacks and declared inputs. Register the returned object
#' with [register_taper_model()].
#'
#' @param id Required single nonempty character name, unitless. Missing is an
#'   error. Example: `'example.linear'`.
#'
#' Registration additionally requires
#'   a distinct name beginning with a letter and containing a dot.
#' @param family Required single nonempty character family name, unitless.
#'   Missing is an error. Example: `'user'`.
#' @param dib Required R function for inside-bark diameter, with the exact
#'   arguments and return type listed in the corresponding section. Omission or missing is an
#'   error.
#' @param dob Optional R function for outside-bark diameter. Default `NULL`
#'   uses a supplied bark ratio when available. Missing is invalid.
#'
#' It has the same argument names as `dib`.
#' @param height_at_dib Optional R function for height at inside-bark diameter. Default `NULL` uses
#' numerical search. Missing is invalid.
#'
#' Its arguments
#'   are `dbh`, `ht`, `dib`, and `aux`.
#' @param volume Optional R function for inside-bark volume. Default `NULL`
#'   uses numerical integration. Missing is invalid.
#'
#' Its arguments are `dbh`,
#'   `ht`, `lower`, `upper`, and `aux`.
#' @param inputs Named list of required and optional equation inputs and
#'   paired names. The default empty vectors and empty pair list require no
#'   extra measurements. Missing is an error.
#'
#' the callback contract defines the shape. Unitless names describe inputs with their own units.
#' @param units One character value, `'imperial'` (default) or `'metric'`,
#'   declaring the equation's measurement system. Missing is an error. Imperial means inches, feet,
#' and cubic feet.
#'
#' Metric means centimeters,
#'   meters, and cubic meters. Example: `units = 'metric'`.
#' @param species Numeric vector of positive whole-number inventory species
#'   codes. Default `integer()` means unrestricted. Missing, nonfinite,
#'   fractional, or out-of-integer-range codes are errors. Unitless.
#'
#' @param stump_ht One finite nonnegative numeric height. Default `1` means
#'   1 foot for an imperial equation or 1 meter for a metric equation.
#'   Missing is an error.
#' @param bark_ratio One numeric inside-bark to outside-bark diameter ratio. Default `NA_real_`
#' means no default ratio. A supplied value must exceed
#'   zero and be at most one.
#'
#' Unitless.
#' @param source,notes Single character source description and equation notes. Both default to
#'   `''`, meaning no text supplied. Missing is an error.
#'
#' Unitless. Example: `source = 'Illustrative equation'`.
#'
#' @details Validation checks object fields and evaluates an illustrative
#'   profile. Passing this check does not establish scientific suitability.
#'   Register the returned model before using its identifier in calculations.
#' @return A validated `taper_model` list with the fields.
#' @section Taper model fields:
#' A `taper_model` is a named list. Fields are supplied by the defining function,
#' not columns of an inventory. `id` is its nonempty character equation name,
#' `family` is its nonempty character equation family, and `units` is either
#' `'imperial'` or `'metric'`.
#'
#' These labels are unitless. `species` is a
#' vector of positive whole-number inventory species codes, with `integer()`
#' meaning unrestricted. Missing codes are errors.
#'
#' `stump_ht` is one finite
#' nonnegative height in the equation's feet or meters. `bark_ratio` is one
#' numeric inside-bark to outside-bark diameter ratio greater than zero and
#' at most one, or `NA` when no default is supplied.
#'
#' `dib`, `dob`, `height_at_dib`, and `volume` contain equation functions or `NULL` when that
#' function is absent. For an equation written in R, `dib` is required.
#'
#' `inputs` is a list with character vectors `required` and `optional`, then a list `pairs`, in
#' that order. For example, `list(required = character(), optional = 'bark_ratio', pairs =
#' list())` declares an optional bark ratio. All declared input names must come from the
#' equation inputs described on [dib()].
#'
#' Names cannot be duplicated. A pair is a character vector such as `c('upper_ht1',
#' 'upper_d1')`.
#'
#' `source` and `notes` are single character strings, default `''` when made by
#' [new_taper_model()]. Empty text is allowed and missing text is an error. `data` holds
#' equation coefficients or other fixed equation data.
#'
#' It cannot be an environment, function, or external pointer. `class_version` is the character
#' representation version, currently `'0.3'`, and must match.
#'
#' The literal field `kernel` describes which equation functions are present. Its `type` is
#' `'r'` or `'compiled'`, `key` identifies a built-in equation or is `NA` for an R function,
#' and logical `has_dob`, `has_inverse`, and `has_integral` identify direct calculations. These
#' flags are not missing.
#'
#' The logical `oracle_verified` attribute records source-test coverage: `TRUE` means checked
#' against source-library test cases, `FALSE` means not exercised, and `NA` means that
#' comparison does not apply. It does not rate prediction accuracy for a stand. There are no
#' data-frame list columns.
#'
#' @section Equation function inputs and returns:
#' Diameter callbacks take `dbh`, `ht`, `h`, and `aux`, in that order. Height callbacks replace `h`
#' with `dib`. Volume callbacks take `dbh`, `ht`, `lower`, `upper`, and `aux`.
#'
#' Measurements arrive as equal-length vectors in the equation's declared units. `aux` is a
#' named list of declared extra input vectors. A function must return a double vector of the
#' same length in diameter, height, or cubic units, respectively.
#'
#' Missing or invalid outputs fail evaluation. An optional integer `status` attribute can
#' retain per-row diagnoses. The functions must not change global objects, options, or random
#' state.
#'
#'
#' @section Status and missing values:
#' This
#' function returns no tree status code. Invalid declarations or failed function checks stop
#' the call. Correct the equation before registering it.
#'
#' Missing optional functions select the fallback only when supplied as `NULL`, not as `NA`.
#' @seealso [register_taper_model()] to make the equation available, [validate_taper_model()]
#' to check a saved object, [dib()] to calculate.
#' @export
#' @usage
#'
#' ## Call signatures
#' new_taper_model(id, family, dib, dob = NULL, height_at_dib = NULL, volume = NULL, inputs =
#'   list(required = character(), optional = character(), pairs = list()), units = 'imperial',
#'   species = integer(), stump_ht = 1, bark_ratio = NA_real_, source = '', notes = '')
#' @examples
#' ## Retrieve the shipped R callback
#' existing <- get_taper_model(id = 'demo.paraboloid.r')
#'
#' ## Create a model using that callback
#' new_taper_model(id = 'example.callback',
#'                 family = 'example',
#'                 dib = existing$dib,
#'                 bark_ratio = existing$bark_ratio)
new_taper_model <- function(id, family, dib, dob = NULL, height_at_dib = NULL,
                            volume = NULL,
                            inputs = list(required = character(),
                                          optional = character(), pairs = list()),
                            units = "imperial", species = integer(), stump_ht = 1,
                            bark_ratio = NA_real_, source = "", notes = "") {
  model <- new_stem_model_unchecked(
    id = id,
    family = family,
    kernel = list(
      type = "r",
      key = NA_character_,
      has_dob = !is.null(dob),
      has_inverse = !is.null(height_at_dib),
      has_integral = !is.null(volume)
    ),
    dib = dib,
    dob = dob,
    height_at_dib = height_at_dib,
    volume = volume,
    inputs = inputs,
    units = units,
    species = species,
    stump_ht = stump_ht,
    bark_ratio = bark_ratio,
    source = source,
    notes = notes,
    data = list()
  )
  validate_taper_model(model)
}

#' Check a taper equation before using it
#'
#' Check model metadata, declared inputs, and callback behavior. Return the validated model
#' invisibly or stop on an invalid definition.
#'
#' @param x Required `taper_model` list with the fields. No default. Missing and other
#'   objects are errors.
#' @return The validated model, invisibly. Assign it to retain normalized species codes.
#' @details R functions are tested on a fixed profile in the
#'   declared units. The check rejects nonfinite diameter results and increasing upper-stem
#'   diameter, and tests optional function returns. It checks a limited example profile, not
#'   the full range of future trees.
#'
#' Built-in equations are checked for availability and
#'   coefficient structure.
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
#' validate_taper_model(x)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Retrieve an equation assigned to the shipped trees
#' model <- get_taper_model(id = first(x = example_trees$model))
#'
#' ## Validate the model object
#' validate_taper_model(x = model)
#' @inheritSection new_taper_model Taper model fields
#' @inheritSection new_taper_model Equation function inputs and returns
validate_taper_model <- function(x) {
  if (!inherits(x, "taper_model")) {
    stop("x must inherit from taper_model.", call. = FALSE)
  }
  expected <- c(
    "id", "family", "kernel", "dib", "dob", "height_at_dib", "volume",
    "inputs", "units", "species", "stump_ht", "bark_ratio", "source",
    "notes", "data", "class_version"
  )
  if (!identical(names(x), expected)) {
    stop("taper_model fields do not match class version 0.3.", call. = FALSE)
  }
  .scalar_character(x$id, "id")
  .scalar_character(x$family, "family")
  .scalar_character(x$units, "units", c("imperial", "metric"))
  .scalar_character(x$source, "source", allow_empty = TRUE)
  .scalar_character(x$notes, "notes", allow_empty = TRUE)
  oracle_verified <- attr(x, "oracle_verified", exact = TRUE)
  if (!is.logical(oracle_verified) || length(oracle_verified) != 1L) {
    stop("oracle_verified must be TRUE, FALSE, or NA.", call. = FALSE)
  }
  if (!identical(x$class_version, .tv_class_version)) {
    stop("unsupported taper_model class_version.", call. = FALSE)
  }
  kernel_names <- c("type", "key", "has_dob", "has_inverse", "has_integral")
  if (!is.list(x$kernel) || !identical(names(x$kernel), kernel_names) ||
        !is.character(x$kernel$type) || length(x$kernel$type) != 1L ||
        is.na(x$kernel$type) || !x$kernel$type %in% c("r", "compiled")) {
    stop("kernel metadata is invalid.", call. = FALSE)
  }
  capability_names <- c("has_dob", "has_inverse", "has_integral")
  valid_capabilities <- vapply(x$kernel[capability_names], function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }, logical(1))
  if (any(!valid_capabilities)) {
    stop("kernel metadata is invalid.", call. = FALSE)
  }
  .validate_inputs(x$inputs)
  x$species <- .normalize_species_codes(x$species)
  if (!is.numeric(x$stump_ht) || length(x$stump_ht) != 1L ||
        !is.finite(x$stump_ht) || x$stump_ht < 0) {
    stop("stump_ht must be one finite non-negative number.", call. = FALSE)
  }
  if (!is.numeric(x$bark_ratio) || length(x$bark_ratio) != 1L ||
        (!is.na(x$bark_ratio) &&
           (!is.finite(x$bark_ratio) || x$bark_ratio <= 0 || x$bark_ratio > 1))) {
    stop("bark_ratio must be NA or one number in (0, 1].", call. = FALSE)
  }
  if (is.environment(x$data) || is.function(x$data) ||
        typeof(x$data) == "externalptr") {
    stop("data must contain immutable model data.", call. = FALSE)
  }
  nvel_family <- grepl(
    "nvel|nsvb|^flewelling_|^clark_|_taper$|^r4_driver$", x$family,
    ignore.case = TRUE
  )
  if (grepl("^[[:alnum:]]{10}$", x$id) && !nvel_family) {
    warning("id resembles an NVEL equation identifier but family is not NVEL.",
      call. = FALSE
    )
  }

  if (identical(x$kernel$type, "compiled")) {
    .scalar_character(x$kernel$key, "compiled kernel key")
    if (!tv_cpp_kernel_exists(x$kernel$key)) {
      stop("compiled kernel is not present in the C++ registry.", call. = FALSE)
    }
    if (startsWith(x$kernel$key, "published:")) {
      if (!identical(x$kernel$key, paste0("published:", x$family))) {
        stop("published kernel key and family must identify the same form.",
          call. = FALSE
        )
      }
      .validate_published_taper_data(x$family, x$data)
    }
    return(invisible(x))
  }

  if (!is.character(x$kernel$key) || length(x$kernel$key) != 1L ||
        !is.na(x$kernel$key)) {
    stop("an R kernel key must be NA.", call. = FALSE)
  }

  for (name in names(.callback_formals)) {
    .validate_callback(x[[name]], name)
  }
  if (is.null(x$dib)) {
    stop("an R kernel must provide dib.", call. = FALSE)
  }
  expected_capabilities <- c(
    has_dob = !is.null(x$dob),
    has_inverse = !is.null(x$height_at_dib),
    has_integral = !is.null(x$volume)
  )
  actual_capabilities <- unlist(
    x$kernel[names(expected_capabilities)],
    use.names = TRUE
  )
  if (!identical(actual_capabilities, expected_capabilities)) {
    stop("R kernel capability flags do not match its callbacks.", call. = FALSE)
  }

  probe_h <- seq(max(x$stump_ht, 0), 80, length.out = 17L)
  probe_aux <- stats::setNames(
    lapply(c(x$inputs$required, x$inputs$optional), function(name) {
      if (identical(name, "upper_bark")) rep("ob", 17L) else rep(1, 17L)
    }),
    c(x$inputs$required, x$inputs$optional)
  )
  run_probe <- function(callback, ...) {
    .run_validation_probe(callback, list(...))
  }
  profile <- run_probe(x$dib, rep(10, 17L), rep(80, 17L), probe_h, probe_aux)
  if (!is.double(profile) || length(profile) != 17L ||
        any(!is.finite(profile)) || any(profile < 0)) {
    stop("dib must return finite non-negative doubles on the probe grid.", call. = FALSE)
  }
  upper_profile <- profile[seq.int(5L, length(profile))]
  if (any(diff(upper_profile) > sqrt(.Machine$double.eps))) {
    stop("dib is not monotone enough above the lower-stem region.", call. = FALSE)
  }
  if (!is.null(x$dob)) {
    outside <- run_probe(x$dob, rep(10, 17L), rep(80, 17L), probe_h, probe_aux)
    if (!is.double(outside) || length(outside) != 17L || any(!is.finite(outside))) {
      stop("dob must return finite doubles on the probe grid.", call. = FALSE)
    }
  }
  if (!is.null(x$height_at_dib)) {
    heights <- run_probe(
      x$height_at_dib, rep(10, 17L), rep(80, 17L), profile, probe_aux
    )
    if (!is.double(heights) || length(heights) != 17L || any(!is.finite(heights))) {
      stop("height_at_dib must return finite doubles on the probe grid.", call. = FALSE)
    }
  }
  if (!is.null(x$volume)) {
    volumes <- run_probe(
      x$volume, rep(10, 17L), rep(80, 17L), rep(x$stump_ht, 17L),
      rep(70, 17L), probe_aux
    )
    if (!is.double(volumes) || length(volumes) != 17L || any(!is.finite(volumes))) {
      stop("volume must return finite doubles on the probe grid.", call. = FALSE)
    }
  }
  invisible(x)
}

#' @export
format.taper_model <- function(x, ...) {
  capabilities <- c(
    dob = x$kernel$has_dob || is.finite(x$bark_ratio),
    inverse = x$kernel$has_inverse,
    integral = x$kernel$has_integral
  )
  enabled <- names(capabilities)[capabilities]
  oracle_verified <- attr(x, "oracle_verified", exact = TRUE)
  oracle_label <- if (isTRUE(oracle_verified)) {
    "yes"
  } else if (identical(oracle_verified, FALSE)) {
    "no"
  } else {
    "not applicable"
  }
  paste0(
    "<taper_model ", x$id, ">\n",
    "  family: ", x$family, "\n",
    "  kernel: ", x$kernel$type, "\n",
    "  units: ", x$units, "\n",
    "  oracle verified: ", oracle_label, "\n",
    "  capabilities: ", if (length(enabled)) paste(enabled, collapse = ", ") else "dib"
  )
}

#' @export
print.taper_model <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
