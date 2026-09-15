.tv_class_version <- "0.5"

.nvel_unverified_ids <- c(
  "I15FW2W017", "A16CURW042", "A16CURW242", "A16DEMW042", "A16DEMW242",
  "A32CURW042", "A32CURW242", "A32DEMW042", "A32DEMW242", "A61CURW042",
  "A61CURW242", "A61DEMW042",
  "A61DEMW242"
)

.nvel_oracle_verified <- function(id) {
  !id %in% .nvel_unverified_ids
}

.scalar_character <- function(x, name, choices = NULL, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || (!allow_empty && !nzchar(x))) {
    stop(name, " must be one non-missing character value.", call. = FALSE)
  }
  if (!is.null(choices) && !x %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "), ".", call. = FALSE)
  }
  x
}

.callback_formals <- list(
  dib = c("dbh", "ht", "h", "aux"), dob = c("dbh", "ht", "h", "aux"),
  height_at_dib = c("dbh", "ht", "dib", "aux"), volume = c(
    "dbh", "ht", "lower",
    "upper", "aux"
  )
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
    stop(name, " must have formals ", paste(expected, collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

.normalize_species_codes <- function(species) {
  invalid <- !is.numeric(species) || !is.null(dim(species)) || anyNA(species) ||
    any(!is.finite(species)) ||
    any(species <= 0) || any(species != floor(species)) || any(
    species > .Machine$integer.max
  )
  if (invalid) {
    stop(
      paste(
        "spcd must contain positive whole-number FIA species codes.",
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
    global_names = global_names, global_values = if (length(global_names)) {
      mget(global_names, envir = .GlobalEnv, inherits = FALSE)
    } else {
      list()
    }, options = options(), rng_exists = exists(".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    ),
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
    global = global_changed, options = !identical(options(), state$options),
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
      if (!exists(name, envir = .GlobalEnv, inherits = FALSE) || !identical(
        current,
        state$global_values[[name]]
      )) {
        assign(name, state$global_values[[name]], envir = .GlobalEnv)
      }
    }
  }
  added_options <- setdiff(names(options()), names(state$options))
  if (length(added_options)) {
    options(stats::setNames(rep(list(NULL), length(added_options)), added_options))
  }
  options(state$options)
  invisible(NULL)
}

.run_validation_probe <- function(callback, arguments) {
  state <- .validation_state()
  warnings <- messages <- character()
  callback_error <- NULL
  generation <- .registry_enter()
  value <- tryCatch(withCallingHandlers(do.call(callback, arguments),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }, message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  ), error = function(error) {
    callback_error <<- conditionMessage(error)
    NULL
  })
  effects <- .validation_effects(state)
  .restore_validation_state(state)
  .registry_exit(generation)
  if (length(warnings)) {
    stop("callback produced a warning during validation: ", warnings[[1L]], call. = FALSE)
  }
  if (length(messages)) {
    stop("callback produced a message during validation: ", messages[[1L]], call. = FALSE)
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
    stop("callback raised a condition during validation: ", callback_error, call. = FALSE)
  }
  value
}

.validate_inputs <- function(inputs) {
  if (!is.list(inputs) || !identical(names(inputs), c("required", "optional", "pairs"))) {
    stop("inputs must contain required, optional, and pairs in that order.", call. = FALSE)
  }
  if (!is.character(inputs$required) || !is.character(inputs$optional) || anyNA(
    inputs$required
  ) ||
    anyNA(inputs$optional) || any(!nzchar(c(inputs$required, inputs$optional)))) {
    stop("required and optional inputs must be non-missing names.", call. = FALSE)
  }
  declared <- c(inputs$required, inputs$optional)
  if (anyDuplicated(declared)) {
    stop("auxiliary input names must be unique.", call. = FALSE)
  }
  unknown <- setdiff(declared, .auxiliary_table$name)
  if (length(unknown)) {
    stop("auxiliary input is not in the centralized table: ", unknown[[1L]], call. = FALSE)
  }
  if (!is.list(inputs$pairs) || any(!vapply(inputs$pairs, is.character, logical(
    1
  ))) || any(vapply(
    inputs$pairs,
    length, integer(1)
  ) < 2L) || any(!unlist(inputs$pairs, use.names = FALSE) %in% declared)) {
    stop("each input pair must contain at least two declared input names.", call. = FALSE)
  }
  inputs
}

new_stem_model_unchecked <- function(
  id, form, kernel, dib = NULL, dob = NULL, height_at_dib = NULL,
  volume = NULL, inputs = list(
    required = character(), optional = character(),
    pairs = list()
  ),
  measurement_system = "imperial",
  spcd = integer(), stump_ht = 1, bark_ratio = NA_real_, source = "",
  notes = "", data = list(), oracle_verified = NA, class_version = .tv_class_version
) {
  structure(
    list(
      id = id, form = form, kernel = kernel, dib = dib, dob = dob,
      height_at_dib = height_at_dib,
      volume = volume,
      inputs = inputs, measurement_system = measurement_system, spcd = spcd,
      stump_ht = stump_ht,
      bark_ratio = bark_ratio, source = source, notes = notes, data = data,
      class_version = class_version
    ),
    class = "taper_model", oracle_verified = oracle_verified
  )
}

#' Define a taper equation written as an R function
#'
#' @param id The identifier names the taper model in the registry. Character scalar model
#'   identifier. Required, with no default.
#' @param form Equation form used by the taper model. Character scalar. Required, with no default.
#' @param dib The callback computes inside diameter along the stem. R function accepting dbh, ht,
#'   measurement arguments, and aux, with diameters in inches, heights in feet, and volumes in
#'   cubic feet. Required, with no default.
#' @param dob The callback computes outside diameter along the stem. R function accepting dbh, ht,
#'   measurement arguments, and aux, with diameters in inches, heights in feet, and volumes in
#'   cubic feet. Default: \code{NULL}.
#' @param height_at_dib The callback finds height from inside diameter. R function accepting dbh,
#'   ht, measurement arguments, and aux, with diameters in inches, heights in feet, and volumes in
#'   cubic feet. Default: \code{NULL}.
#' @param volume The callback computes volume between two heights. R function accepting dbh, ht,
#'   measurement arguments, and aux, with diameters in inches, heights in feet, and volumes in
#'   cubic feet. Default: \code{NULL}.
#' @param inputs The input declaration lists additional measurements the model needs. List with
#'   required and optional character vectors and a list of paired input names. Default:
#'   \code{list(required = character(), optional = character(), pairs = list())}.
#' @param spcd The species codes limit the trees this model covers. Numeric vector of species
#'   codes. Default: \code{integer()}.
#' @param stump_ht Stump height above ground. Numeric
#'   scalar, feet. Default: \code{1}.
#' @param bark_ratio The bark ratio estimates inside diameter from outside diameter when needed.
#'   Numeric scalar, inside diameter divided by outside diameter. Default: \code{NA_real_}.
#' @param source The source records where the model came from. Character scalar, provenance text.
#'   Default: \code{NULL} stores an empty string.
#' @return A validated taper_model list containing id, form, kernel (callbacks), inputs,
#'   spcd, stump_ht (feet), bark_ratio (diameter ratio), source (provenance), and internal
#'   equation metadata.
#' @usage
#' new_taper_model(
#'   id,
#'   form,
#'   dib,
#'   dob = NULL,
#'   height_at_dib = NULL,
#'   volume = NULL,
#'   inputs = list(required = character(), optional = character(), pairs = list()),
#'   spcd = integer(),
#'   stump_ht = 1,
#'   bark_ratio = NA_real_,
#'   source = NULL
#' )
#' @export
#' @examples
#' ## Inspect the shipped demonstration callback
#' base <- get_taper_model(model = 'demo.paraboloid.r')
#'
#' ## Construct a local model from the callback
#' local_model <- new_taper_model(id = 'example.callback',
#'                                form = 'demonstration',
#'                                dib = base$dib)
#'
#' ## Inspect the local model
#' print(local_model)
new_taper_model <- function(
  id,
  form,
  dib,
  dob = NULL,
  height_at_dib = NULL,
  volume = NULL,
  inputs = list(required = character(), optional = character(), pairs = list()),
  spcd = integer(),
  stump_ht = 1,
  bark_ratio = NA_real_,
  source = NULL
) {
  if (is.null(source))
    source <- ""
  model <- new_stem_model_unchecked(
    id = id, form = form, kernel = list(
      type = "r", key = NA_character_,
      has_dob = !is.null(dob), has_inverse = !is.null(height_at_dib),
      has_integral = !is.null(volume)
    ),
    dib = dib, dob = dob, height_at_dib = height_at_dib, volume = volume, inputs = inputs,
    measurement_system = "imperial",
    spcd = spcd, stump_ht = stump_ht, bark_ratio = bark_ratio,
    source = source, data = list()
  )
  .validate_taper_model(model)
}


.validate_taper_model <- function(x) {
  if (!inherits(x, "taper_model")) {
    stop("x must inherit from taper_model.", call. = FALSE)
  }
  expected <- c(
    "id", "form", "kernel", "dib", "dob", "height_at_dib", "volume", "inputs",
    "measurement_system",
    "spcd", "stump_ht", "bark_ratio", "source", "notes", "data", "class_version"
  )
  if (!identical(names(x), expected)) {
    stop("taper_model fields do not match class version 0.5.", call. = FALSE)
  }
  .scalar_character(x$id, "id")
  .scalar_character(x$form, "form")
  .scalar_character(x$measurement_system, "measurement_system", c("imperial", "metric"))
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
    !is.character(x$kernel$type) ||
    length(x$kernel$type) != 1L || is.na(x$kernel$type) || !x$kernel$type %in% c(
    "r", "compiled"
  )) {
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
  x$spcd <- .normalize_species_codes(x$spcd)
  if (!is.numeric(x$stump_ht) || length(x$stump_ht) != 1L || !is.finite(
    x$stump_ht
  ) || x$stump_ht <
    0) {
    stop("stump_ht must be one finite non-negative number.", call. = FALSE)
  }
  if (!is.numeric(x$bark_ratio) || length(x$bark_ratio) != 1L || (!is.na(
    x$bark_ratio
  ) && (!is.finite(x$bark_ratio) ||
          x$bark_ratio <= 0 || x$bark_ratio > 1))) {
    stop("bark_ratio must be NA or one number in (0, 1].", call. = FALSE)
  }
  if (is.environment(x$data) || is.function(x$data) || typeof(x$data) == "externalptr") {
    stop("data must contain immutable model data.", call. = FALSE)
  }
  nvel_family <- grepl("nvel|nsvb|^flewelling_|^clark_|_taper$|^r4_driver$",
    x$form,
    ignore.case = TRUE
  )
  if (grepl("^[[:alnum:]]{10}$", x$id) && !nvel_family) {
    warning("id resembles an NVEL equation identifier but form is not NVEL.",
      call. = FALSE
    )
  }

  for (name in names(.callback_formals)) {
    .validate_callback(x[[name]], name)
  }
  if (identical(x$kernel$type, "compiled")) {
    .scalar_character(x$kernel$key, "compiled kernel key")
    if (!tv_cpp_kernel_exists(x$kernel$key)) {
      stop("compiled kernel is not present in the C++ registry.", call. = FALSE)
    }
    if (startsWith(x$kernel$key, "published:")) {
      if (!identical(x$kernel$key, paste0("published:", x$form))) {
        stop("published kernel key and form must identify the same form.", call. = FALSE)
      }
      .validate_published_taper_data(x$form, x$data)
    }
    return(invisible(x))
  }

  if (!is.character(x$kernel$key) || length(x$kernel$key) != 1L || !is.na(x$kernel$key)) {
    stop("an R kernel key must be NA.", call. = FALSE)
  }

  if (is.null(x$dib)) {
    stop("an R kernel must provide dib.", call. = FALSE)
  }
  expected_capabilities <- c(
    has_dob = !is.null(x$dob), has_inverse = !is.null(x$height_at_dib),
    has_integral = !is.null(x$volume)
  )
  actual_capabilities <- unlist(x$kernel[names(expected_capabilities)], use.names = TRUE)
  if (!identical(actual_capabilities, expected_capabilities)) {
    stop("R kernel capability flags do not match its callbacks.", call. = FALSE)
  }

  probe_h <- seq(max(x$stump_ht, 0), 80, length.out = 17L)
  probe_aux <- stats::setNames(lapply(
    c(x$inputs$required, x$inputs$optional),
    function(name) {
      if (identical(name, "upper_bark"))
        rep("ob", 17L) else rep(1, 17L)
    }
  ), c(x$inputs$required, x$inputs$optional))
  run_probe <- function(callback, ...) {
    .run_validation_probe(callback, list(...))
  }
  profile <- run_probe(x$dib, rep(10, 17L), rep(80, 17L), probe_h, probe_aux)
  if (!is.double(profile) || length(profile) != 17L || any(!is.finite(profile)) ||
        any(profile <
              0)) {
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
    heights <- run_probe(x$height_at_dib, rep(10, 17L), rep(80, 17L), profile, probe_aux)
    if (!is.double(heights) || length(heights) != 17L || any(!is.finite(heights))) {
      stop("height_at_dib must return finite doubles on the probe grid.", call. = FALSE)
    }
  }
  if (!is.null(x$volume)) {
    volumes <- run_probe(x$volume, rep(10, 17L), rep(80, 17L), rep(x$stump_ht, 17L), rep(
      70,
      17L
    ), probe_aux)
    if (!is.double(volumes) || length(volumes) != 17L || any(!is.finite(volumes))) {
      stop("volume must return finite doubles on the probe grid.", call. = FALSE)
    }
  }
  invisible(x)
}

#' @export
format.taper_model <- function(x, ...) {
  capabilities <- c(
    dob = x$kernel$has_dob || is.finite(x$bark_ratio), inverse = x$kernel$has_inverse,
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
    "<taper_model ", x$id, ">\n", "  form: ", x$form, "\n", "  kernel: ", x$kernel$type,
    "\n", "  units: ", x$measurement_system, "\n", "  oracle verified: ", oracle_label, "\n",
    "  capabilities: ",
    if (length(enabled))
      paste(enabled, collapse = ", ") else "dib"
  )
}

#' @export
print.taper_model <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
