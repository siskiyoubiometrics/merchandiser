test_that("new_taper_model constructs the versioned class", {
  model <- new_taper_model(id = "test.constructor", form = "user", dib = linear_dib)

  expect_s3_class(model, "taper_model")
  expect_identical(model$class_version, "0.5")
  expect_identical(attr(model, "oracle_verified"), NA)
  expect_identical(model$data, list())
  expect_identical(model$spcd, integer())
  expect_named(model$kernel, c("type", "key", "has_dob", "has_inverse", "has_integral"))
  expect_identical(.validate_taper_model(model), model)
  expect_match(format(model), "test.constructor")
  expect_output(print(model), "kernel: r")
  expect_output(print(model), "oracle verified: not applicable")
})

test_that("oracle verification metadata is validated and printed", {
  model <- get_taper_model("I15FW2W017")
  expect_identical(attr(model, "oracle_verified"), FALSE)
  expect_output(print(model), "oracle verified: no")
  first <- dib(
    dbh = 12, ht = 80, h = c(1, 20, 40, 79), model = model$id,
    spcd = .interface_spcd(model$id)
  )
  second <- dib(
    dbh = 12, ht = 80, h = c(1, 20, 40, 79), model = model$id,
    spcd = .interface_spcd(model$id)
  )
  expect_identical(second, first)
  expect_identical(first$status, integer(4L))
  expect_true(all(is.finite(first$value)))

  verified <- get_taper_model("F00FW2W202")
  expect_identical(attr(verified, "oracle_verified"), TRUE)
  expect_output(print(verified), "oracle verified: yes")

  malformed <- model
  attr(malformed, "oracle_verified") <- "no"
  expect_error(.validate_taper_model(malformed), "oracle_verified")
})

test_that("model validation rejects malformed metadata and callbacks", {
  expect_error(new_taper_model(id = "bad", form = "user", dib = function(x) x), "formals")
  expect_error(
    new_taper_model(
      "bad", "user", linear_dib,
      source = NA_character_
    ), "source must be one non-missing character value"
  )
  numeric_species <- new_taper_model(
    id = "test.numeric_species", form = "user", dib = linear_dib,
    spcd = c(122, 202)
  )
  expect_identical(numeric_species$spcd, c(122L, 202L))
  expect_identical(
    new_taper_model(id = "test.unrestricted", form = "user", dib = linear_dib)$spcd,
    integer()
  )
  numeric_species$spcd <- c(202, 263)
  expect_identical(.validate_taper_model(numeric_species)$spcd, c(202L, 263L))
  expect_error(
    new_taper_model(id = "bad", form = "user", dib = linear_dib, spcd = 122.5),
    "whole-number"
  )
  expect_error(
    new_taper_model(id = "bad", form = "user", dib = linear_dib, spcd = 0),
    "positive whole-number"
  )
  expect_error(
    new_taper_model(id = "bad", form = "user", dib = linear_dib, spcd = -122),
    "positive whole-number"
  )
  invalid_species <- list("202", matrix(202, nrow = 1L), NA_real_, Inf, as.double(
    .Machine$integer.max
  ) +
    1)
  for (species in invalid_species) {
    expect_error(
      new_taper_model(id = "bad", form = "user", dib = linear_dib, spcd = species),
      "spcd"
    )
  }

  registry <- merchandiser:::.tv_registry
  old_models <- registry$models
  old_generation <- registry$generation
  on.exit(
    {
      registry$models <- old_models
      registry$generation <- old_generation
    },
    add = TRUE
  )
  numeric_species$id <- "test.package_numeric_species"
  numeric_species$spcd <- c(122, 202)
  merchandiser:::.register_package_model(numeric_species)
  expect_identical(get_taper_model("test.package_numeric_species")$spcd, c(122L, 202L))
  expect_error(
    new_taper_model(id = "bad", form = "user", dib = linear_dib, bark_ratio = 2),
    "bark_ratio"
  )
  increasing <- function(dbh, ht, h, aux) as.double(dbh + h)
  expect_error(new_taper_model(id = "bad", form = "user", dib = increasing), "monotone")
  integer_return <- function(dbh, ht, h, aux) rep(1L, length(dbh))
  expect_error(
    new_taper_model(id = "bad", form = "user", dib = integer_return),
    "doubles"
  )
})

test_that("NVEL-like private identifiers produce the specified warning", {
  expect_warning(new_taper_model(
    id = "1234567890", form = "user",
    dib = linear_dib
  ), "resembles an NVEL")
})

test_that("input declarations and pairs are validated", {
  expect_error(new_taper_model(
    id = "bad.inputs", form = "user", dib = linear_dib,
    inputs = list(
      optional = character(),
      required = character(), pairs = list()
    )
  ), "in that order")
  expect_error(new_taper_model(
    id = "bad.pair", form = "user", dib = linear_dib,
    inputs = list(
      required = character(),
      optional = "upper_ht1", pairs = list(c("upper_ht1", "upper_d1"))
    )
  ), "declared")

  upper_bark_dib <- function(dbh, ht, h, aux) {
    stopifnot(is.character(aux$upper_bark))
    as.double(dbh * (1 - h / ht))
  }
  upper_bark_model <- new_taper_model(
    id = "test.upper_bark", form = "user", dib = upper_bark_dib,
    inputs = list(required = "upper_bark", optional = character(), pairs = list())
  )
  expect_s3_class(upper_bark_model, "taper_model")
})

test_that("validation rejects and rolls back callback side effects", {
  global_name <- ".treevolume_validation_side_effect"
  if (exists(global_name, envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = global_name, envir = .GlobalEnv)
  }
  on.exit(
    {
      if (exists(global_name, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = global_name, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  global_writer <- function(dbh, ht, h, aux) {
    assign(global_name, TRUE, envir = .GlobalEnv)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(new_taper_model(
    id = "bad.global", form = "user",
    dib = global_writer
  ), "global environment")
  expect_false(exists(global_name, envir = .GlobalEnv, inherits = FALSE))

  assign(global_name, 1L, envir = .GlobalEnv)
  global_rewriter <- function(dbh, ht, h, aux) {
    assign(global_name, 2L, envir = .GlobalEnv)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.global_rewrite", form = "user", dib = global_rewriter),
    "global environment"
  )
  expect_identical(get(global_name, envir = .GlobalEnv), 1L)

  global_remover <- function(dbh, ht, h, aux) {
    rm(list = global_name, envir = .GlobalEnv)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.global_remove", form = "user", dib = global_remover),
    "global environment"
  )
  expect_identical(get(global_name, envir = .GlobalEnv), 1L)

  old_width <- getOption("width")
  option_writer <- function(dbh, ht, h, aux) {
    options(width = old_width + 1L)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(new_taper_model(
    id = "bad.options", form = "user",
    dib = option_writer
  ), "changed options")
  expect_identical(getOption("width"), old_width)

  option_adder <- function(dbh, ht, h, aux) {
    options(treevolume.validation.option = TRUE)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.option_add", form = "user", dib = option_adder),
    "changed options"
  )
  expect_null(getOption("treevolume.validation.option"))

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed)
    get(".Random.seed", envir = .GlobalEnv) else NULL
  rng_writer <- function(dbh, ht, h, aux) {
    stats::runif(1)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.rng", form = "user", dib = rng_writer),
    "RNG state"
  )
  expect_identical(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE), had_seed)
  if (had_seed) {
    expect_identical(get(".Random.seed", envir = .GlobalEnv), old_seed)
  }

  warning_callback <- function(dbh, ht, h, aux) {
    warning("probe warning")
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.warning", form = "user", dib = warning_callback),
    "produced a warning.*probe warning"
  )
  message_callback <- function(dbh, ht, h, aux) {
    message("probe message")
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.message", form = "user", dib = message_callback),
    "produced a message.*probe message"
  )

  error_callback <- function(dbh, ht, h, aux) {
    stop("probe error")
  }
  expect_error(new_taper_model(
    id = "bad.error", form = "user",
    dib = error_callback
  ), "raised a condition.*probe error")

  intruder <- new_taper_model(
    id = "private.validation_intruder", form = "test",
    dib = linear_dib
  )
  registry_writer <- function(dbh, ht, h, aux) {
    register_taper_model(intruder)
    as.double(dbh * (1 - h / ht))
  }
  expect_error(
    new_taper_model(id = "bad.registry", form = "user", dib = registry_writer),
    "registry cannot change"
  )
  expect_false(has_taper_model("private.validation_intruder"))
})

test_that("optional analytic callbacks are validated on probe vectors", {
  outside <- function(dbh, ht, h, aux) {
    as.double(dbh * (1.1 - h / ht))
  }
  inverse <- function(dbh, ht, dib, aux) {
    as.double(ht * (1 - dib / dbh))
  }
  volume <- function(dbh, ht, lower, upper, aux) {
    scale <- pi * dbh^2 / 576
    as.double(scale * ((upper - lower) - (upper^2 - lower^2) / (2 * ht)))
  }
  model <- new_taper_model(
    id = "test.callbacks", form = "user", dib = linear_dib, dob = outside,
    height_at_dib = inverse, volume = volume
  )
  expect_true(model$kernel$has_dob)
  expect_true(model$kernel$has_inverse)
  expect_true(model$kernel$has_integral)

  bad_dob <- function(dbh, ht, h, aux) rep(1L, length(dbh))
  expect_error(
    new_taper_model(id = "bad.dob", form = "user", dib = linear_dib, dob = bad_dob),
    "dob must return"
  )
})

test_that(".validate_taper_model rejects damaged representations", {
  expect_error(.validate_taper_model(list()), "inherit")
  model <- new_taper_model(id = "test.damage", form = "user", dib = linear_dib)
  model$notes <- NULL
  expect_error(.validate_taper_model(model), "fields")

  model <- new_taper_model(id = "test.flags", form = "user", dib = linear_dib)
  model$kernel$has_dob <- TRUE
  expect_error(.validate_taper_model(model), "capability flags")

  model <- new_taper_model(id = "test.mutable-data", form = "user", dib = linear_dib)
  model$data <- new.env(parent = emptyenv())
  expect_error(.validate_taper_model(model), "immutable model data")
})
