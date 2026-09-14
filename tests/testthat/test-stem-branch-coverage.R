test_that("inversion refinement covers exact and failed midpoint branches", {
  model <- new_taper_model("private.refine", "test", linear_dib)
  exact <- merchandiser:::.refine_crossing_brackets(
    model, "dib", 10, 80, 5, list(), 1L, 39, 41,
    linear_dib(10, 80, 39, list()) - 5
  )
  expect_identical(exact$status, 0L)
  expect_identical(exact$value, 40)

  failed_dib <- function(dbh, ht, h, aux) {
    value <- as.double(dbh * (1 - h / ht))
    attr(value, "status") <- ifelse(h == 40, 54L, 0L)
    value
  }
  failed_model <- new_taper_model("private.refine_failure", "test", failed_dib)
  failed <- merchandiser:::.refine_crossing_brackets(
    failed_model, "dib", 10, 80, 5, list(), 1L, 39, 41,
    failed_dib(10, 80, 39, list()) - 5
  )
  expect_identical(failed$status, 54L)

  all_failed <- function(dbh, ht, h, aux) {
    value <- as.double(dbh * (1 - h / ht))
    attr(value, "status") <- rep(54L, length(value))
    value
  }
  scan_model <- new_taper_model("private.scan_failure", "test", all_failed)
  scan <- merchandiser:::.numeric_inverse(
    scan_model, "dib", 10, 80, 5, list(), 1L
  )
  expect_identical(scan$status, 54L)

  tracker <- new.env(parent = emptyenv())
  tracker$calls <- 0L
  refinement_failure <- function(dbh, ht, h, aux) {
    tracker$calls <- tracker$calls + 1L
    value <- as.double(dbh * (1 - h / ht))
    if (tracker$calls > 1L) {
      attr(value, "status") <- rep(54L, length(value))
    }
    value
  }
  refinement_model <- new_taper_model(
    "private.refinement_scan_failure", "test", refinement_failure
  )
  tracker$calls <- 0L
  target <- linear_dib(10, 80, 40.001, list())
  refinement <- merchandiser:::.numeric_inverse(
    refinement_model, "dib", 10, 80, target, list(), 1L
  )
  expect_identical(refinement$status, 54L)
  expect_true(is.na(refinement$value))
})

test_that("outside inversion covers missing and mixed bark ratios", {
  model <- new_taper_model(
    "private.mixed_bark", "test", linear_dib,
    inputs = list(
      required = character(), optional = "bark_ratio", pairs = list()
    ),
    bark_ratio = NA_real_
  )
  register_taper_model(model)
  on.exit(unregister_taper_model("private.mixed_bark"), add = TRUE)
  missing <- height_at_dob(10, 80, 5, "private.mixed_bark", status = TRUE)
  expect_identical(missing$status, 53L)

  aux <- merchandiser:::.set_aux_caller_units(
    list(bark_ratio = c(NA_real_, 0.9)), "imperial"
  )
  mixed <- merchandiser:::.inverse_group(
    model, "dob", c(10, 10), c(80, 80), c(5, 5), aux, 1:2
  )
  expect_identical(mixed$status, c(53L, 0L))
  expect_true(is.na(mixed$value[[1L]]))
  expect_true(is.finite(mixed$value[[2L]]))

  inverse <- function(dbh, ht, dib, aux) {
    as.double(ht * (1 - dib / dbh))
  }
  analytic_model <- new_taper_model(
    "private.mixed_bark_analytic", "test", linear_dib,
    height_at_dib = inverse,
    inputs = list(
      required = character(), optional = "bark_ratio", pairs = list()
    ),
    bark_ratio = NA_real_
  )
  analytic <- merchandiser:::.inverse_group(
    analytic_model, "dob", c(10, 10), c(80, 80), c(5, 5), aux, 1:2
  )
  expect_identical(analytic$status, c(53L, 0L))
  expect_true(is.finite(analytic$value[[2L]]))
})

test_that("invalid groups exit height, volume, and profile evaluation", {
  expect_identical(
    height_at_dib(10, 80, 5, "missing.model", status = TRUE)$status,
    50L
  )
  expect_identical(
    height_at_dib(0, 80, 5, "demo.paraboloid", status = TRUE)$status,
    2L
  )
  expect_identical(
    stem_volume(10, 80, "missing.model", status = TRUE)$status,
    50L
  )
  expect_identical(
    stem_volume(0, 80, "demo.paraboloid", status = TRUE)$status,
    2L
  )
  expect_identical(stem_profile(
    10, 80, "missing.model", status = TRUE
  )$status, 50L)
  expect_identical(stem_profile(
    0, 80, "demo.paraboloid", status = TRUE
  )$status, 2L)
})

test_that("quadrature covers a degenerate interval and a kernel status", {
  model <- get_taper_model("demo.paraboloid.r")
  zero <- merchandiser:::.gauss_legendre_integral(
    model, "dib", 10, 80, 20, 20, list(), 1L
  )
  expect_identical(zero$status, 0L)
  expect_identical(zero$value, 0)

  failed_dib <- function(dbh, ht, h, aux) {
    value <- as.double(dbh * (1 - h / ht))
    attr(value, "status") <- rep(54L, length(value))
    value
  }
  register_test_model("private.quadrature_failure", dib = failed_dib)
  on.exit(unregister_taper_model("private.quadrature_failure"), add = TRUE)
  failed <- stem_volume(
    10, 80, "private.quadrature_failure", status = TRUE
  )
  expect_identical(failed$status, 54L)
  expect_true(is.na(failed$value))
})
