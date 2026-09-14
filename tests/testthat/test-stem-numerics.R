test_that("numeric inversion agrees with the analytic paraboloid inverse", {
  set.seed(20260905)
  dbh <- runif(200, 8, 30)
  ht <- runif(200, 50, 140)
  target_h <- runif(200, 5, 0.9 * ht)
  target <- dib(dbh, ht, target_h, "demo.paraboloid")

  analytic <- height_at_dib(dbh, ht, target, "demo.paraboloid")
  numeric <- height_at_dib(dbh, ht, target, "demo.paraboloid.r")
  expect_equal(numeric, analytic, tolerance = 5e-5)
  expect_equal(numeric, target_h, tolerance = 5e-5)
  expect_equal(
    height_at_dib(
      dbh, ht, dib(dbh, ht, target_h, "demo.paraboloid.r"),
      "demo.paraboloid.r"
    ),
    target_h,
    tolerance = 5e-5
  )
})

test_that("numeric integration agrees with the analytic paraboloid integral", {
  set.seed(5)
  dbh <- runif(30, 8, 30)
  ht <- runif(30, 50, 130)
  lower <- runif(30, 1, 10)
  upper <- ht - runif(30, 0, 10)

  analytic <- stem_volume(
    dbh, ht, "demo.paraboloid", lower, "height", upper, "height"
  )
  numeric <- stem_volume(
    dbh, ht, "demo.paraboloid.r", lower, "height", upper, "height"
  )
  expect_equal(numeric, analytic, tolerance = 1e-11)
})

test_that("inversion reports above-tip", {
  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  register_test_model("private.positive_tip", dib = positive_tip)
  on.exit(unregister_taper_model("private.positive_tip"), add = TRUE)
  above <- height_at_dib(10, 80, 1, "private.positive_tip", status = TRUE)
  expect_identical(above$status, 100L)

})

test_that("every inversion path returns the independently known highest crossing", {
  lower_root <- 8.233549
  upper_root <- 8.266479
  target <- 10
  piecewise <- function(dbh, ht, h, aux) {
    value <- ifelse(
      h <= lower_root,
      target + 0.1 * (lower_root - h),
      ifelse(
        h <= upper_root,
        target - 100 * (h - lower_root) * (upper_root - h),
        target + 0.1 * (h - upper_root) / (1 + h - upper_root)^2
      )
    )
    as.double(value)
  }
  highest_inverse <- function(dbh, ht, dib, aux) {
    rep(as.double(upper_root), length(dbh))
  }
  register_test_model("private.crossings.numeric", dib = piecewise)
  register_test_model(
    "private.crossings.analytic", dib = piecewise,
    height_at_dib = highest_inverse
  )
  on.exit(unregister_taper_model("private.crossings.numeric"), add = TRUE)
  on.exit(unregister_taper_model("private.crossings.analytic"), add = TRUE)

  for (model in c("private.crossings.numeric", "private.crossings.analytic")) {
    inside <- height_at_dib(10, 80, target, model, status = TRUE)
    outside <- height_at_dob(10, 80, target / 0.9, model, status = TRUE)
    expect_identical(inside$status, 102L)
    expect_identical(outside$status, 102L)
    expect_equal(inside$value, upper_root, tolerance = 5e-5)
    expect_equal(outside$value, upper_root, tolerance = 5e-5)
  }

  multiple_volume <- stem_volume(
    10, 80, "private.crossings.numeric", lower = target, lower_type = "dib",
    status = TRUE
  )
  expect_identical(multiple_volume$status, 102L)
  expect_true(is.finite(multiple_volume$value))
  multiple_profile <- stem_profile(
    10, 80, "private.crossings.numeric", lower = target, lower_type = "dib",
    step = 120, status = TRUE
  )
  expect_true(all(multiple_profile$status == 102L))
  expect_true(all(is.finite(multiple_profile$h)))
})

test_that("a tangent at a discovery node is a crossing", {
  target <- 50
  root <- 1 + 1000 / (12 * 16)
  tangent <- function(dbh, ht, h, aux) {
    joined <- target - (12 - root)^2
    value <- ifelse(
      h <= 12,
      target - (h - root)^2,
      joined * (ht - h) / (ht - 12)
    )
    as.double(value)
  }
  register_test_model("private.tangent", dib = tangent)
  on.exit(unregister_taper_model("private.tangent"), add = TRUE)

  result <- height_at_dib(10, 80, target, "private.tangent", status = TRUE)
  expect_identical(result$status, 0L)
  expect_equal(result$value, root, tolerance = 1e-12)
})

test_that("bisection exhaustion returns no_convergence", {
  base_height <- 1e12
  crossing <- base_height + 0.50006
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  stalled <- function(dbh, ht, h, aux) {
    calls$count <- calls$count + 1L
    runtime <- ht > 1e6
    as.double(ifelse(runtime, ifelse(h < crossing, 3, 1), 1))
  }
  model <- new_taper_model(
    "private.exhaustion", "test", stalled,
    stump_ht = base_height, bark_ratio = 0.9
  )
  calls$count <- 0L
  register_taper_model(model)
  on.exit(unregister_taper_model("private.exhaustion"), add = TRUE)
  result <- height_at_dib(
    10, base_height + 1, 2, "private.exhaustion", status = TRUE
  )
  expect_identical(result$status, 103L)
  expect_true(is.na(result$value))
  expect_gte(calls$count, 201L)
})

test_that("inside and outside diameter inversions are consistent", {
  height <- c(10, 30, 60)
  target_inside <- dib(12, 80, height, "demo.paraboloid")
  target_outside <- dob(12, 80, height, "demo.paraboloid")

  expect_equal(
    height_at_dib(12, 80, target_inside, "demo.paraboloid"),
    height,
    tolerance = 1e-12
  )
  expect_equal(
    height_at_dob(12, 80, target_outside, "demo.paraboloid"),
    height,
    tolerance = 1e-12
  )
})

test_that("R kernels are called once per ordinary model group", {
  calls <- 0L
  counted <- function(dbh, ht, h, aux) {
    calls <<- calls + 1L
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.counted", dib = counted)
  on.exit(unregister_taper_model("private.counted"), add = TRUE)
  calls <- 0L

  expect_length(dib(1:20 + 10, 80, 20, "private.counted"), 20L)
  expect_identical(calls, 1L)
})

test_that("R analytic inverse, integral, and direct dob callbacks are used", {
  outside <- function(dbh, ht, h, aux) {
    as.double(1.1 * dbh * (1 - h / ht))
  }
  inverse <- function(dbh, ht, dib, aux) {
    as.double(ht * (1 - dib / dbh))
  }
  volume <- function(dbh, ht, lower, upper, aux) {
    scale <- pi * dbh^2 / 576
    as.double(scale * ((upper - lower) - (upper^2 - lower^2) / (2 * ht)))
  }
  register_test_model(
    "private.analytic", dob = outside,
    height_at_dib = inverse, volume = volume
  )
  on.exit(unregister_taper_model("private.analytic"), add = TRUE)

  expect_equal(height_at_dib(10, 80, 5, "private.analytic"), 40)
  expect_true(is.finite(stem_volume(10, 80, "private.analytic")))
  outside_target <- dob(10, 80, 30, "private.analytic")
  expect_equal(
    height_at_dob(10, 80, outside_target, "private.analytic"),
    30,
    tolerance = 5e-5
  )
  expect_true(is.finite(stem_volume(
    10, 80, "private.analytic", bark = "outside"
  )))
})

test_that("malformed R callback results become kernel_error", {
  malformed <- function(dbh, ht, h, aux) {
    if (any(dbh == 15)) return(as.double(dbh[1L]))
    if (any(dbh == 16)) return(rep(1L, length(dbh)))
    value <- as.double(dbh * (1 - h / ht))
    if (any(dbh == 17)) attr(value, "status") <- rep("bad", length(value))
    if (any(dbh == 18)) value[] <- Inf
    if (any(dbh == 19)) attr(value, "status") <- rep(49L, length(value))
    if (any(dbh == 20)) attr(value, "status") <- rep(1e20, length(value))
    value
  }
  register_test_model("private.malformed", dib = malformed)
  on.exit(unregister_taper_model("private.malformed"), add = TRUE)

  wrong_length <- dib(c(15, 15), 80, 20, "private.malformed", status = TRUE)
  expect_true(all(wrong_length$status == 54L))
  expect_identical(dib(16, 80, 20, "private.malformed", status = TRUE)$status, 54L)
  expect_identical(dib(17, 80, 20, "private.malformed", status = TRUE)$status, 54L)
  expect_identical(dib(18, 80, 20, "private.malformed", status = TRUE)$status, 54L)
  expect_identical(dib(19, 80, 20, "private.malformed", status = TRUE)$status, 54L)
  expect_identical(dib(20, 80, 20, "private.malformed", status = TRUE)$status, 54L)
})

test_that("model errors take precedence over numeric diagnoses", {
  inverse_failure <- function(dbh, ht, dib, aux) {
    if (any(dib > 10)) stop("inverse exploded")
    as.double(ht * (1 - dib / dbh))
  }
  register_test_model(
    "private.inverse_failure", height_at_dib = inverse_failure
  )
  on.exit(unregister_taper_model("private.inverse_failure"), add = TRUE)
  result <- height_at_dib(
    10, 80, 20, "private.inverse_failure", status = TRUE
  )
  expect_identical(result$status, 54L)

  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  selective_failure <- function(dbh, ht, dib, aux) {
    if (any(dib == 5)) stop("upper inverse exploded")
    as.double(ht * (1 - (dib - 2) / dbh))
  }
  register_test_model(
    "private.bound_failure", dib = positive_tip,
    height_at_dib = selective_failure
  )
  on.exit(unregister_taper_model("private.bound_failure"), add = TRUE)
  bounds <- stem_volume(
    10, 80, "private.bound_failure", lower = 1, lower_type = "dib",
    upper = 5, upper_type = "dib", status = TRUE
  )
  expect_identical(bounds$status, 54L)
})

test_that("the thin compiled boundary validates requests", {
  evaluate <- merchandiser:::tv_cpp_kernel_eval
  vectors <- rep(list(1), 5L)
  expect_error(
    evaluate("demo_paraboloid", 1L, 1:2, vectors[[2L]], vectors[[3L]],
      vectors[[4L]], vectors[[5L]], 1L
    ),
    "equal lengths"
  )
  expect_error(
    evaluate("demo_paraboloid", 1L, 1, 1, 1, 1, 1, 0L),
    "at least one"
  )
  expect_error(evaluate("missing", 1L, 1, 1, 1, 1, 1, 1L), "unknown")
  expect_error(
    evaluate("demo_paraboloid", 99L, 1, 1, 1, 1, 1, 1L),
    "requested operation"
  )
  expect_true(is.nan(evaluate("demo_paraboloid", 1L, 0, 80, 10, 0, 0.9, 1L)))
  expect_true(is.nan(evaluate("demo_paraboloid", 2L, 10, 80, -1, 0, 0.9, 1L)))
  expect_true(is.nan(evaluate("demo_paraboloid", 3L, 10, 80, 5, 5, 0.9, 1L)))
})
