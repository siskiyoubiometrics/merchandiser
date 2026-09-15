test_that("numeric inversion agrees with the analytic paraboloid inverse", {
  set.seed(20260905)
  dbh <- runif(200, 8, 30)
  ht <- runif(200, 50, 140)
  target_h <- runif(200, 5, 0.9 * ht)
  target <- dib(
    dbh = dbh, ht = ht, h = target_h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value

  analytic <- height_at_dib(
    dbh = dbh, ht = ht, dib = target,
    model = "demo.paraboloid", spcd = .interface_spcd("demo.paraboloid")
  )$value
  numeric <- height_at_dib(
    dbh = dbh, ht = ht, dib = target,
    model = "demo.paraboloid.r", spcd = .interface_spcd("demo.paraboloid.r")
  )$value
  expect_equal(numeric, analytic, tolerance = 5e-05)
  expect_equal(numeric, target_h, tolerance = 5e-05)
  expect_equal(
    height_at_dib(
      dbh = dbh, ht = ht, dib = dib(dbh, ht, target_h, 202,
        model = "demo.paraboloid.r"
      )$value,
      model = "demo.paraboloid.r", spcd = .interface_spcd("demo.paraboloid.r")
    )$value, target_h,
    tolerance = 5e-05
  )
})

test_that("numeric integration agrees with the analytic paraboloid integral", {
  set.seed(5)
  dbh <- runif(30, 8, 30)
  ht <- runif(30, 50, 130)
  lower <- runif(30, 1, 10)
  upper <- ht - runif(30, 0, 10)

  analytic <- stem_volume(
    dbh = dbh,
    ht = ht,
    model = "demo.paraboloid",
    from = lower,
    to = upper,
    spcd = .interface_spcd("demo.paraboloid")
  )$value
  numeric <- stem_volume(
    dbh = dbh,
    ht = ht,
    model = "demo.paraboloid.r",
    from = lower,
    to = upper,
    spcd = .interface_spcd("demo.paraboloid.r")
  )$value
  expect_equal(numeric, analytic, tolerance = 1e-11)
})

test_that("inversion reports above-tip", {
  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  register_test_model("private.positive_tip", dib = positive_tip)
  on.exit(unregister_taper_model("private.positive_tip"), add = TRUE)
  above <- height_at_dib(
    dbh = 10, ht = 80, dib = 1, model = "private.positive_tip",
    spcd = .interface_spcd("private.positive_tip")
  )
  expect_identical(above$status, 100L)
})

test_that("every inversion path returns the independently known highest crossing", {
  lower_root <- 8.233549
  upper_root <- 8.266479
  target <- 10
  piecewise <- function(dbh, ht, h, aux) {
    value <- ifelse(
      h <= lower_root, target + 0.1 * (lower_root - h),
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
  register_test_model("private.crossings.analytic",
    dib = piecewise,
    height_at_dib = highest_inverse
  )
  on.exit(unregister_taper_model("private.crossings.numeric"), add = TRUE)
  on.exit(unregister_taper_model("private.crossings.analytic"), add = TRUE)

  for (model in c("private.crossings.numeric", "private.crossings.analytic")) {
    inside <- height_at_dib(
      dbh = 10, ht = 80, dib = target, model = model,
      spcd = .interface_spcd(model)
    )
    outside <- height_at_dob(
      dbh = 10, ht = 80, dob = target / 0.9, model = model,
      spcd = .interface_spcd(model)
    )
    expect_identical(inside$status, 102L)
    expect_identical(outside$status, 102L)
    expect_equal(inside$value, upper_root, tolerance = 5e-05)
    expect_equal(outside$value, upper_root, tolerance = 5e-05)
  }

  multiple_volume <- stem_volume(
    dbh = 10,
    ht = 80,
    model = "private.crossings.numeric",
    from_dib = target,
    spcd = .interface_spcd("private.crossings.numeric")
  )
  expect_identical(multiple_volume$status, 102L)
  expect_true(is.finite(multiple_volume$value))
  multiple_profile <- stem_profile(
    dbh = 10,
    ht = 80,
    model = "private.crossings.numeric",
    step = 10,
    from_dib = target,
    spcd = .interface_spcd(
      "private.crossings.numeric"
    ),
    tree_id = seq_along(rep_len(10, max(length(10), length(80))))
  )
  expect_true(all(multiple_profile$status == 102L))
  expect_true(all(is.finite(multiple_profile$h)))
})

test_that("a tangent at a discovery node is a crossing", {
  target <- 50
  root <- 1 + 1000 / (12 * 16)
  tangent <- function(dbh, ht, h, aux) {
    joined <- target - (12 - root)^2
    value <- ifelse(h <= 12, target - (h - root)^2, joined * (ht - h) / (ht - 12))
    as.double(value)
  }
  register_test_model("private.tangent", dib = tangent)
  on.exit(unregister_taper_model("private.tangent"), add = TRUE)

  result <- height_at_dib(
    dbh = 10, ht = 80, dib = target,
    model = "private.tangent", spcd = .interface_spcd("private.tangent")
  )
  expect_identical(result$status, 0L)
  expect_equal(result$value, root, tolerance = 1e-12)
})

test_that("physical height bounds prevent bisection exhaustion", {
  base_height <- 1e+12
  crossing <- base_height + 0.50006
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  stalled <- function(dbh, ht, h, aux) {
    calls$count <- calls$count + 1L
    runtime <- ht > 1e+06
    as.double(ifelse(runtime, ifelse(h < crossing, 3, 1), 1))
  }
  model <- new_taper_model(
    id = "private.exhaustion", form = "test", dib = stalled, stump_ht = base_height,
    bark_ratio = 0.9
  )
  calls$count <- 0L
  register_taper_model(model)
  on.exit(unregister_taper_model("private.exhaustion"), add = TRUE)
  calls$count <- 0L
  result <- height_at_dib(
    dbh = 10, ht = base_height + 1, dib = 2, model = "private.exhaustion",
    spcd = .interface_spcd("private.exhaustion")
  )
  expect_identical(result$status, 3L)
  expect_true(is.na(result$value))
  expect_identical(calls$count, 0L)
})

test_that("inside and outside diameter inversions are consistent", {
  height <- c(10, 30, 60)
  target_inside <- dib(
    dbh = 12, ht = 80, h = height, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value
  target_outside <- dob(
    dbh = 12, ht = 80, h = height, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value

  expect_equal(height_at_dib(
    dbh = 12, ht = 80, dib = target_inside, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value, height, tolerance = 1e-12)
  expect_equal(height_at_dob(
    dbh = 12, ht = 80, dob = target_outside, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value, height, tolerance = 1e-12)
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

  expect_length(
    dib(
      dbh = 1:20 + 10, ht = 80, h = 20, model = "private.counted",
      spcd = .interface_spcd("private.counted")
    )$value,
    20L
  )
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
  register_test_model("private.analytic",
    dob = outside, height_at_dib = inverse,
    volume = volume
  )
  on.exit(unregister_taper_model("private.analytic"), add = TRUE)

  expect_equal(
    height_at_dib(
      dbh = 10, ht = 80, dib = 5, model = "private.analytic",
      spcd = .interface_spcd("private.analytic")
    )$value,
    40
  )
  expect_true(is.finite(stem_volume(
    dbh = 10, ht = 80, model = "private.analytic",
    spcd = .interface_spcd("private.analytic")
  )$value))
  outside_target <- dob(
    dbh = 10, ht = 80, h = 30, model = "private.analytic",
    spcd = .interface_spcd("private.analytic")
  )$value
  expect_equal(height_at_dob(
    dbh = 10, ht = 80, dob = outside_target, model = "private.analytic",
    spcd = .interface_spcd("private.analytic")
  )$value, 30, tolerance = 5e-05)
  expect_true(is.finite(stem_volume(
    dbh = 10, ht = 80, model = "private.analytic", spcd = .interface_spcd(
      "private.analytic"
    ),
    inside_bark = FALSE
  )$value))
})

test_that("malformed R callback results become kernel_error", {
  malformed <- function(dbh, ht, h, aux) {
    if (any(dbh == 15))
      return(as.double(dbh[1L]))
    if (any(dbh == 16))
      return(rep(1L, length(dbh)))
    value <- as.double(dbh * (1 - h / ht))
    if (any(dbh == 17))
      attr(value, "status") <- rep("bad", length(value))
    if (any(dbh == 18))
      value[] <- Inf
    if (any(dbh == 19))
      attr(value, "status") <- rep(49L, length(value))
    if (any(dbh == 20))
      attr(value, "status") <- rep(1e+20, length(value))
    value
  }
  register_test_model("private.malformed", dib = malformed)
  on.exit(unregister_taper_model("private.malformed"), add = TRUE)

  wrong_length <- dib(
    dbh = c(15, 15), ht = 80, h = 20, model = "private.malformed",
    spcd = .interface_spcd("private.malformed")
  )
  expect_true(all(wrong_length$status == 54L))
  expect_identical(
    dib(
      dbh = 16, ht = 80, h = 20, model = "private.malformed",
      spcd = .interface_spcd("private.malformed")
    )$status,
    54L
  )
  expect_identical(
    dib(
      dbh = 17, ht = 80, h = 20, model = "private.malformed",
      spcd = .interface_spcd("private.malformed")
    )$status,
    54L
  )
  expect_identical(
    dib(
      dbh = 18, ht = 80, h = 20, model = "private.malformed",
      spcd = .interface_spcd("private.malformed")
    )$status,
    54L
  )
  expect_identical(
    dib(
      dbh = 19, ht = 80, h = 20, model = "private.malformed",
      spcd = .interface_spcd("private.malformed")
    )$status,
    54L
  )
  expect_identical(
    dib(
      dbh = 20, ht = 80, h = 20, model = "private.malformed",
      spcd = .interface_spcd("private.malformed")
    )$status,
    54L
  )
})

test_that("model errors take precedence over numeric diagnoses", {
  inverse_failure <- function(dbh, ht, dib, aux) {
    if (any(dib > 10))
      stop("inverse exploded")
    as.double(ht * (1 - dib / dbh))
  }
  register_test_model("private.inverse_failure", height_at_dib = inverse_failure)
  on.exit(unregister_taper_model("private.inverse_failure"), add = TRUE)
  result <- height_at_dib(
    dbh = 10, ht = 80, dib = 20,
    model = "private.inverse_failure", spcd = .interface_spcd("private.inverse_failure")
  )
  expect_identical(result$status, 54L)

  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  selective_failure <- function(dbh, ht, dib, aux) {
    if (any(dib == 5))
      stop("upper inverse exploded")
    as.double(ht * (1 - (dib - 2) / dbh))
  }
  register_test_model("private.bound_failure",
    dib = positive_tip,
    height_at_dib = selective_failure
  )
  on.exit(unregister_taper_model("private.bound_failure"), add = TRUE)
  bounds <- stem_volume(
    dbh = 10,
    ht = 80,
    model = "private.bound_failure",
    from_dib = 1,
    to_dib = 5,
    spcd = .interface_spcd("private.bound_failure")
  )
  expect_identical(bounds$status, 54L)
})

test_that("the thin compiled boundary validates requests", {
  evaluate <- merchandiser:::tv_cpp_kernel_eval
  vectors <- rep(list(1), 5L)
  expect_error(evaluate(
    "demo_paraboloid", 1L, 1:2, vectors[[2L]], vectors[[3L]], vectors[[4L]],
    vectors[[5L]], 1L
  ), "equal lengths")
  expect_error(evaluate("demo_paraboloid", 1L, 1, 1, 1, 1, 1, 0L), "at least one")
  expect_error(evaluate("missing", 1L, 1, 1, 1, 1, 1, 1L), "unknown")
  expect_error(evaluate("demo_paraboloid", 99L, 1, 1, 1, 1, 1, 1L), "requested operation")
  expect_true(is.nan(evaluate("demo_paraboloid", 1L, 0, 80, 10, 0, 0.9, 1L)))
  expect_true(is.nan(evaluate("demo_paraboloid", 2L, 10, 80, -1, 0, 0.9, 1L)))
  expect_true(is.nan(evaluate("demo_paraboloid", 3L, 10, 80, 5, 5, 0.9, 1L)))
})
