test_that("imperial and metric diameter calls round trip", {
  imperial <- dib(12, 80, 20, "demo.paraboloid")
  metric <- dib(
    12 * 2.54, 80 * 0.3048, 20 * 0.3048,
    "demo.paraboloid", units = "metric"
  )
  expect_equal(metric / 2.54, imperial, tolerance = 1e-14)

  outside_imperial <- dob(12, 80, 20, "demo.paraboloid")
  outside_metric <- dob(
    12 * 2.54, 80 * 0.3048, 20 * 0.3048,
    "demo.paraboloid", units = "metric"
  )
  expect_equal(outside_metric / 2.54, outside_imperial, tolerance = 1e-14)
})

test_that("height and volume outputs convert at the boundary", {
  target <- dib(12, 80, 30, "demo.paraboloid")
  imperial_height <- height_at_dib(12, 80, target, "demo.paraboloid")
  metric_height <- height_at_dib(
    12 * 2.54, 80 * 0.3048, target * 2.54,
    "demo.paraboloid", units = "metric"
  )
  expect_equal(metric_height / 0.3048, imperial_height, tolerance = 1e-13)

  imperial_volume <- stem_volume(12, 80, "demo.paraboloid")
  metric_volume <- stem_volume(
    12 * 2.54, 80 * 0.3048, "demo.paraboloid", units = "metric"
  )
  expect_equal(metric_volume / 0.028316846592, imperial_volume, tolerance = 1e-13)
})

test_that("default profile steps are equivalent across unit systems", {
  imperial <- stem_profile(12, 20, "demo.paraboloid")
  metric <- stem_profile(
    12 * 2.54, 20 * 0.3048, "demo.paraboloid", units = "metric"
  )
  expect_equal(nrow(metric), nrow(imperial))
  expect_equal(metric$h / 0.3048, imperial$h, tolerance = 1e-12)
})

test_that("metric-native user callbacks receive native metric values", {
  seen <- new.env(parent = emptyenv())
  metric_dib <- function(dbh, ht, h, aux) {
    seen$dbh <- dbh
    seen$ht <- ht
    seen$h <- h
    as.double(dbh * (1 - h / ht))
  }
  model <- new_taper_model(
    "private.metric", "user", metric_dib, units = "metric", bark_ratio = 0.9
  )
  register_taper_model(model)
  on.exit(unregister_taper_model("private.metric"), add = TRUE)

  dib(10 / 2.54, 20 / 0.3048, 5 / 0.3048, "private.metric")
  expect_equal(seen$dbh, 10)
  expect_equal(seen$ht, 20)
  expect_equal(seen$h, 5)
})

test_that("metric-native inverse and integral convert back to imperial", {
  metric_inverse <- function(dbh, ht, dib, aux) {
    as.double(ht * (1 - dib / dbh))
  }
  metric_volume <- function(dbh, ht, lower, upper, aux) {
    scale <- pi * dbh^2 / 40000
    as.double(scale * ((upper - lower) - (upper^2 - lower^2) / (2 * ht)))
  }
  model <- new_taper_model(
    "private.metric_analytic", "user", linear_dib,
    height_at_dib = metric_inverse, volume = metric_volume,
    units = "metric", bark_ratio = 0.9
  )
  register_taper_model(model)
  on.exit(unregister_taper_model("private.metric_analytic"), add = TRUE)

  expect_equal(
    height_at_dib(10 / 2.54, 20 / 0.3048, 5 / 2.54, "private.metric_analytic"),
    10 / 0.3048,
    tolerance = 1e-12
  )
  expect_true(is.finite(stem_volume(
    10 / 2.54, 20 / 0.3048, "private.metric_analytic"
  )))
})

test_that("dimensioned auxiliaries round trip through a metric boundary", {
  seen <- new.env(parent = emptyenv())
  auxiliary_dib <- function(dbh, ht, h, aux) {
    seen$aux <- aux
    as.double(aux$upper_d1)
  }
  dimensioned <- c(
    "upper_ht1", "upper_ht2", "upper_d1", "upper_d2", "site_index",
    "basal_area"
  )
  model <- new_taper_model(
    "private.aux_units", "test", auxiliary_dib,
    inputs = list(required = dimensioned, optional = character(), pairs = list()),
    bark_ratio = 0.9
  )
  register_taper_model(model)
  on.exit(unregister_taper_model("private.aux_units"), add = TRUE)

  area_factor <- 0.09290304 / 0.40468564224
  imperial <- dib(
    10, 80, 20, "private.aux_units", upper_ht1 = 30, upper_ht2 = 40,
    upper_d1 = 8, upper_d2 = 6, site_index = 100, basal_area = 120
  )
  metric <- dib(
    25.4, 80 * 0.3048, 20 * 0.3048, "private.aux_units",
    upper_ht1 = 30 * 0.3048, upper_ht2 = 40 * 0.3048,
    upper_d1 = 8 * 2.54, upper_d2 = 6 * 2.54,
    site_index = 100 * 0.3048, basal_area = 120 * area_factor,
    units = "metric"
  )

  expect_equal(metric, imperial * 2.54, tolerance = 1e-13)
  expect_equal(seen$aux$upper_ht1, 30, tolerance = 1e-13)
  expect_equal(seen$aux$upper_ht2, 40, tolerance = 1e-13)
  expect_equal(seen$aux$upper_d1, 8, tolerance = 1e-13)
  expect_equal(seen$aux$upper_d2, 6, tolerance = 1e-13)
  expect_equal(seen$aux$site_index, 100, tolerance = 1e-13)
  expect_equal(seen$aux$basal_area, 120, tolerance = 1e-13)
})
