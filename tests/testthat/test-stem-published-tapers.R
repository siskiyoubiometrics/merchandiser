test_that("published forms instantiate metric compiled models", {
  expected <- list(
    kozak_1988 = c(FALSE, FALSE),
    kozak_2002 = c(FALSE, FALSE),
    max_burkhart = c(TRUE, TRUE)
  )
  for (form in names(taper_test_truth)) {
    model <- taper_model_from_coefficients(
      paste0("private.form.", form), form, taper_test_truth[[form]]
    )
    expect_s3_class(model, "taper_model")
    expect_identical(model$units, "metric")
    expect_identical(model$data, taper_test_truth[[form]])
    expect_identical(model$kernel$type, "compiled")
    expect_identical(
      c(model$kernel$has_inverse, model$kernel$has_integral),
      expected[[form]]
    )
    expect_match(model$source, "doi:", fixed = TRUE)
  }
})

test_that("compiled diameters implement each published form", {
  heights <- c(0, 1.3, 5, 10, 20)
  ids <- paste0("private.profile.", names(taper_test_truth))
  on.exit({
    for (registered_id in ids) {
      if (has_taper_model(registered_id)) unregister_taper_model(registered_id)
    }
  }, add = TRUE)
  for (form in names(taper_test_truth)) {
    id <- paste0("private.profile.", form)
    register_taper_test_model(id, form, taper_test_truth[[form]])
    expected <- merchandiser:::.taper_evaluate(
      form, rep(40, length(heights)), rep(25, length(heights)),
      heights, taper_test_truth[[form]]
    )
    expect_equal(
      dib(40, 25, heights, id, units = "metric"),
      expected,
      tolerance = 1e-12
    )
    expect_true(is.finite(stem_volume(
      40, 25, id, lower = 0, lower_type = "height", units = "metric"
    )))
  }
})

test_that("Max and Burkhart analytic operations agree with numeric checks", {
  model <- register_taper_test_model(
    "private.max.numeric", "max_burkhart", taper_test_truth$max_burkhart
  )
  on.exit(unregister_taper_model("private.max.numeric"), add = TRUE)
  target <- dib(40, 25, 10, "private.max.numeric", units = "metric")
  expect_equal(
    height_at_dib(40, 25, target, "private.max.numeric", units = "metric"),
    10,
    tolerance = 1e-10
  )
  analytic <- stem_volume(
    40, 25, "private.max.numeric", lower = 0, lower_type = "height",
    units = "metric"
  )
  numeric <- merchandiser:::.gauss_legendre_integral(
    model, "dib", 40, 25, 0, 25, list(), 1L
  )
  expect_identical(numeric$status, 0L)
  expect_equal(analytic, numeric$value, tolerance = 5e-8)
})

test_that("Kozak inverses use crossing discovery", {
  ids <- paste0("private.crossing.", c("kozak_1988", "kozak_2002"))
  on.exit({
    for (registered_id in ids) {
      if (has_taper_model(registered_id)) unregister_taper_model(registered_id)
    }
  }, add = TRUE)
  for (form in c("kozak_1988", "kozak_2002")) {
    id <- paste0("private.crossing.", form)
    model <- register_taper_test_model(id, form, taper_test_truth[[form]])
    expect_false(model$kernel$has_inverse)
    target <- dib(40, 25, 5, id, units = "metric")
    inverse <- height_at_dib(
      40, 25, target, id, units = "metric", status = TRUE
    )
    expect_equal(inverse$value, 5, tolerance = 1e-4)
    expect_true(inverse$status %in% c(0L, 102L))
  }
})

test_that("metric-native models round trip through imperial calls", {
  id <- "private.metric.kozak"
  register_taper_test_model(
    id, "kozak_2002", taper_test_truth$kozak_2002
  )
  on.exit(unregister_taper_model(id), add = TRUE)
  metric_diameter <- dib(40, 25, 5, id, units = "metric")
  imperial_diameter <- dib(
    40 / 2.54, 25 / 0.3048, 5 / 0.3048, id, units = "imperial"
  )
  expect_equal(imperial_diameter * 2.54, metric_diameter, tolerance = 1e-12)

  metric_height <- height_at_dib(
    40, 25, metric_diameter, id, units = "metric"
  )
  imperial_height <- height_at_dib(
    40 / 2.54, 25 / 0.3048, metric_diameter / 2.54, id,
    units = "imperial"
  )
  expect_equal(imperial_height * 0.3048, metric_height, tolerance = 1e-12)

  metric_volume <- stem_volume(
    40, 25, id, lower = 0, lower_type = "height", units = "metric"
  )
  imperial_volume <- stem_volume(
    40 / 2.54, 25 / 0.3048, id,
    lower = 0, lower_type = "height", units = "imperial"
  )
  expect_equal(
    imperial_volume * 0.028316846592, metric_volume, tolerance = 1e-12
  )
})

test_that("published form validation rejects invalid model data", {
  max_coefficients <- taper_test_truth$max_burkhart
  expect_error(
    taper_model_from_coefficients(
      "private.bad.type", "max_burkhart", as.character(max_coefficients)
    ),
    "finite named coefficient vector"
  )
  expect_error(
    taper_model_from_coefficients(
      "private.bad.names", "max_burkhart", unname(max_coefficients)
    ),
    "named coefficient vector"
  )
  expect_error(
    taper_model_from_coefficients(
      "private.bad.knots", "max_burkhart",
      replace(max_coefficients, c(5, 6), c(0.1, 0.8))
    ),
    "0 < a2 < a1 < 1",
    fixed = TRUE
  )
  kozak_coefficients <- taper_test_truth$kozak_1988
  expect_error(
    taper_model_from_coefficients(
      "private.bad.p", "kozak_1988",
      replace(kozak_coefficients, 9, 1)
    ),
    "p in (0, 1)",
    fixed = TRUE
  )
  model <- taper_model_from_coefficients(
    "private.bad.match", "max_burkhart", max_coefficients
  )
  model$kernel$key <- "published:kozak_2002"
  expect_error(validate_taper_model(model), "same form")
})
