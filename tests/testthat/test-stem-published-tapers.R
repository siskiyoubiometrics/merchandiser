test_that("published forms instantiate metric compiled models", {
  expected <- list(
    kozak_1988 = c(FALSE, FALSE), kozak_2002 = c(FALSE, FALSE),
    max_burkhart = c(
      TRUE,
      TRUE
    )
  )
  for (form in names(taper_test_truth)) {
    model <- taper_model_from_coefficients(
      id = paste0("private.form.", form), form = form,
      coefficients = taper_test_truth[[form]]
    )
    expect_s3_class(model, "taper_model")
    expect_identical(model$measurement_system, "metric")
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
  on.exit(
    {
      for (registered_id in ids) {
        if (has_taper_model(registered_id)) unregister_taper_model(registered_id)
      }
    },
    add = TRUE
  )
  for (form in names(taper_test_truth)) {
    id <- paste0("private.profile.", form)
    register_taper_test_model(id, form, taper_test_truth[[form]])
    expected <- merchandiser:::.taper_evaluate(
      form, rep(40, length(heights)), rep(25, length(heights)),
      heights, taper_test_truth[[form]]
    )
    expect_equal(dib(
      dbh = 40 / 2.54, ht = 25 / 0.3048, h = heights / 0.3048,
      model = id, spcd = .interface_spcd(id)
    )$value *
      2.54, expected, tolerance = 1e-12)
    expect_true(is.finite(stem_volume(
      dbh = 40 / 2.54,
      ht = 25 / 0.3048,
      model = id,
      from = 0 / 0.3048,
      spcd = .interface_spcd(id),
      inside_bark = TRUE
    )$value * 0.028316846592))
  }
})

test_that("Max and Burkhart analytic operations agree with numeric checks", {
  model <- register_taper_test_model(
    "private.max.numeric", "max_burkhart",
    taper_test_truth$max_burkhart
  )
  on.exit(unregister_taper_model("private.max.numeric"), add = TRUE)
  target <- dib(
    dbh = 40 / 2.54, ht = 25 / 0.3048, h = 10 / 0.3048, model = "private.max.numeric",
    spcd = .interface_spcd("private.max.numeric")
  )$value * 2.54
  expect_equal(height_at_dib(
    dbh = 40 / 2.54, ht = 25 / 0.3048, dib = target / 2.54, model = "private.max.numeric",
    spcd = .interface_spcd("private.max.numeric")
  )$value * 0.3048, 10, tolerance = 1e-10)
  analytic <- stem_volume(
    dbh = 40 / 2.54,
    ht = 25 / 0.3048,
    model = "private.max.numeric",
    from = 0 / 0.3048,
    spcd = .interface_spcd("private.max.numeric"),
    inside_bark = TRUE
  )$value *
    0.028316846592
  numeric <- merchandiser:::.gauss_legendre_integral(
    model, "dib", 40, 25, 0, 25,
    list(), 1L
  )
  expect_identical(numeric$status, 0L)
  expect_equal(analytic, numeric$value, tolerance = 5e-08)
})

test_that("Kozak inverses use crossing discovery", {
  ids <- paste0("private.crossing.", c("kozak_1988", "kozak_2002"))
  on.exit(
    {
      for (registered_id in ids) {
        if (has_taper_model(registered_id)) unregister_taper_model(registered_id)
      }
    },
    add = TRUE
  )
  for (form in c("kozak_1988", "kozak_2002")) {
    id <- paste0("private.crossing.", form)
    model <- register_taper_test_model(id, form, taper_test_truth[[form]])
    expect_false(model$kernel$has_inverse)
    target <- dib(
      dbh = 40 / 2.54, ht = 25 / 0.3048, h = 5 / 0.3048, model = id,
      spcd = .interface_spcd(id)
    )$value *
      2.54
    inverse <- .interface_metric_result(height_at_dib(
      dbh = 40 / 2.54, ht = 25 / 0.3048, dib = target / 2.54,
      model = id, spcd = .interface_spcd(id)
    ), 0.3048)
    expect_equal(inverse$value, 5, tolerance = 1e-04)
    expect_true(inverse$status %in% c(0L, 102L))
  }
})

test_that("published form validation rejects invalid model data", {
  max_coefficients <- taper_test_truth$max_burkhart
  expect_error(taper_model_from_coefficients(
    id = "private.bad.type", form = "max_burkhart",
    coefficients = as.character(max_coefficients)
  ), "finite named coefficient vector")
  expect_error(taper_model_from_coefficients(
    id = "private.bad.names", form = "max_burkhart",
    coefficients = unname(max_coefficients)
  ), "named coefficient vector")
  expect_error(taper_model_from_coefficients(
    id = "private.bad.knots", form = "max_burkhart",
    coefficients = replace(max_coefficients, c(5, 6), c(0.1, 0.8))
  ), "0 < a2 < a1 < 1", fixed = TRUE)
  kozak_coefficients <- taper_test_truth$kozak_1988
  expect_error(taper_model_from_coefficients(
    id = "private.bad.p",
    form = "kozak_1988", coefficients = replace(
      kozak_coefficients,
      9, 1
    )
  ), "p in (0, 1)", fixed = TRUE)
  model <- taper_model_from_coefficients(
    id = "private.bad.match",
    form = "max_burkhart", coefficients = max_coefficients
  )
  model$kernel$key <- "published:kozak_2002"
  expect_error(.validate_taper_model(model), "same form")
})
