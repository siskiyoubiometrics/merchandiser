test_that("fit_taper recovers parameters for all three forms", {
  tolerances <- c(
    kozak_1988 = 0.003,
    kozak_2002 = 0.02,
    max_burkhart = 0.02
  )
  for (form in names(taper_test_truth)) {
    fit <- cached_taper_fit(form)
    expect_s3_class(fit, "taper_fit")
    expect_lt(
      max(abs(fit$coefficients - taper_test_truth[[form]])),
      tolerances[[form]]
    )
    expect_identical(fit$n_trees, 24L)
    expect_identical(fit$n_measurements, 288L)
    expect_true(fit$convergence$converged)
    expect_lt(fit$fit_statistics$overall$rmse, 0.002)
    expect_equal(
      sum(fit$fit_statistics$by_relative_height$n),
      fit$n_measurements
    )
  }
})

test_that("fit_taper warns when standardized residuals are undefined", {
  data <- simulate_taper_test_data(
    "kozak_1988", noise = 0, tree_count = 8L
  )
  expect_warning(
    fit <- fit_taper(
      data, "kozak_1988", start = taper_test_truth$kozak_1988
    ),
    "standardized residuals are undefined"
  )
  expect_true(all(is.na(fit$residual_diagnostics$standardized_residual)))
})

test_that("fit equation helpers reproduce the natural equations", {
  dbh <- c(25, 40, 55)
  ht <- c(18, 25, 32)
  h <- c(4, 12, 32)
  for (form in c("kozak_1988", "kozak_2002")) {
    internal <- merchandiser:::.taper_natural_to_internal(
      taper_test_truth[[form]], form
    )
    helper <- get(
      paste0(".taper_fit_", form),
      envir = asNamespace("merchandiser")
    )
    fitted <- do.call(
      helper,
      c(list(dbh = dbh, ht = ht, h = h), as.list(internal))
    )
    expected <- merchandiser:::.taper_evaluate(
      form, dbh, ht, h, taper_test_truth[[form]]
    )
    expect_equal(fitted, expected, tolerance = 1e-12)
  }
})

test_that("taper_fit methods report and predict", {
  fit <- cached_taper_fit("max_burkhart")
  expect_output(print(fit), "<taper_fit>")
  summarized <- summary(fit)
  expect_s3_class(summarized, "summary.taper_fit")
  expect_output(print(summarized), "Fit statistics by relative-height class")
  expect_length(predict(fit), fit$n_measurements)
  newdata <- data.frame(dbh = c(25, 40), ht = c(18, 27), h = c(4, 12))
  expected <- merchandiser:::.taper_evaluate(
    "max_burkhart", newdata$dbh, newdata$ht, newdata$h,
    fit$coefficients, stabilize = TRUE
  )
  expect_equal(predict(fit, newdata), expected)
  imperial <- transform(
    newdata,
    dbh = dbh / 2.54,
    ht = ht / 0.3048,
    h = h / 0.3048
  )
  expect_equal(predict(fit, imperial, units = "imperial") * 2.54, expected)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  expect_invisible(plot(fit))
  grDevices::dev.off()
  expect_true(file.exists(plot_file))
  unlink(plot_file)
})

test_that("a fitted model registers and serves every stem computation", {
  fit <- cached_taper_fit("max_burkhart")
  model <- as_taper_model(
    fit, "private.fitted.max", species = 122L, bark_ratio = 0.9
  )
  expect_identical(model$data, fit$coefficients)
  register_taper_model(model)
  on.exit(unregister_taper_model("private.fitted.max"), add = TRUE)
  inside <- dib(40, 25, 5, "private.fitted.max", units = "metric")
  outside <- dob(40, 25, 5, "private.fitted.max", units = "metric")
  expect_equal(outside * 0.9, inside)
  expect_equal(
    height_at_dib(
      40, 25, inside, "private.fitted.max", units = "metric"
    ),
    5,
    tolerance = 1e-4
  )
  expect_equal(
    height_at_dob(
      40, 25, outside, "private.fitted.max", units = "metric"
    ),
    5,
    tolerance = 1e-4
  )
  expect_true(is.finite(stem_volume(
    40, 25, "private.fitted.max", units = "metric"
  )))
  profile <- stem_profile(
    40, 25, "private.fitted.max", step = 100, units = "metric"
  )
  expect_s3_class(profile, "data.frame")
  expect_true(all(profile$status == 0L))
})

test_that("a fitted registered model works inside mutate", {
  skip_if_not_installed("dplyr")
  fit <- cached_taper_fit("kozak_2002")
  register_taper_model(as_taper_model(
    fit, "private.fitted.mutate", bark_ratio = 0.9
  ))
  on.exit(unregister_taper_model("private.fitted.mutate"), add = TRUE)
  trees <- data.frame(dbh = c(25, 40), ht = c(18, 27), h = c(4, 12))
  result <- dplyr::mutate(
    trees,
    diameter = dib(dbh, ht, h, "private.fitted.mutate", units = "metric"),
    volume = stem_volume(
      dbh, ht, "private.fitted.mutate", units = "metric"
    )
  )
  expect_equal(nrow(result), 2L)
  expect_true(all(is.finite(result$diameter)))
  expect_true(all(is.finite(result$volume)))
})

test_that("fit_taper supports weights, species, aliases, and imperial data", {
  data <- simulate_taper_test_data("max_burkhart")
  names(data)[names(data) == "tree_id"] <- "tree"
  names(data)[names(data) == "ht"] <- "total_height"
  names(data)[names(data) == "h"] <- "measurement_height"
  names(data)[names(data) == "dib"] <- "diameter_inside_bark"
  data$spcd <- 122
  data$fit_weight <- seq(1, 2, length.out = nrow(data))
  data$dbh <- data$dbh / 2.54
  data$total_height <- data$total_height / 0.3048
  data$measurement_height <- data$measurement_height / 0.3048
  data$diameter_inside_bark <- data$diameter_inside_bark / 2.54
  fit <- fit_taper(
    data, "max_burkhart", species = "spcd", weights = "fit_weight",
    units = "imperial"
  )
  expect_identical(fit$input_units, "imperial")
  expect_identical(fit$species, 122L)
  expect_lt(
    max(abs(fit$coefficients - taper_test_truth$max_burkhart)),
    0.02
  )
})

test_that("fit_taper accepts a complete natural-scale start", {
  data <- simulate_taper_test_data("max_burkhart")
  fit <- fit_taper(
    data, "max_burkhart", start = taper_test_truth$max_burkhart
  )
  expect_lt(
    max(abs(fit$coefficients - taper_test_truth$max_burkhart)),
    0.02
  )
})

test_that("fit_taper can fit a group random effect", {
  set.seed(731)
  data <- simulate_taper_test_data(
    "max_burkhart", noise = 0, seed = 731, tree_count = 32L
  )
  data$site <- rep(rep(letters[1:4], each = 8L), each = 12L)
  group_effect <- c(a = -0.12, b = -0.04, c = 0.04, d = 0.12)
  data$dib <- vapply(seq_len(nrow(data)), function(row) {
    coefficients <- taper_test_truth$max_burkhart
    coefficients[["b1"]] <- -3.2 +
      group_effect[[data$site[[row]]]]
    merchandiser:::.taper_evaluate(
      "max_burkhart", data$dbh[[row]], data$ht[[row]], data$h[[row]],
      coefficients
    )
  }, numeric(1)) + stats::rnorm(nrow(data), 0, 0.01)
  fit <- fit_taper(
    data, "max_burkhart", group = "site",
    weights = seq(0.9, 1.1, length.out = nrow(data))
  )
  expect_identical(fit$method, "nlme")
  expect_identical(fit$groups_seen, letters[1:4])
  expect_equal(nrow(fit$random_effects), 4L)
  expect_false(isTRUE(all.equal(
    predict(fit, re_form = "conditional"),
    predict(fit, re_form = "population")
  )))
  expect_length(
    predict(fit, group = rep(data$site, length.out = fit$n_measurements)),
    fit$n_measurements
  )
  prediction_data <- data.frame(
    dbh = c(30, 40), ht = c(20, 25), h = c(5, 10), site = c("a", "new")
  )
  expect_length(predict(fit, prediction_data, group = "site"), 2L)
  names(prediction_data)[names(prediction_data) == "site"] <- "group"
  expect_length(predict(fit, prediction_data), 2L)
})

test_that("fit_taper and fit methods reject invalid inputs", {
  data <- simulate_taper_test_data("max_burkhart")
  expect_error(fit_taper(1), "data frame")
  expect_error(fit_taper(data[-1], "max_burkhart"), "must contain")
  nonnumeric <- data
  nonnumeric$dbh <- as.character(nonnumeric$dbh)
  expect_error(fit_taper(nonnumeric, "max_burkhart"), "must be numeric")
  inconsistent <- data
  inconsistent$dbh[[2L]] <- inconsistent$dbh[[2L]] + 1
  expect_error(fit_taper(inconsistent, "max_burkhart"), "constant")
  bad_height <- data
  bad_height$h[[1L]] <- bad_height$ht[[1L]] + 1
  expect_error(fit_taper(bad_height, "max_burkhart"), "out-of-domain")
  expect_error(fit_taper(data, "max_burkhart", weights = 0), "above zero")
  expect_error(fit_taper(data, "max_burkhart", species = 1.5), "whole-number FIA")
  expect_error(fit_taper(data, "max_burkhart", species = "absent"),
    "whole-number FIA"
  )
  expect_error(fit_taper(data, "max_burkhart", weights = 1:2),
    "one value per row"
  )
  expect_error(
    fit_taper(data[1:6, ], "max_burkhart"),
    "more than 6 complete measurements"
  )
  bad_start <- taper_test_truth$max_burkhart[-1]
  expect_error(
    fit_taper(data, "max_burkhart", start = bad_start),
    "start must be"
  )
  zero <- data
  zero$dib <- 0
  expect_error(
    fit_taper(zero, "kozak_1988"),
    "could not produce finite start values"
  )
  expect_error(fit_taper(zero, "kozak_2002"), "failed to converge")
  expect_error(
    merchandiser:::.taper_natural_to_internal(
      replace(taper_test_truth$kozak_1988, 9, 0.4), "kozak_1988"
    ),
    "requires p"
  )
  expect_null(merchandiser:::.taper_weighted_lm(
    cbind(1, 1), c(1, 1), c(1, 1)
  ))
  degenerate <- data.frame(
    dbh = rep(30, 20), ht = rep(20, 20), h = rep(5, 20),
    dib = rep(20, 20), weight = rep(1, 20)
  )
  expect_length(merchandiser:::.taper_kozak_1988_starts(degenerate), 0L)
  bad_group <- data
  bad_group$site <- 1
  bad_group$site[[1L]] <- Inf
  expect_warning(
    prepared <- merchandiser:::.taper_prepare_data(
      bad_group, NULL, "site", NULL, "metric", "max_burkhart"
    ),
    "omitted 1"
  )
  expect_equal(nrow(prepared), nrow(data) - 1L)

  fit <- cached_taper_fit("max_burkhart")
  expect_error(predict(fit, data.frame(dbh = 30)), "dbh, ht, and h")
  expect_error(
    predict(fit, data.frame(dbh = "30", ht = 20, h = 5)),
    "must be numeric"
  )
  expect_error(
    predict(fit, data.frame(dbh = 30, ht = 20, h = 25)),
    "out-of-domain"
  )
  kozak_fit <- cached_taper_fit("kozak_2002")
  expect_error(
    predict(kozak_fit, data.frame(dbh = 30, ht = 1, h = 0.5)),
    "out-of-domain"
  )
  expect_error(as_taper_model(fit, "private.bad.species", species = 122.5),
    "whole-number FIA"
  )
  expect_error(merchandiser:::.taper_validate_fit(list()), "taper_fit")
  numeric_species <- taper_model_from_coefficients(
    "private.numeric.species", "max_burkhart",
    taper_test_truth$max_burkhart, species = c(122, 202)
  )
  expect_identical(numeric_species$species, c(122L, 202L))
  expect_error(
    taper_model_from_coefficients(
      "private.bad.species.zero", "max_burkhart",
      taper_test_truth$max_burkhart, species = 0
    ),
    "positive whole-number"
  )
  expect_error(
    taper_model_from_coefficients(
      "private.bad.species.fraction", "max_burkhart",
      taper_test_truth$max_burkhart, species = 122.5
    ),
    "whole-number"
  )
  expect_error(
    taper_model_from_coefficients(
      "private.bad.species.negative", "max_burkhart",
      taper_test_truth$max_burkhart, species = -122
    ),
    "positive whole-number"
  )
  expect_error(
    taper_model_from_coefficients(
      "private.bad.kozak.scale", "kozak_2002",
      replace(taper_test_truth$kozak_2002, 1, -1)
    ),
    "a0 above zero"
  )
  internal <- merchandiser:::.taper_natural_to_internal(
    taper_test_truth$max_burkhart, "max_burkhart"
  )
  expect_error(
    merchandiser:::.taper_fit_nlme(
      data.frame(
        dbh = 30, ht = 20, h = 5, dib = 20,
        weight = 2, group = factor("a")
      ),
      "max_burkhart", replace(internal, 1, NA_real_)
    ),
    "random-effects fit"
  )
})

test_that("fit_taper reports row omission and a one-level group fallback", {
  data <- simulate_taper_test_data("max_burkhart")
  data$dib[[1L]] <- NA_real_
  data$site <- "only"
  captured <- capture_warnings(
    fit_taper(data, "max_burkhart", group = "site")
  )
  fit <- captured$value
  expect_match(captured$messages[[1L]], "omitted 1 incomplete")
  expect_match(captured$messages[[2L]], "fewer than two levels")
  expect_identical(fit$n_measurements, 287L)
  expect_identical(fit$method, "nls")
})
