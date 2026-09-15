test_that("height form equations match published numeric anchors", {
  coefficients <- list(
    chapman_richards = c(la = log(95), lb = log(0.055), lc = log(1.25)),
    curtis = c(la = log(15), lb = log(0.6)), wykoff = c(a = log(100), lb = log(5)),
    naslund = c(
      la = log(0.08),
      lb = log(0.5)
    ), schumacher = c(la = log(100), lb = log(8))
  )
  expected <- c(
    chapman_richards = 36.912570383697769, curtis = 40.084072299257365,
    wykoff = 67.973641894028248,
    naslund = 63.671597633136088, schumacher = 49.432896411722155
  )
  actual <- vapply(names(coefficients), function(form) {
    merchandiser:::.height_evaluate(10, coefficients[[form]], form)
  }, numeric(1L))

  expect_equal(actual, expected, tolerance = 1e-12)
})

test_that("Chapman-Richards fixed and random parameters are recovered", {
  data <- height_test_data()
  fit <- height_test_fit()

  expect_s3_class(fit, "height_fit")
  expect_identical(fit$form, "chapman_richards")
  expect_named(fit, c(
    "data", "form", "fixed_effects", "variance_components", "groups_seen",
    "n_by_species",
    "pooled_species", "package_version", "measurement_system", "min_n", "models",
    "model_map",
    "internal_fixed_effects", "random_effects", "convergence_pooled_species"
  ))
  species <- fit$fixed_effects[fit$fixed_effects$spcd == 122L, , drop = FALSE]
  expect_equal(species$a, 95, tolerance = 0.2)
  expect_equal(species$b, 0.055, tolerance = 0.3)
  expect_equal(species$c, 1.25, tolerance = 0.3)
  variance <- fit$variance_components[fit$variance_components$spcd == 122L, , drop = FALSE]
  expect_equal(variance$random_effect_sd, 0.12, tolerance = 0.5)
  expect_equal(variance$residual_sd, 1.5, tolerance = 0.4)
  expect_setequal(fit$groups_seen, unique(data$group))
  expect_identical(fit$n_by_species$n, nrow(data))
  expect_identical(fit$package_version, tv_version())
  expect_output(print(fit), "<height_fit>")
})

test_that("each documented height form fits a constrained surface", {
  forms <- c("curtis", "wykoff", "naslund", "schumacher")
  for (form in forms) {
    data <- height_test_data(form, seed = match(form, forms) + 900)
    fit <- fit_height(
      dbh = data$dbh, ht = data$ht, spcd = data$spcd, group = data$group,
      form = form
    )
    prediction <- predict_height(
      fit = fit, dbh = c(0.01, 5, 20, 40), spcd = 122L,
      re_form = "population"
    )
    expect_true(all(is.finite(prediction)), info = form)
    expect_true(all(prediction >= 4.5), info = form)
    expect_identical(fit$form, form)
  }
})

test_that("species below min_n use the pooled model", {
  data <- height_test_data()
  small <- data[seq_len(6), ]
  small$spcd <- 202L
  combined <- rbind(data, small)
  fit <- fit_height(
    dbh = combined$dbh, ht = combined$ht, spcd = combined$spcd, group = combined$group,
    min_n = 7
  )

  expect_identical(fit$pooled_species, 202L)
  expect_true(fit$fixed_effects$pooled[fit$fixed_effects$spcd == 202L])
  expect_identical(unname(fit$model_map[["202"]]), "pooled")
  expect_identical(unname(fit$model_map[["122"]]), "spcd_122")
  expect_true(is.finite(predict_height(fit = fit, dbh = 12, spcd = 202L, group = "plot_1")))
})

test_that("a failed species fit falls back after alternate starts", {
  data <- height_test_data()
  degenerate <- data.frame(dbh = rep(10, 7), ht = seq(30, 36), spcd = 202L, group = rep(
    "flat",
    7
  ), stringsAsFactors = FALSE)
  combined <- rbind(data[, c("dbh", "ht", "spcd", "group")], degenerate)
  warnings <- capture_warnings(fit_height(
    dbh = combined$dbh, ht = combined$ht, spcd = combined$spcd,
    group = combined$group
  ))
  fit <- warnings$value
  expect_match(warnings$messages, "species 202.*chapman_richards.*pooled fit was used")
  expect_identical(fit$convergence_pooled_species, 202L)
  expect_identical(unname(fit$model_map[["202"]]), "pooled")
})

test_that("conditional and population predictions handle groups", {
  fit <- height_test_fit()
  known <- predict_height(fit = fit, dbh = 15, spcd = 122L, group = "plot_1")
  population <- predict_height(
    fit = fit, dbh = 15, spcd = 122L, group = "plot_1",
    re_form = "population"
  )
  unseen <- predict_height(fit = fit, dbh = 15, spcd = 122L, group = "not_seen")
  missing <- predict_height(fit = fit, dbh = 15, spcd = 122L, group = NA_character_)
  no_group <- predict_height(fit = fit, dbh = 15, spcd = 122L)

  expect_false(isTRUE(all.equal(known, population)))
  expect_identical(unseen, population)
  expect_identical(missing, population)
  expect_identical(no_group, population)
})

test_that("simulated intervals have stated coverage", {
  fit <- height_test_fit()
  set.seed(322)
  size <- 800L
  dbh <- runif(size, 3, 30)
  random_effect <- rnorm(size, 0, 0.12)
  truth <- merchandiser:::.height_evaluate(
    dbh, c(la = log(95), lb = log(0.055), lc = log(1.25)),
    "chapman_richards", random_effect
  ) + rnorm(size, 0, 1.5)
  interval <- predict_height(
    fit = fit, dbh = dbh, spcd = 122L, re_form = "population", interval = TRUE,
    level = 0.95, nsim = 500, seed = 42
  )
  coverage <- mean(truth >= interval$lower & truth <= interval$upper)

  expect_named(interval, c("fit", "lower", "upper"))
  expect_equal(nrow(interval), size)
  expect_gte(coverage, 0.90)
  expect_lte(coverage, 0.99)
})

test_that("interval streams are reproducible and thread invariant", {
  fit <- height_test_fit()
  dbh <- seq(5, 25, length.out = 25)
  one <- with_threads(1, predict_height(
    fit = fit, dbh = dbh, spcd = 122L, group = "plot_2",
    interval = TRUE, nsim = 250, seed = 123
  ))
  four <- with_threads(4, predict_height(
    fit = fit, dbh = dbh, spcd = 122L, group = "plot_2",
    interval = TRUE, nsim = 250, seed = 123
  ))
  changed <- predict_height(
    fit = fit, dbh = dbh, spcd = 122L, group = "plot_2", interval = TRUE,
    nsim = 250, seed = 124
  )

  expect_identical(four, one)
  expect_false(identical(changed$lower, one$lower))
  set.seed(88)
  before <- .Random.seed
  predict_height(fit = fit, dbh = 10, spcd = 122L, interval = TRUE, nsim = 20, seed = 7)
  expect_identical(.Random.seed, before)
})

test_that("metric fitting and prediction round trip at the boundary", {
  data <- height_test_data()
  imperial <- height_test_fit()
  metric <- fit_height(
    dbh = data$dbh * 2.54 / 2.54, ht = data$ht * 0.3048 / 0.3048, spcd = data$spcd,
    group = data$group
  )
  diameter <- c(8, 16, 24)
  expected <- predict_height(fit = imperial, dbh = diameter, spcd = 122L, group = c(
    "plot_1",
    "plot_2", "new"
  ))
  actual <- predict_height(
    fit = metric, dbh = diameter * 2.54 / 2.54, spcd = 122L,
    group = c(
      "plot_1",
      "plot_2", "new"
    )
  ) * 0.3048

  expect_equal(actual / 0.3048, expected, tolerance = 1e-9)
  metric_interval <- predict_height(
    fit = metric, dbh = diameter * 2.54 / 2.54, spcd = 122L,
    interval = TRUE, nsim = 100, seed = 5
  ) * 0.3048
  imperial_interval <- predict_height(
    fit = imperial, dbh = diameter, spcd = 122L, interval = TRUE,
    nsim = 100, seed = 5
  )
  expect_equal(metric_interval / 0.3048, imperial_interval, tolerance = 1e-9)
})

test_that("complete_heights changes only missing entries", {
  data <- height_test_data()
  fit <- height_test_fit()
  observed <- data$ht
  observed[c(2, 50)] <- NA_real_
  completed <- complete_heights(
    dbh = data$dbh, ht = observed, spcd = data$spcd, group = data$group,
    fit = fit
  )

  expect_identical(completed[!is.na(observed)], observed[!is.na(observed)])
  expect_true(all(is.finite(completed[c(2, 50)])))
  expect_warning(
    automatic <- complete_heights(
      dbh = data$dbh, ht = observed, spcd = data$spcd,
      group = data$group
    ),
    "missing or nonfinite.*omitted"
  )
  expect_true(all(is.finite(automatic)))
})

test_that("height inputs follow recycling, NA, and warning conventions", {
  fit <- height_test_fit()
  warnings <- capture_warnings(predict_height(fit = fit, dbh = c(NA, 0, 10, 10), spcd = c(
    122,
    122, 0, 999
  ), group = "plot_1"))
  expect_true(is.na(warnings$value[[1L]]))
  expect_true(all(is.na(warnings$value[2:4])))
  expect_length(warnings$messages, 2L)
  expect_match(warnings$messages[[1L]], "invalid_dbh")
  expect_match(warnings$messages[[2L]], "unknown_species.*2 of 4")

  data <- height_test_data()
  data$dbh[c(1, 2)] <- c(NA, 0)
  data$spcd[[3]] <- 0
  fit_warnings <- capture_warnings(fit_height(
    dbh = data$dbh, ht = data$ht, spcd = data$spcd,
    group = data$group
  ))
  fitted <- fit_warnings$value
  expect_true(any(grepl("invalid_dbh", fit_warnings$messages)))
  expect_true(any(grepl("unknown_species", fit_warnings$messages)))
  expect_s3_class(fitted, "height_fit")
})

test_that("height functions define explicit zero-length behavior", {
  fit <- height_test_fit()
  expect_identical(predict_height(fit = fit, dbh = numeric(), spcd = numeric()), numeric())
  interval <- predict_height(
    fit = fit, dbh = numeric(), spcd = numeric(), interval = TRUE,
    nsim = 1
  )
  expect_named(interval, c("fit", "lower", "upper"))
  expect_equal(nrow(interval), 0L)
  expect_identical(
    complete_heights(dbh = numeric(), ht = numeric(), spcd = numeric()), numeric()
  )
  expect_error(
    fit_height(dbh = numeric(), ht = numeric(), spcd = numeric()),
    "no complete.*found 0"
  )
})

test_that("height controls and types fail clearly", {
  fit <- height_test_fit()
  data <- height_test_data()
  expect_error(
    fit_height(dbh = data$dbh, ht = data$ht, spcd = data$spcd, form = "Chapman"),
    "form"
  )
  expect_error(fit_height(
    dbh = data$dbh, ht = data$ht, spcd = data$spcd,
    min_n = 0
  ), "min_n")
  expect_error(
    fit_height(dbh = "ten", ht = data$ht, spcd = data$spcd),
    "dbh must be a numeric"
  )
  expect_error(fit_height(dbh = 1:2, ht = 1:3, spcd = 122), "size-one")
  expect_error(
    fit_height(dbh = data$dbh, ht = data$ht, spcd = data$spcd, group = list(1)),
    "group"
  )
  expect_error(predict_height(fit = list(), dbh = 10, spcd = 122), "height_fit")
  expect_error(predict_height(fit = fit, dbh = 10, spcd = 122, re_form = "all"), "re_form")
  expect_error(predict_height(fit = fit, dbh = 10, spcd = 122, interval = NA), "interval")
  expect_error(predict_height(fit = fit, dbh = 10, spcd = 122, level = 1), "level")
  expect_error(predict_height(fit = fit, dbh = 10, spcd = 122, nsim = 0), "nsim")
  expect_error(predict_height(fit = fit, dbh = 10, spcd = 122, seed = -1), "seed")
  expect_error(
    predict_height(fit, 10, 122, measurement_system = "Metric"), "measurement_system"
  )
  expect_error(
    complete_heights(dbh = 10, ht = NA_real_, spcd = 122, fit = fit, mystery = 1),
    "unused"
  )
  expect_error(complete_heights(dbh = 10, ht = 30, spcd = 122, fit = list()), "height_fit")
  expect_error(complete_heights(dbh = 10, ht = 30, spcd = 122, form = "Chapman"), "form")
  expect_error(complete_heights(dbh = 10, ht = 30, spcd = 122, min_n = 0), "min_n")
  expect_error(complete_heights(dbh = 10, ht = 30, spcd = 122, re_form = "all"), "re_form")
  expect_error(merchandiser:::.height_completion_dots(list(1)), "unique")
  expect_error(fit_height(dbh = 1:7, ht = NULL, spcd = 122), "ht.*size 0.*size.*7")
})

test_that("pooled convergence errors name species and form", {
  expect_error(fit_height(dbh = rep(10, 8), ht = seq(20, 27), spcd = 122L, group = rep(
    "only",
    8
  ), form = "chapman_richards"), "species pooled.*chapman_richards.*failed to converge")
})

test_that("internal height validation and RNG fallbacks are covered", {
  expect_equal(nrow(merchandiser:::.height_bind_rows(list())), 0L)
  linear <- merchandiser:::.height_linear_start(rep(1, 3), rep(1, 3), 2, 3)
  expect_identical(linear, c(2, 3))
  numeric_group <- merchandiser:::.height_prepare(1, 122, group = Inf)
  expect_true(is.na(numeric_group$group))

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  saved_seed <- if (had_seed)
    .Random.seed else NULL
  if (had_seed) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  state <- merchandiser:::.height_rng_state(1L)
  merchandiser:::.height_restore_rng(state)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  if (had_seed) {
    assign(".Random.seed", saved_seed, envir = .GlobalEnv)
  }

  no_seed_interval <- predict_height(
    fit = height_test_fit(), dbh = 10, spcd = 122L, interval = TRUE,
    nsim = 20
  )
  expect_true(all(is.finite(unlist(no_seed_interval))))
})
