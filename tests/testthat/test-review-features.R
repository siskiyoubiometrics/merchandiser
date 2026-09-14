test_that("merchandise plots preserve species inputs and ground geometry", {
  model_id <- "review.species_plot"
  register_taper_model(new_taper_model(
    model_id, "user", dib = function(dbh, ht, h, aux) {
      if (is.null(aux$spcd)) return(rep(NA_real_, length(dbh)))
      dbh * sqrt(pmax(0, (ht - h) / (ht - 4.5)))
    }, inputs = list(required = "spcd", optional = character(), pairs = list())
  ))
  on.exit(unregister_taper_model(model_id), add = TRUE)
  saw <- .mc_legacy_product("saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
                            scale_rule = "cubic", measurement_quantity = "cubic",
                            scale_unit = "ft3",
                            scale_bark_basis = "ib")
  x <- merchandise(c(12, 14), 60, model_id, saw, spcd = c(202, 263),
                   id = factor(c("a", "b")), stump_ht = 0, status = TRUE)
  expect_identical(x$trees$status, c(0L, 0L))
  profile <- .mc_plot_profile(x, 2)
  expect_true(all(is.finite(profile$dib)))
  expect_equal(range(profile$h), c(0, 60))
  expect_equal(profile$dib[1], 14 * sqrt(60 / 55.5))

  drawn <- list()
  local_mocked_bindings(
    lines = function(x, y, ...) drawn[[length(drawn) + 1L]] <<- list(x = x, y = y),
    .package = "graphics"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  expect_invisible(plot(x, trees = x$trees$id[2]))
  expect_length(drawn, 2L)
  expect_equal(drawn[[1]]$x, profile$h)
  expect_equal(drawn[[2]]$y - drawn[[1]]$y, profile$dib)
  drawn <- list()
  expect_invisible(plot(profile))
  expect_length(drawn, 1L)
  expect_equal(drawn[[1]]$x, profile$dib)
})

test_that("stand grouping cannot shadow summary quantities", {
  saw <- .mc_legacy_product("saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
                            scale_rule = "cubic", measurement_quantity = "cubic",
                            scale_unit = "ft3",
                            scale_bark_basis = "ib")
  x <- merchandise(12, 60, "demo.paraboloid", saw, id = "a", status = TRUE)
  trees <- data.frame(id = "a", dbh = 12, spcd = 202)
  for (name in c("trees_per_acre", "logs_per_acre", "net_scale_ft3_per_acre")) {
    trees[[name]] <- "label"
    expect_error(stand_table(x, trees, 1, by = name), "summary output column")
  }
})

test_that("height plots draw population curves in the requested units", {
  measured <- example_trees_pnw[!is.na(example_trees_pnw$ht_observed), ]
  fit <- with(measured, fit_height(dbh, ht_observed, spcd, group = plot, min_n = 50))
  drawn <- list()
  local_mocked_bindings(
    lines = function(x, y, ...) drawn[[length(drawn) + 1L]] <<- list(x = x, y = y),
    .package = "graphics"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  expect_invisible(plot(fit, units = "metric"))
  species <- unique(fit$data$spcd)
  expect_length(drawn, length(species))
  for (index in seq_along(species)) {
    dbh <- fit$data$dbh[fit$data$spcd == species[index]]
    expect_equal(range(drawn[[index]]$x), range(dbh) * 2.54)
    expect_equal(drawn[[index]]$y, predict_height(
      fit, drawn[[index]]$x, species[index], re_form = "population", units = "metric"
    ))
  }
})
