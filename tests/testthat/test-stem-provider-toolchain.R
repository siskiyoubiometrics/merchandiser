test_that("the compiled treevolume header probe evaluates a kernel", {
  diameter <- merchandiser:::treevolume_toolchain_check()
  expect_type(diameter, "double")
  expect_equal(diameter, 10)
})

test_that("the compiled provider preserves basal-area units", {
  model <- "900CLKE001"
  session <- merchandiser:::.mc_open_provider(model)
  on.exit(merchandiser:::mc_provider_close(session$pointer), add = TRUE)
  area_factor <- 0.09290304 / 0.40468564224
  aux <- list(
    site_index = 100 * 0.3048, basal_area = 120 * area_factor,
    bark_ratio = 0.9
  )
  compiled <- merchandiser:::mc_provider_query(
    session$pointer, 10 * 2.54, 80 * 0.3048, 1L, aux, 1L,
    40 * 0.3048, 2L
  )
  public <- merchandiser::dib(
    10 * 2.54, 80 * 0.3048, 40 * 0.3048, model,
    site_index = aux$site_index, basal_area = aux$basal_area,
    bark_ratio = aux$bark_ratio,
    units = "metric", status = TRUE
  )
  expect_identical(compiled$status, public$status)
  expect_equal(compiled$dib, public$value, tolerance = 1e-12)
})

test_that("the compiled provider accepts an empty auxiliary list", {
  model <- "900CLKE001"
  session <- merchandiser:::.mc_open_provider(model)
  on.exit(merchandiser:::mc_provider_close(session$pointer), add = TRUE)
  crossings <- merchandiser:::mc_provider_crossings(
    session$pointer, 12, 80, 1L, list(), 4, 0L, 1, 80, 1L
  )
  expect_identical(crossings$status, 0L)
  expect_true(length(crossings$roots) > 0L)
})

test_that("R fallback returns every discovered diameter crossing", {
  model_id <- "merch.test.multiple-crossings"
  if (merchandiser::has_taper_model(model_id)) merchandiser::unregister_taper_model(model_id)
  on.exit({
    gc()
    if (merchandiser::has_taper_model(model_id)) merchandiser::unregister_taper_model(model_id)
  }, add = TRUE)
  profile <- function(dbh, ht, h, aux) {
    below <- 10 + 2 * sin(pi * h)
    above <- 10 * pmax(ht - h, 0) / (ht - 4.5)
    as.double(ifelse(h < 4.5, below, above))
  }
  model <- merchandiser::new_taper_model(
    model_id, "test", profile, units = "imperial", stump_ht = 0,
    bark_ratio = 1, source = "merchandiser all-crossing test"
  )
  merchandiser::register_taper_model(model)
  roots <- merchandiser:::.mc_r_crossing_basis(
    list(
      dbh = 12, ht = 20, model = model_id,
      units = "imperial", status = TRUE
    ),
    target = 10, lower = 0, upper = 10, basis = "ib"
  )[[1L]]
  expect_identical(attr(roots, "status"), 0L)
  expect_equal(as.double(roots), c(0, 1, 2, 3, 4, 4.5), tolerance = 1e-4)
})

test_that("crossing and profile passes do not grow by tree or log", {
  product <- .mc_legacy_product(
    "log", 1L, min_length = 8, max_length = 16, length_step = 1,
    min_boundary_length = 8, trim = 0.5, min_sed = 3,
    diameter_basis = "ib", scale_rule = "huber", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib"
  )
  calls <- new.env(parent = emptyenv())
  calls$crossings <- 0L
  calls$profile <- 0L
  original_crossings <- merchandiser:::.mc_all_crossings
  original_profile <- merchandiser:::.mc_query_points
  testthat::local_mocked_bindings(
    .mc_all_crossings = function(...) {
      calls$crossings <- calls$crossings + 1L
      original_crossings(...)
    },
    .mc_query_points = function(...) {
      calls$profile <- calls$profile + 1L
      original_profile(...)
    },
    .package = "merchandiser"
  )
  merchandise(
    rep(12, 3), rep(60, 3), rep("demo.paraboloid.r", 3), product,
    status = TRUE
  )
  expect_identical(calls$crossings, 1L)
  expect_identical(calls$profile, 1L)
})

test_that("the compiled provider converts green weight in both unit systems", {
  volume <- c(1, 2)
  imperial <- merchandiser:::mc_provider_green_weight(
    volume, c(202L, 202L), c(0L, 1L), 1L
  )
  metric <- merchandiser:::mc_provider_green_weight(
    volume * 0.028316846592, c(202L, 202L), c(0L, 1L), 2L
  )
  expect_identical(imperial$status, c(0L, 0L))
  expect_identical(metric$status, c(0L, 0L))
  expect_equal(metric$value, imperial$value * 0.45359237, tolerance = 1e-12)
  expect_error(
    merchandiser:::mc_provider_green_weight(
      volume, 202L, c(0L, 1L), 1L
    ),
    "inconsistent sizes"
  )
})
