test_that("all exact Clark equations and pattern families resolve", {
  metadata <- utils::read.csv(
    system.file(
      "extdata", "clark_models.csv", package = "merchandiser",
      mustWork = TRUE
    ),
    stringsAsFactors = FALSE, colClasses = c(id = "character")
  )
  expect_equal(nrow(metadata), 2824L)
  expect_equal(sum(metadata$family == "clark_r8"), 2156L)
  expect_equal(sum(metadata$family == "clark_r9"), 668L)
  expect_true(all(has_taper_model(metadata$id)))
  expect_identical(get_taper_model("814CLKE100")$family, "clark_r8")
  expect_identical(get_taper_model("811CLKE100")$family, "clark_r9")
  expect_identical(get_taper_model("900CLKE001")$family, "clark_r9")

  expect_true(has_taper_model("899CLKE100"))
  expect_identical(get_taper_model("899CLKE100")$family, "clark_r8")
  expect_true(has_taper_model("999CLKE001"))
  expect_identical(get_taper_model("999CLKE001")$family, "clark_r9")
})

test_that("Clark metadata freezes capabilities and auxiliaries", {
  model <- get_taper_model("814CLKE100")
  expect_false(model$kernel$has_dob)
  expect_true(model$kernel$has_inverse)
  expect_true(model$kernel$has_integral)
  expect_identical(model$inputs$required, character())
  expect_identical(
    model$inputs$optional,
    c("upper_ht1", "site_index", "basal_area", "bark_ratio")
  )
  expect_identical(model$stump_ht, 1)
  expect_true(is.na(model$bark_ratio))
  expect_identical(model$units, "imperial")
})

test_that("Clark outside bark is derived only with a caller ratio", {
  id <- "900CLKE001"
  inside <- dib(12, 80, 40, id)
  missing <- dob(12, 80, 40, id, status = TRUE)
  outside <- dob(12, 80, 40, id, bark_ratio = 0.9)
  expect_identical(missing$status, 53L)
  expect_true(is.na(missing$value))
  expect_equal(outside, inside / 0.9, tolerance = 1e-13)
})

test_that("Clark source inverse retains the below-FIXDI dispatch", {
  port <- height_at_dib(6, 100, 3, "834CLKE110", status = TRUE)
  source <- .with_treevolume_compat(
    "nvel", height_at_dib(6, 100, 3, "834CLKE110", status = TRUE)
  )

  expect_identical(source, port)
  expect_identical(source$status, 0L)
})

test_that("Clark compatibility changes only the added-root case", {
  port <- height_at_dib(
    6, 100, 5, "834CLKE110", upper_ht1 = 75, status = TRUE
  )
  source <- .with_treevolume_compat(
    "nvel",
    height_at_dib(
      6, 100, 5, "834CLKE110", upper_ht1 = 75, status = TRUE
    )
  )
  expect_identical(port$status, 102L)
  expect_equal(source$value, 22.98306, tolerance = 1e-6)
  expect_identical(source$status, 0L)

  below_stump <- height_at_dib(
    4, 15, 2, "814CLKE100", upper_ht1 = 11.25, status = TRUE
  )
  source_below_stump <- .with_treevolume_compat(
    "nvel",
    height_at_dib(
      4, 15, 2, "814CLKE100", upper_ht1 = 11.25, status = TRUE
    )
  )
  expect_identical(source_below_stump, below_stump)
  expect_identical(source_below_stump$status, 100L)
})

test_that("Clark driver auxiliaries are accepted and path scoped", {
  baseline <- dib(12, 80, 40, "900CLKE001")
  supplied <- dib(
    12, 80, 40, "900CLKE001", site_index = 60, basal_area = 80
  )
  expect_identical(supplied, baseline)

  explicit <- dib(12, 80, 40, "814CLKE100", upper_ht1 = 60)
  expect_true(is.finite(explicit))
  derived <- dib(12, 80, 40, "814CLKE100")
  expect_true(is.finite(derived))
})

test_that("Clark operations dispatch through compiled kernels", {
  cases <- data.frame(
    id = c("814CLKE100", "818CLKE300", "811CLKE100", "900CLKE001"),
    upper = c(60, NA, NA, NA), stringsAsFactors = FALSE
  )
  for (row in seq_len(nrow(cases))) {
    arguments <- list(
      dbh = 12, ht = 80, h = 40, model = cases$id[[row]]
    )
    if (is.finite(cases$upper[[row]])) {
      arguments$upper_ht1 <- cases$upper[[row]]
    }
    at_height <- do.call(dib, arguments)
    expect_true(is.finite(at_height), info = cases$id[[row]])
    inverse_arguments <- list(
      dbh = 12, ht = 80, dib = at_height, model = cases$id[[row]],
      status = TRUE
    )
    volume_arguments <- list(
      dbh = 12, ht = 80, model = cases$id[[row]], status = TRUE
    )
    if (is.finite(cases$upper[[row]])) {
      inverse_arguments$upper_ht1 <- cases$upper[[row]]
      volume_arguments$upper_ht1 <- cases$upper[[row]]
    }
    recovered <- do.call(height_at_dib, inverse_arguments)
    total <- do.call(stem_volume, volume_arguments)
    expect_true(recovered$status %in% c(0L, 102L), info = cases$id[[row]])
    expect_equal(recovered$value, 40, tolerance = 1e-8)
    expect_identical(total$status, 0L)
    expect_gte(total$value, 0)
  }
})
