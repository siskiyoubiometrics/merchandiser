test_that("all fixture equations and Flewelling patterns resolve", {
  metadata <- utils::read.csv(
    system.file("extdata", "flewelling_models.csv", package = "merchandiser"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(metadata), 181L)
  expect_equal(sum(metadata$family == "flewelling_2pt"), 154L)
  expect_equal(sum(metadata$family == "flewelling_3pt"), 27L)
  expect_true(all(has_taper_model(metadata$id)))

  expect_true(has_taper_model("F09FW2W202"))
  expect_identical(get_taper_model("F09FW2W202")$family, "flewelling_2pt")
  expect_true(has_taper_model("A03F33W098"))
  expect_identical(get_taper_model("A03F33W098")$family, "flewelling_3pt")
})
test_that("three-point inputs and recommendation are frozen", {
  model <- get_taper_model("A00FW3W098")
  expect_identical(model$inputs$required, c("upper_ht1", "upper_d1"))
  expect_identical(
    model$inputs$optional,
    c("upper_ht2", "upper_d2", "upper_bark", "bark_ratio")
  )
  expect_match(model$notes, "two-point Flewelling form is recommended")

  missing <- dib(12, 80, 30, "A00FW3W098", status = TRUE)
  expect_identical(missing$status, 51L)
  incomplete <- dib(
    12, 80, 30, "A00FW3W098", upper_ht1 = 40, status = TRUE
  )
  expect_identical(incomplete$status, 51L)
  complete <- dib(
    12, 80, 30, "A00FW3W098", upper_ht1 = 40, upper_d1 = 8,
    status = TRUE
  )
  expect_identical(complete$status, 0L)
  expect_true(is.finite(complete$value))

  incomplete_second <- dib(
    12, 80, 40, "A00FW3W098", upper_ht1 = 30, upper_d1 = 9,
    upper_ht2 = 50, status = TRUE
  )
  expect_identical(incomplete_second$status, 51L)
  two_upper_points <- dib(
    12, 80, 40, "A00FW3W098", upper_ht1 = 30, upper_d1 = 9,
    upper_ht2 = 50, upper_d2 = 6, upper_bark = "ib", status = TRUE
  )
  expect_identical(two_upper_points$status, 0L)
  expect_true(is.finite(two_upper_points$value))
  outside_bark_upper <- dib(
    12, 80, 40, "A00FW3W098", upper_ht1 = 30, upper_d1 = 9,
    upper_bark = "ob", status = TRUE
  )
  expect_identical(outside_bark_upper$status, 53L)
})

test_that("three-point measurements convert from metric units", {
  imperial <- dib(
    12, 80, 40, "A00FW3W098", upper_ht1 = 30, upper_d1 = 9
  )
  metric <- dib(
    12 * 2.54, 80 * 0.3048, 40 * 0.3048, "A00FW3W098",
    upper_ht1 = 30 * 0.3048, upper_d1 = 9 * 2.54, units = "metric"
  )
  expect_equal(metric, imperial * 2.54, tolerance = 1e-12)
})

test_that("geographic subregion characters select adjustment tables", {
  west_global <- dib(12, 80, 40, "F00FW2W202")
  west_coast <- dib(12, 80, 40, "F01FW2W202")
  ingy_global <- dib(12, 80, 40, "I00FW2W122")
  ingy_subregion <- dib(12, 80, 40, "I11FW2W122")

  expect_false(isTRUE(all.equal(west_global, west_coast)))
  expect_false(isTRUE(all.equal(ingy_global, ingy_subregion)))
  expect_equal(
    dib(12, 80, 40, "FOOFW2W202"),
    west_global,
    tolerance = 1e-14
  )
})

test_that("library failures become offset statuses", {
  identifiers <- sprintf("F%02dFW2W260", 0:8)
  result <- dib(12, 80, 40, identifiers, status = TRUE)
  expect_true(all(is.na(result$value)))
  expect_true(all(result$status == 301L))
})

test_that("Flewelling operations dispatch through the compiled registry", {
  id <- "F00FW2W202"
  at_height <- dib(12, 80, 30, id)
  outside <- dob(12, 80, 30, id)
  expect_true(is.finite(at_height))
  expect_gt(outside, at_height)
  expect_equal(height_at_dib(12, 80, at_height, id), 30, tolerance = 1e-4)
  expect_equal(height_at_dob(12, 80, outside, id), 30, tolerance = 1e-4)
  expect_true(is.finite(stem_volume(12, 80, id)))

  profile <- stem_profile(12, 40, id, step = 60)
  expect_true(nrow(profile) > 2L)
  expect_true(all(profile$status %in% c(0L, 102L)))
})
