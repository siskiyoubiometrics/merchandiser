test_that("representative regional profiles are finite and reach zero", {
  ids <- c(
    "A16DEMW042", "A16DEMW098", "A16DEMW242", "A16DEMW351",
    "400MATW015", "400MATW202", "403MATW122", "407MATW093"
  )
  heights <- rep(seq(1, 80, length.out = 101L), length(ids))
  models <- rep(ids, each = 101L)
  values <- dib(12, 80, heights, models, status = TRUE)
  expect_true(all(values$status == 0L))
  expect_true(all(is.finite(values$value) & values$value >= 0))
  expect_identical(values$value[heights == 80], rep(0, length(ids)))
})

test_that("regional inverse paths reproduce profile targets", {
  ids <- rep(c("A16DEMW098", "A16DEMW351", "400MATW202"), each = 4L)
  heights <- rep(c(10, 25, 50, 70), 3L)
  targets <- dib(12, 80, heights, ids)
  recovered <- height_at_dib(12, 80, targets, ids, status = TRUE)
  expect_true(all(recovered$status %in% c(0L, 102L)))
  # R10HTS stops on its source tolerance; R4MATTAPER is analytic.
  expect_equal(recovered$value, heights, tolerance = 0.02)
})

test_that("regional compatibility is scoped to ruled inverse differences", {
  ordinary <- height_at_dib(4, 15, 2, "A01CURW000", status = TRUE)
  source_ordinary <- .with_treevolume_compat(
    "nvel", height_at_dib(4, 15, 2, "A01CURW000", status = TRUE)
  )
  expect_identical(source_ordinary, ordinary)
  expect_identical(source_ordinary$status, 102L)

  bru <- height_at_dib(4, 15, 2, "A01BRUW202", status = TRUE)
  source_bru <- .with_treevolume_compat(
    "nvel", height_at_dib(4, 15, 2, "A01BRUW202", status = TRUE)
  )
  expect_identical(source_bru, bru)
  expect_identical(source_bru$status, 102L)

  outside_interval <- height_at_dib(
    10, 80, 9, "400MATW015", status = TRUE
  )
  source_outside_interval <- .with_treevolume_compat(
    "nvel", height_at_dib(10, 80, 9, "400MATW015", status = TRUE)
  )
  expect_identical(source_outside_interval, outside_interval)
  expect_identical(source_outside_interval$status, 101L)
})

test_that("Region 10 BRU profiles are deterministic", {
  id <- "A16BRUW042"
  heights <- c(1, 10, 30, 60, 80)
  first <- dib(12, 80, heights, id, status = TRUE)
  second <- dib(12, 80, heights, id, status = TRUE)
  expect_identical(first, second)
  expect_identical(first$status, integer(length(heights)))
  expect_true(all(is.finite(first$value) & first$value >= 0))

  inverse <- height_at_dib(
    12, 80, first$value[2:4], id, status = TRUE
  )
  expect_true(all(inverse$status %in% c(0L, 102L)))
  expect_equal(inverse$value, heights[2:4], tolerance = 0.02)

  first_volume <- stem_volume(
    12, 80, id, lower = 0, lower_type = "height",
    upper = 0, upper_type = "tip", status = TRUE
  )
  second_volume <- stem_volume(
    12, 80, id, lower = 0, lower_type = "height",
    upper = 0, upper_type = "tip", status = TRUE
  )
  expect_identical(first_volume, second_volume)
  expect_identical(first_volume$status, 0L)
  expect_true(is.finite(first_volume$value) && first_volume$value > 0)
})

test_that("Region 4 bounded volume is additive", {
  id <- "400MATW202"
  complete <- stem_volume(
    12, 80, id, lower = 1, lower_type = "height",
    upper = 70, upper_type = "height"
  )
  lower <- stem_volume(
    12, 80, id, lower = 1, lower_type = "height",
    upper = 35, upper_type = "height"
  )
  upper <- stem_volume(
    12, 80, id, lower = 35, lower_type = "height",
    upper = 70, upper_type = "height"
  )
  expect_equal(complete, lower + upper, tolerance = 1e-12)
})

test_that("regional kernels are independent of thread count", {
  ids <- rep(c("A16DEMW098", "400MATW202"), each = 200L)
  heights <- rep(seq(1, 79, length.out = 200L), 2L)
  one <- with_threads(1, dib(12, 80, heights, ids))
  four <- with_threads(4, dib(12, 80, heights, ids))
  expect_identical(one, four)
})
