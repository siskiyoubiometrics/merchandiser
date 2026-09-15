test_that("pinned Scribner and International routines retain discrete behavior", {
  expect_equal(merchandiser:::.mc_scribner(10, 16, TRUE), trunc((16 *
    merchandiser:::.mc_scribner_factor[
      125
    ] +
    5) / 10) * 10)
  expect_equal(merchandiser:::.mc_scribner(10, 16, FALSE), trunc(16 *
    merchandiser:::.mc_scribner_factor[
      125
    ] +
    0.5))
  expect_equal(merchandiser:::.mc_intl14(3, 16), 0)
  expect_true(merchandiser:::.mc_intl14(10, 16) %% 5 == 0)
})

test_that("NVEL segmentation ports exercise every declared option family", {
  expect_equal(merchandiser:::.mc_nvel_segments(40, "split_20"), c(20, 20))
  expect_equal(sum(merchandiser:::.mc_nvel_segments(35, "allocated_20")), 36)
  expect_equal(sum(merchandiser:::.mc_nvel_segments(60, "whole_40")), 60)
  for (option in c(11:14, 21:24)) {
    maximum <- c(`11` = 16, `12` = 20, `13` = 32, `14` = 40)[as.character(option)]
    if (is.na(maximum))
      maximum <- 16
    number <- merchandiser:::.mc_numlog(option, 2L, 45, maximum, 2, 0.5)
    logs <- merchandiser:::.mc_segmnt(option, 2L, 45, maximum, 2, 0.5, number)
    expect_lte(length(logs), 20L)
    expect_true(all(logs >= 0))
  }
})

test_that("every rounding operator is exhaustive at its decision boundary", {
  cases <- data.frame(
    operator = c(
      "truncate_1in", "nearest_1in_half_up", "nearest_0.5in_half_up",
      "truncate_1cm", "nearest_1cm_half_up", "truncate_1ft", "nearest_1ft_half_up",
      "truncate_0.1m",
      "nearest_0.1m_half_up", "truncate_board_foot", "nearest_board_foot_half_up",
      "nearest_10_board_feet_half_up"
    ),
    dimension = c(
      rep("diameter", 5), rep("length", 4), rep("volume", 3)
    ), measurement_system = c(
      "imperial",
      "imperial", "imperial", "metric", "metric", "imperial", "imperial", "metric",
      "metric",
      "imperial", "imperial", "imperial"
    ), threshold = c(
      10, 10.5, 10.25, 10, 10.5, 10,
      10.5, 1, 1.05, 10, 10.5, 15
    ), below = c(9, 10, 10, 9, 10, 9, 10, 0.9, 1, 9, 10, 10),
    exact = c(10, 11, 10.5, 10, 11, 10, 11, 1, 1.1, 10, 11, 20), above = c(
      10, 11, 10.5,
      10, 11, 10, 11, 1, 1.1, 10, 11, 20
    ), stringsAsFactors = FALSE
  )
  values <- as.vector(vapply(cases$threshold, function(x) {
    c(x - 1e-09, x, x + 1e-09)
  }, numeric(3L)))
  expected <- as.vector(t(as.matrix(cases[c("below", "exact", "above")])))
  rows <- rep(seq_len(nrow(cases)), each = 3L)
  actual_r <- vapply(seq_along(values), function(at) {
    row <- rows[at]
    merchandiser:::.mc_round_dimension(
      values[at], cases$operator[row], cases$dimension[row],
      cases$measurement_system[row]
    )
  }, numeric(1L))
  operation <- match(cases$operator[rows], merchandiser:::.mc_rounding_operators) - 1L
  dimension <- match(cases$dimension[rows], c("diameter", "length", "volume")) - 1L
  measurement_system <- match(cases$measurement_system[rows], c("imperial", "metric"))
  actual_cpp <-
    merchandiser:::mc_round_dimension_cpp(values, operation, dimension, measurement_system)
  expect_equal(actual_r, expected, tolerance = 1e-12)
  expect_identical(actual_cpp, actual_r)
  unchanged <- c(0, 0.5, 10.5)
  expect_identical(merchandiser:::mc_round_dimension_cpp(unchanged, c(0L, 1L, 1L), c(
    0L, 1L,
    2L
  ), c(1L, 2L, 1L)), unchanged)
})

test_that("dimension rounding covers imperial and metric conversions", {
  round_dimension <- merchandiser:::.mc_round_dimension
  expect_equal(round_dimension(10.4, "nearest_0.5in_half_up", "diameter", "imperial"), 10.5)
  expect_equal(round_dimension(5.07, "truncate_1in", "diameter", "metric"), 2.54)
  expect_equal(round_dimension(1.1, "truncate_1cm", "diameter", "imperial"), 2 / 2.54)
  expect_equal(round_dimension(2.6, "nearest_1cm_half_up", "diameter", "metric"), 3)
  expect_equal(round_dimension(1, "truncate_1ft", "length", "metric"), 3 * 0.3048)
  expect_equal(round_dimension(1.5, "nearest_1ft_half_up", "length", "imperial"), 2)
  expect_equal(round_dimension(1, "truncate_0.1m", "length", "imperial"), 0.3 / 0.3048)
  expect_equal(round_dimension(1.26, "nearest_0.1m_half_up", "length", "metric"), 1.3)
  expect_equal(round_dimension(14.9, "truncate_board_foot", "volume", "imperial"), 14)
  expect_equal(round_dimension(
    14.5, "nearest_board_foot_half_up", "volume",
    "imperial"
  ), 15)
})

test_that("NVEL helpers cover short logs and terminal redistribution", {
  numlog <- merchandiser:::.mc_numlog
  segmnt <- merchandiser:::.mc_segmnt
  expect_identical(numlog(12L, 1L, 1, 20, 2, 0.5), 0L)
  expect_gt(numlog(12L, 1L, 21, 20, 2, 0.5), 0L)
  expect_gt(numlog(21L, 1L, 21, 20, 2, 0.5), 0L)
  expect_gt(numlog(21L, 2L, 22, 20, 2, 0.5), 0L)
  expect_gt(numlog(23L, 2L, 23, 20, 2, 0.5), 0L)
  expect_gt(numlog(24L, 2L, 25, 20, 2, 0.5), 0L)
  expect_length(segmnt(12L, 1L, 1, 20, 2, 0, 0L), 0L)

  cases <- list(c(24, 2, 2, 16, 2, 0, 1), c(24, 2, 8, 16, 2, 0, 1), c(
    24, 2, 16, 16, 2, 0,
    1
  ), c(23, 1, 1, 16, 2, 0, 1), c(23, 1, 8, 16, 2, 0, 1), c(12, 1, 47, 20, 2, 0, 3), c(
    21,
    2, 44, 20, 2, 0, 3
  ), c(21, 2, 52, 20, 2, 0, 3), c(22, 2, 41, 20, 8, 0, 3), c(
    22, 2, 50,
    20, 2, 0, 3
  ), c(23, 2, 41, 20, 8, 0, 3), c(23, 2, 50, 20, 2, 0, 3), c(
    24, 2, 42, 20,
    2, 0, 3
  ), c(24, 2, 50, 20, 2, 0, 3), c(24, 2, 58, 20, 2, 0, 3))
  outputs <- lapply(cases, function(case) do.call(segmnt, as.list(case)))
  expect_true(all(vapply(outputs, is.double, logical(1L))))
  expect_true(all(vapply(outputs, function(x) all(x >= 0), logical(1L))))
})

test_that("ported board-foot routines cover boundary rounding", {
  scribner <- merchandiser:::.mc_scribner
  intl14 <- merchandiser:::.mc_intl14
  expect_identical(scribner(0.9, 16, TRUE), 0)
  expect_true(scribner(140, 16, FALSE) > 0)
  expect_true(scribner(42, 4, TRUE) >= 0)
  expect_identical(scribner(42.9, 4, TRUE), 340)
  grid <- expand.grid(diameter = c(4, 10, 20), length = c(1, 4, 8, 16, 32))
  values <- mapply(intl14, grid$diameter, grid$length)
  expect_true(all(values >= 0))
})
