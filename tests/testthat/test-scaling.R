test_that("pinned Scribner and International routines retain discrete behavior", {
  expect_equal(
    merchandiser:::.mc_scribner(10, 16, TRUE),
    trunc((16 * merchandiser:::.mc_scribner_factor[125] + 5) / 10) * 10
  )
  expect_equal(
    merchandiser:::.mc_scribner(10, 16, FALSE),
    trunc(16 * merchandiser:::.mc_scribner_factor[125] + 0.5)
  )
  expect_equal(merchandiser:::.mc_intl14(3, 16), 0)
  expect_true(merchandiser:::.mc_intl14(10, 16) %% 5 == 0)
})

test_that("NVEL segmentation ports exercise every declared option family", {
  expect_equal(merchandiser:::.mc_nvel_segments(40, "split_20"), c(20, 20))
  expect_equal(sum(merchandiser:::.mc_nvel_segments(35, "allocated_20")), 36)
  expect_equal(sum(merchandiser:::.mc_nvel_segments(60, "whole_40")), 60)
  for (option in c(11:14, 21:24)) {
    maximum <- c(`11` = 16, `12` = 20, `13` = 32, `14` = 40)[as.character(option)]
    if (is.na(maximum)) maximum <- 16
    number <- merchandiser:::.mc_numlog(option, 2L, 45, maximum, 2, 0.5)
    logs <- merchandiser:::.mc_segmnt(option, 2L, 45, maximum, 2, 0.5, number)
    expect_lte(length(logs), 20L)
    expect_true(all(logs >= 0))
  }
})

test_that("public reporting families execute on one log", {
  product <- product("scale", 1L, lengths = 16, min_sed = 0, volume_unit = "cubic")
  extra <- data.frame(
    scale_rule = c(
      "smalian", "huber", "doyle_formula", "international_1_4_4ft",
      "scribner_decimal_c_split_20", "scribner_factor_split_20",
      "scribner_decimal_c_whole_40"
    ),
    scale_bark_basis = "ib", stringsAsFactors = FALSE
  )
  result <- merchandise(
    12, 40, "demo.paraboloid", product, utilization_height = 17,
    report_also = extra, status = TRUE
  )
  expect_setequal(result$scales$scale_rule,
                  c("cubic", extra$scale_rule))
  expect_true(all(is.finite(result$scales$gross)))
  expect_true(all(result$scales$gross >= 0))
  expect_equal(result$logs$gross_scale, result$scales$gross[result$scales$is_product])
  expect_error(merchandiser:::.mc_report_also("scribner_factor_allocated_20", NULL, "imperial"),
               "retired allocation")
})

test_that("formula scales and executable rounding use named operators", {
  expect_equal(
    merchandiser:::.mc_round_dimension(10.9, "truncate_1in", "diameter", "imperial"),
    10
  )
  expect_equal(
    merchandiser:::.mc_round_dimension(10.5, "nearest_1in_half_up", "diameter",
                                       "imperial"),
    11
  )
  expect_equal(
    merchandiser:::.mc_round_dimension(14.9, "nearest_10_board_feet_half_up",
                                       "volume", "imperial"),
    10
  )
})

test_that("every rounding operator is exhaustive at its decision boundary", {
  cases <- data.frame(
    operator = c(
      "truncate_1in", "nearest_1in_half_up", "nearest_0.5in_half_up",
      "truncate_1cm", "nearest_1cm_half_up", "truncate_1ft",
      "nearest_1ft_half_up", "truncate_0.1m", "nearest_0.1m_half_up",
      "truncate_board_foot", "nearest_board_foot_half_up",
      "nearest_10_board_feet_half_up"
    ),
    dimension = c(rep("diameter", 5), rep("length", 4), rep("volume", 3)),
    units = c(
      "imperial", "imperial", "imperial", "metric", "metric",
      "imperial", "imperial", "metric", "metric",
      "imperial", "imperial", "imperial"
    ),
    threshold = c(10, 10.5, 10.25, 10, 10.5, 10, 10.5, 1, 1.05, 10, 10.5, 15),
    below = c(9, 10, 10, 9, 10, 9, 10, 0.9, 1, 9, 10, 10),
    exact = c(10, 11, 10.5, 10, 11, 10, 11, 1, 1.1, 10, 11, 20),
    above = c(10, 11, 10.5, 10, 11, 10, 11, 1, 1.1, 10, 11, 20),
    stringsAsFactors = FALSE
  )
  values <- as.vector(vapply(cases$threshold, function(x) {
    c(x - 1e-9, x, x + 1e-9)
  }, numeric(3L)))
  expected <- as.vector(t(as.matrix(cases[c("below", "exact", "above")])))
  rows <- rep(seq_len(nrow(cases)), each = 3L)
  actual_r <- vapply(seq_along(values), function(at) {
    row <- rows[at]
    merchandiser:::.mc_round_dimension(
      values[at], cases$operator[row], cases$dimension[row], cases$units[row]
    )
  }, numeric(1L))
  operation <- match(cases$operator[rows], merchandiser:::.mc_rounding_operators) - 1L
  dimension <- match(cases$dimension[rows], c("diameter", "length", "volume")) - 1L
  units <- match(cases$units[rows], c("imperial", "metric"))
  actual_cpp <- merchandiser:::mc_round_dimension_cpp(
    values, operation, dimension, units
  )
  expect_equal(actual_r, expected, tolerance = 1e-12)
  expect_identical(actual_cpp, actual_r)
  unchanged <- c(0, 0.5, 10.5)
  expect_identical(
    merchandiser:::mc_round_dimension_cpp(
      unchanged, c(0L, 1L, 1L), c(0L, 1L, 2L), c(1L, 2L, 1L)
    ),
    unchanged
  )
})

test_that("compiled merchandising executes every NVEL segmentation option", {
  for (option in c(11:14, 21:24)) {
    product <- .mc_legacy_product(
      paste0("option_", option), 1L, min_length = 8, max_length = 20,
      length_step = 1, length_parity = "even", trim = 0.5, min_sed = 0,
      diameter_basis = "ib", segmentation_policy = paste0("nvel_opt_", option),
      scale_rule = "cubic", measurement_quantity = "cubic",
      scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
    )
    number <- merchandiser:::.mc_numlog(option, 2L, 46, 20, 8, 0.5)
    expected <- merchandiser:::.mc_segmnt(option, 2L, 46, 20, 8, 0.5, number)
    arguments <- list(
      dbh = 20, ht = 80, model = "demo.paraboloid", products = product,
      stump_ht = 1, utilization_height = 47, status = TRUE
    )
    cascade <- do.call(merchandise, arguments)
    dynamic <- do.call(optimize_bucking, arguments)
    expect_identical(
      cascade$logs$nominal_length, expected, info = as.character(option)
    )
    expect_identical(
      dynamic$logs$nominal_length, expected, info = as.character(option)
    )
  }
  option_24 <- .mc_legacy_product(
    "option_24_single", 1L, min_length = 8, max_length = 20,
    length_step = 1, length_parity = "even", trim = 0.5, min_sed = 0,
    diameter_basis = "ib", segmentation_policy = "nvel_opt_24",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  single <- merchandise(
    20, 40, "demo.paraboloid", option_24, stump_ht = 1,
    utilization_height = 16, status = TRUE
  )
  expect_identical(single$logs$nominal_length, 10)
})

test_that("NVEL rules translate every behavioral member", {
  rules <- merchandiser::nvel_rules(
    even_or_odd = 2L, option = 22L, maximum_length = 16,
    minimum_length = 8, minimum_top_length = 8, merchantable_length = 40,
    primary_top = 6, secondary_top = 4, stump = 1, trim = 0.5,
    bark_ratio = 0.9, minimum_board_foot_dbh = 12, scribner = "table",
    prod = "01", ht_type = "F", live = "L", ctype = "C", cull = 5,
    forest = 1, district = 2
  )
  result <- products_from_nvel_rules(rules)
  expect_identical(result$products$length_parity, c("even", "even"))
  expect_identical(result$products$segmentation_policy,
                   c("nvel_opt_22", "nvel_opt_22"))
  expect_equal(result$stump_ht, 1)
  expect_equal(result$utilization_height, 41)
  expect_equal(result$model_aux$bark_ratio, 0.9)
  expect_true(all(grepl("^meta_nvel_", grep("^meta_", names(result$products),
                                            value = TRUE))))
})

test_that("NVEL segmentation policies control cascade log lengths", {
  product <- .mc_legacy_product(
    "nvel", 1L, min_length = 8, max_length = 20, length_step = 1,
    length_parity = "even", trim = 0.5, min_sed = 0,
    diameter_basis = "ib", segmentation_policy = "nvel_opt_22",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  result <- merchandise(
    20, 80, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 47, status = TRUE
  )
  expect_identical(result$logs$nominal_length, c(20, 12, 12))
  expect_identical(result$logs$end_height, c(21.5, 34, 46.5))
  expect_identical(
    result$residuals$cause,
    c("stump", "short_remainder", "top")
  )
  expect_error(
    .mc_legacy_product(
      "bad", 1L, lengths = c(8, 16), min_sed = 0,
      diameter_basis = "ib", segmentation_policy = "nvel_opt_22",
      scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
    ),
    "require step lengths with a finite maximum"
  )
})

test_that("dynamic-program controls execute through optimize_bucking", {
  product <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib"
  )
  result <- optimize_bucking(
    12, 40, "demo.paraboloid", product,
    objective = "net_cubic_ib", unpriced = "zero", status = TRUE
  )
  expect_identical(result$run_metadata$algorithm, "dp")
  expect_identical(result$run_metadata$objective, "net_cubic_ib")
})

test_that("scaling validation rejects every incompatible schema branch", {
  validate <- merchandiser:::.mc_validate_scaling
  base <- data.frame(
    scale_rule = "smalian", scale_bark_basis = "ib",
    stringsAsFactors = FALSE
  )
  expect_null(validate(NULL))
  expect_error(validate("smalian"), "data frame")
  expect_error(validate(data.frame(scale_rule = "smalian")),
               "Missing scaling column")
  expect_error(validate(transform(base, extra = "x")),
               "Unknown scaling column")
  factor_scaling <- validate(
    transform(base, scale_rule = factor(scale_rule))
  )
  expect_identical(factor_scaling$scale_rule, "smalian")
  expect_error(validate(transform(base, scale_rule = "unknown")),
               "invalid rule")
  expect_error(
    validate(data.frame(
      scale_rule = "doyle_formula", scale_bark_basis = "ob"
    )),
    "requires inside bark"
  )
  expect_error(validate(transform(base, diameter_round = "unknown")),
               "unsupported operator")
  expect_error(validate(transform(base, diameter_round = "truncate_1ft")),
               "wrong dimension")
  expect_error(validate(transform(base, volume_round = "truncate_board_foot")),
               "incompatible with a cubic")
  expect_error(validate(transform(
    base, scale_rule = "cubic", diameter_round = "truncate_1in"
  )),
  "cubic does not accept", fixed = TRUE)

  nvel <- data.frame(
    scale_rule = "scribner_factor_split_20", scale_bark_basis = "ib",
    length_round = "none", stringsAsFactors = FALSE
  )
  expect_error(validate(nvel), "not overrideable")
  conflict <- data.frame(
    scale_rule = rep("doyle_formula", 2), scale_bark_basis = rep("ib", 2),
    diameter_round = c("none", "truncate_1in"), stringsAsFactors = FALSE
  )
  expect_error(validate(conflict), "Conflicting scaling rows")
  expect_equal(nrow(validate(rbind(base, base))), 1L)
})

test_that("dimension rounding covers imperial and metric conversions", {
  round_dimension <- merchandiser:::.mc_round_dimension
  expect_equal(round_dimension(10.4, "nearest_0.5in_half_up", "diameter",
                               "imperial"), 10.5)
  expect_equal(round_dimension(5.07, "truncate_1in", "diameter", "metric"),
               2.54)
  expect_equal(round_dimension(1.1, "truncate_1cm", "diameter", "imperial"),
               2 / 2.54)
  expect_equal(round_dimension(2.6, "nearest_1cm_half_up", "diameter", "metric"),
               3)
  expect_equal(round_dimension(1, "truncate_1ft", "length", "metric"),
               3 * 0.3048)
  expect_equal(round_dimension(1.5, "nearest_1ft_half_up", "length", "imperial"),
               2)
  expect_equal(round_dimension(1, "truncate_0.1m", "length", "imperial"),
               0.3 / 0.3048)
  expect_equal(round_dimension(1.26, "nearest_0.1m_half_up", "length", "metric"),
               1.3)
  expect_equal(round_dimension(14.9, "truncate_board_foot", "volume", "imperial"),
               14)
  expect_equal(round_dimension(14.5, "nearest_board_foot_half_up", "volume",
                               "imperial"), 15)
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

  cases <- list(
    c(24, 2, 2, 16, 2, 0, 1),
    c(24, 2, 8, 16, 2, 0, 1),
    c(24, 2, 16, 16, 2, 0, 1),
    c(23, 1, 1, 16, 2, 0, 1),
    c(23, 1, 8, 16, 2, 0, 1),
    c(12, 1, 47, 20, 2, 0, 3),
    c(21, 2, 44, 20, 2, 0, 3),
    c(21, 2, 52, 20, 2, 0, 3),
    c(22, 2, 41, 20, 8, 0, 3),
    c(22, 2, 50, 20, 2, 0, 3),
    c(23, 2, 41, 20, 8, 0, 3),
    c(23, 2, 50, 20, 2, 0, 3),
    c(24, 2, 42, 20, 2, 0, 3),
    c(24, 2, 50, 20, 2, 0, 3),
    c(24, 2, 58, 20, 2, 0, 3)
  )
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
  grid <- expand.grid(
    diameter = c(4, 10, 20), length = c(1, 4, 8, 16, 32)
  )
  values <- mapply(intl14, grid$diameter, grid$length)
  expect_true(all(values >= 0))
  expect_equal(
    merchandiser:::.mc_convert_cubic(1, "imperial", "m3"),
    0.028316846592
  )
  expect_equal(
    merchandiser:::.mc_convert_cubic(0.028316846592, "metric", "ft3"),
    1
  )

  product <- .mc_legacy_product(
    "scribner", 1L, lengths = 4, min_sed = 0, diameter_basis = "ib",
    scale_rule = "scribner_decimal_c_split_20",
    measurement_quantity = "board_foot", scale_unit = "board_foot",
    scale_bark_basis = "ib"
  )
  cpp <- merchandiser:::.mc_cpp_products(product, NULL)
  diameter <- 42.9
  length <- 4.5
  expect_identical(
    merchandiser:::mc_nvel_log_scale_cpp(cpp, 1L, diameter, length),
    scribner(diameter, length, TRUE)
  )
})

test_that("native measurement scales are not converted twice", {
  sale_product <- function(quantity, unit, cord_fraction = NA_real_) {
    .mc_legacy_product(
      "sale", 1L, lengths = 8, trim = 0.5, min_sed = 0,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = quantity, scale_unit = unit, scale_bark_basis = "ib",
      cord_solid_fraction = cord_fraction, allow_lower_products = FALSE
    )
  }
  run <- function(products) {
    merchandise(
      12, 40, "demo.paraboloid", products,
      utilization_height = 9.5, status = TRUE
    )
  }

  cubic <- run(sale_product("cubic", "m3"))
  cubic_body <- cubic$logs$log_gross_cubic_ib -
    cubic$logs$trim_cubic_ib
  expect_equal(cubic$logs$gross_scale, cubic_body * 0.028316846592)
  cord <- run(sale_product("cord", "cord", 0.8))
  cord_body <- cord$logs$log_gross_cubic_ib - cord$logs$trim_cubic_ib
  expect_equal(cord$logs$gross_scale, cord_body / (128 * 0.8))
})
