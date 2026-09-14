test_that("product constructors fill the closed typed schema", {
  product <- product(
    "saw", 1L, species = "spcd(131,202)", lengths = as.double(c(16.01, 16)),
    min_sed = 6, inside_bark = TRUE, volume_unit = "cubic"
  )
  expect_s3_class(product, "merch_products")
  expect_identical(names(product), names(products(product)))
  expect_identical(product$priority, 1L)
  expect_type(product$min_dbh, "double")
  expect_type(product$max_logs_per_segment, "integer")
  expect_identical(product$lengths[[1L]], 16)
  expect_identical(validate_products(product), product)
})

test_that("species grammar and product schema are closed", {
  base <- list(
    product = "saw", priority = 1L, lengths = 16, min_sed = 6,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
  )
  for (species in c("SPCD(202)", "spcd(202,202)", "spcd(+202)",
                    "spcd(202, 131)", "spcd(202.0)")) {
    expect_error(do.call(product, c(base, list(species = species))))
  }
  expect_error(do.call(product, c(base, list(defect_excludes = "rot"))),
               "Unknown product column")
  bad_type <- do.call(product, base)
  bad_type$priority <- 1
  normalized <- validate_products(bad_type)
  expect_identical(normalized$priority, 1L)
})

test_that("discrete and step lengths normalize independently", {
  discrete <- .mc_legacy_product(
    "discrete", 1L, lengths = as.double(c(8.01, 8.04, 10)),
    length_parity = "even", trim = 0.49, min_boundary_length = 4.01,
    min_sed = 2, diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
  )
  expect_equal(discrete$lengths[[1L]], c(8, 10))
  expect_equal(discrete$trim, 0.5)
  expect_equal(discrete$min_boundary_length, 4)
  step <- .mc_legacy_product(
    "step", 1L, lengths = NULL, min_length = 7.99, max_length = 10.01,
    length_step = 0.99, min_sed = 2, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  expect_equal(step$min_length, 8)
  expect_equal(step$max_length, 10)
  expect_equal(step$length_step, 1)
  expect_error(.mc_legacy_product(
    "zero", 1L, lengths = 0.01, min_sed = 2, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  ), "normalize to zero")
})

test_that("metric normalization uses one-inch metric quantum", {
  product <- .mc_legacy_product(
    "metric", 1L, lengths = 1, trim = 0, min_sed = 10,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "m3", scale_bark_basis = "ib"
  )
  metric <- merchandiser:::.validate_products_units(product, "metric")
  expect_equal(metric$lengths[[1L]], 0.9906)
  expect_equal(attr(metric, "quantum"), 0.0254)
})

test_that("scale, measurement, limits, and rounding combinations validate", {
  expect_error(.mc_legacy_product(
    "bad", 1L, lengths = 16, min_sed = 6, diameter_basis = "ib",
    scale_rule = "doyle_formula", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  ), "Board-foot")
  expect_error(.mc_legacy_product(
    "bad", 1L, lengths = 16, min_sed = 6, diameter_basis = "ib",
    scale_rule = "scribner_decimal_c_split_20",
    measurement_quantity = "board_foot", scale_unit = "board_foot",
    scale_bark_basis = "ib", length_round = "truncate_1ft"
  ), "not overrideable")
  expect_error(.mc_legacy_product(
    "bad", 1L, lengths = 16, min_sed = 6, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cord", scale_unit = "cord",
    scale_bark_basis = "ib", cord_solid_fraction = 1
  ), "strictly between")
})

test_that("empty product tables retain typed columns", {
  empty <- products()
  expect_s3_class(empty, "merch_products")
  expect_equal(nrow(empty), 0L)
  expect_type(empty$product, "character")
  expect_type(empty$priority, "integer")
})

test_that("product normalization is independent of input row order", {
  first <- .mc_legacy_product(
    "first", 1L, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  second <- .mc_legacy_product(
    "second", 2L, lengths = 8, min_sed = 2, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )

  expect_identical(
    products(second, first),
    products(first, second)
  )
})

test_that("product validation covers value and normalization error paths", {
  validate_expanded <- function(x) .mc_validate_products_expanded(x, "imperial")
  base <- .mc_legacy_product(
    "base", 1L, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  invalid <- function(name, value) {
    result <- base
    result[[name]] <- value
    result
  }
  expect_error(validate_expanded(invalid("product", "")), "nonempty")
  expect_error(validate_expanded(invalid("priority", 0L)), "positive")
  expect_error(validate_expanded(invalid("grade", "")), "grade")
  expect_error(validate_expanded(invalid("fallback", NA)), "may not be missing")
  expect_error(validate_expanded(invalid("max_dbh", -1)), "invalid interval")
  expect_error(validate_expanded(invalid("min_sed", NA_real_)), "may not be missing")
  expect_error(validate_expanded(invalid("trim", -1)), "finite nonnegative")
  expect_error(validate_expanded(invalid("price_quantity", 0)), "positive")
  expect_error(validate_expanded(invalid("max_defect_pct", 101)), "zero and 100")
  expect_error(validate_expanded(invalid("max_logs_per_segment", 0L)), "1 through 1000")
  expect_error(validate_expanded(invalid("price", -1)), "finite and nonnegative")
  expect_error(validate_expanded(invalid("length_parity", "invalid")),
               "any, even, or odd")
  expect_error(validate_expanded(invalid("diameter_basis", "invalid")),
               "must be ib or ob")
  expect_error(validate_expanded(invalid("segmentation_policy", "invalid")),
               "not recognized")
  expect_error(validate_expanded(invalid("scale_rule", "invalid")),
               "not registered")
  expect_error(validate_expanded(invalid("measurement_quantity", "invalid")),
               "not recognized")
  expect_error(validate_expanded(invalid("scale_unit", "mismatch")),
               "incompatible")
  expect_error(
    validate_expanded(invalid("scale_rule", "doyle_formula")),
    "Board-foot rules require"
  )

  green <- base
  green$measurement_quantity <- "green_weight"
  green$scale_unit <- "green_short_ton"
  green$scale_rule <- "smalian"
  expect_error(validate_expanded(green), "require cubic")
  expect_error(validate_expanded(invalid("cord_solid_fraction", 0.5)),
               "only accepted for cord")
  expect_error(validate_expanded(invalid("diameter_round", "invalid")),
               "unsupported rounding")
  expect_error(validate_expanded(invalid("diameter_round", "truncate_1ft")),
               "wrong dimension")
  expect_error(validate_expanded(invalid("diameter_round", "truncate_1in")),
               "does not accept dimension")
  expect_error(validate_expanded(invalid("max_sweep", "")), "nonempty")

  bad_lengths <- base
  bad_lengths$lengths[[1L]] <- 16L
  normalized <- validate_expanded(bad_lengths)
  expect_identical(normalized$lengths[[1L]], 16)
  bad_lengths <- base
  bad_lengths$min_length <- 8
  expect_error(validate_expanded(bad_lengths), "leave step fields missing")
  expect_error(
    .mc_legacy_product(
      "parity", 1L, lengths = 9, length_parity = "even", min_sed = 4,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
    ),
    "removes every"
  )
  expect_error(
    .mc_legacy_product(
      "step", 1L, min_length = NA_real_, length_step = 1, min_sed = 4,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
    ),
    "A length range requires"
  )
  expect_error(
    .mc_legacy_product(
      "step", 1L, min_length = 10, max_length = 8, length_step = 1,
      min_sed = 4, diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
    ),
    "below min_length"
  )
  expect_error(
    .mc_legacy_product(
      "top", 1L, lengths = 16, min_boundary_length = 0.01, min_sed = 4,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
    ),
    "min_boundary_length may not normalize"
  )

  first <- base
  first$product <- "first"
  first$pulp_product <- TRUE
  second <- base
  second$product <- "second"
  second$priority <- 2L
  second$pulp_product <- TRUE
  expect_error(products(first, second), "At most one")
})
