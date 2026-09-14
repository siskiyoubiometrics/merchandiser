interface_product <- function(sold_by = "cubic", ...) {
  product("saw", 1, lengths = 16, trim = 0.5, min_sed = 4,
          diameter_basis = "ib", sold_by = sold_by, ...)
}

test_that("measurement aliases normalize and preserve the call's input units", {
  aliases <- c("scribner_whole", "scribner_allocated", "scribner_split",
               "international", "doyle", "cubic", "green_ton", "cord")
  canonical <- c("scribner_decimal_c_whole_40", "scribner_decimal_c_allocated_20",
                 "scribner_decimal_c_split_20", "international_1_4_4ft", "doyle_formula",
                 "cubic_ft_ib", "green_short_ton", "cord_ib")
  expect_identical(.mc_sold_by(aliases, "imperial")$sold_by, canonical)
  p <- interface_product()
  metric <- .validate_products_units(products(p), "metric")
  expect_identical(metric$scale_unit, "m3")
  expect_error(.validate_products_units(interface_product("cubic_ft_ib"), "metric"),
               "scale_unit = ft3.*the product fields cannot express it.*report_also")
  expect_false(any(c("scale_rule", "measurement_quantity", "scale_unit", "scale_bark_basis",
                     "diameter_round", "length_round", "volume_round", "grade",
                     "segmentation_policy", "length_parity", "fallback", "price_quantity") %in%
                     names(p)))
  expect_identical(validate_products(p), p)
  expect_error(interface_product("scribner_factor_split_20"), "report_also")
  expect_error(interface_product(diameter_round = "truncate_1in"), "report_also")
  expect_error(interface_product("scribner_allocated"), "report_also")
})

test_that("renamed fields retain their legacy values with explicit migration", {
  p <- suppressMessages(product(
    "saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib", sold_by = "doyle",
    fallback = FALSE, pulp_product = TRUE, max_defect_pct = 10,
    price_quantity = 1000, source_id = "example specification", grade = "recorded"
  ))
  expect_false(p$allow_after_higher_product)
  expect_true(p$accepts_pulp_restriction)
  expect_identical(p$max_rot_pct, 10)
  expect_identical(p$price_per, 1000)
  expect_identical(p$specification_source, "example specification")
  expect_identical(p$meta_grade, "recorded")
  expect_error(interface_product(allow_after_higher_product = TRUE, fallback = FALSE),
               "Supply only")
})

test_that("parity migration retains the calculation lengths in each unit system", {
  for (units in c("imperial", "metric")) {
    p <- suppressMessages(product(
      "saw", 1, min_length = 8, max_length = 300, length_step = 1,
      length_parity = "even", min_sed = 4, diameter_basis = "ib", sold_by = "cubic"
    ))
    old <- .mc_legacy_product(
      "saw", 1, min_length = 8, max_length = 300, length_step = 1,
      length_parity = "even", min_sed = 4, diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = if (units == "imperial") "ft3" else "m3",
      scale_bark_basis = "ib"
    )
    migrated <- .validate_products_units(p, units)
    original <- .mc_validate_products_expanded(old, units)
    expect_identical(.mc_lengths_at(migrated, 1, 300, .mc_q(units)),
                     .mc_lengths_at(original, 1, 300, .mc_q(units)))
    expect_false("length_parity" %in% names(p))
  }
})

test_that("named reporting definitions scale existing cuts without changing values", {
  p <- interface_product("cubic_ft_ib", price = 10)
  run <- function(report_also = NULL) {
    merchandise(20, 80, "demo.paraboloid", p, spcd = 202, currency = "USD",
                status = TRUE, report_also = report_also)
  }
  base <- run()
  extra <- run(data.frame(
    name = c("Doyle unrounded", "Doyle truncated"),
    sold_by = "doyle_formula", diameter_round = c("none", "truncate_1in")
  ))
  expect_identical(.mc_plain_product_frame(extra$logs), .mc_plain_product_frame(base$logs))
  expect_identical(extra$values, base$values)
  reports <- extra$scales[!extra$scales$is_product, ]
  expect_setequal(reports$name, c("Doyle unrounded", "Doyle truncated"))
  expect_equal(nrow(reports), 2 * nrow(base$logs))
  expect_true(all(reports$unit == "board_foot"))
  summary <- product_summary(extra, table = "scales")
  expect_equal(nrow(summary), 3)
  expect_setequal(summary$name, c("product", "Doyle unrounded", "Doyle truncated"))
  weights <- run(c("green_short_ton", "green_metric_ton"))
  expect_identical(.mc_plain_product_frame(weights$logs), .mc_plain_product_frame(base$logs))
  expect_true(all(is.finite(weights$scales$gross)))
  expect_setequal(weights$scales$unit, c("ft3", "green_short_ton", "green_metric_ton"))
  expect_error(run(data.frame(name = "cord", sold_by = "cord")), "cord_solid_fraction")
})

test_that("felled diagrams preserve caller panels and stable colors", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  p <- interface_product()
  x <- merchandise(c(12, 16), c(60, 80), "demo.paraboloid", p, status = TRUE)
  graphics::par(mfrow = c(1, 2))
  plot(x, trees = x$trees$id)
  expect_identical(graphics::par("mfrow"), c(1L, 2L))
  expect_identical(graphics::par("mfg")[1:2], c(1L, 1L))
  plot(x)
  expect_identical(graphics::par("mfg")[1:2], c(1L, 2L))
  expect_identical(.mc_product_colors(c("saw", "pulp"))["saw"],
                   .mc_product_colors("saw"))
})
