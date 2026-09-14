test_that("log positions, trim, and reconciliation are invariant properties", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  result <- merchandise(
    12, 80, acceptance_model_id, acceptance_products_a(), spcd = 202,
    stump_ht = 1, status = TRUE
  )
  expect_true(all(diff(result$logs$start_height) >= 0))
  expect_equal(result$logs$nominal_end_height,
               result$logs$start_height + result$logs$nominal_length)
  expect_equal(result$logs$end_height,
               result$logs$nominal_end_height + result$logs$trim)
  ledger <- with(result$trees,
    log_net_cubic_ib + stump_cubic_ib + top_cubic_ib + cull_cubic_ib +
      break_cubic_ib + located_deduction_cubic_ib + posthoc_deduction_cubic_ib
  )
  expect_equal(result$trees$gross_stem_cubic_ib, ledger, tolerance = 1e-8)
})

test_that("thread counts one and eight return identical results", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  run <- function(threads) {
    merchandiser::with_threads(threads, merchandise(
      rep(12, 8), rep(80, 8), acceptance_model_id, acceptance_products_a(),
      id = 1:8, spcd = 202, stump_ht = 1, status = TRUE
    ))
  }
  one <- run(1)
  eight <- run(8)
  expect_identical(one$logs, eight$logs)
  expect_identical(one$trees, eight$trees)
  expect_identical(one$residuals, eight$residuals)
})

test_that("only size-one call vectors recycle", {
  products <- .mc_legacy_product(
    "saw", 1L, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  result <- merchandise(c(12, 14), c(40, 45), "demo.paraboloid", products,
                        status = TRUE)
  expect_equal(nrow(result$trees), 2L)
  expect_error(merchandise(c(12, 14), c(40, 45, 50), "demo.paraboloid", products),
               "common size")
  expect_error(merchandise(12, 40, "demo.paraboloid", products,
                           id = c(1L, 1L)), "unique")
})

test_that("zero-length and single-tree console calls are stable", {
  products <- .mc_legacy_product(
    "saw", 1L, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  empty <- merchandise(double(), double(), character(), products, status = TRUE)
  expect_s3_class(empty, "merch_result")
  expect_equal(nrow(empty$trees), 0L)
  single <- merchandise(12, 40, "demo.paraboloid", products, status = TRUE)
  expect_equal(nrow(single$trees), 1L)
})

test_that("metric calls apply the 0.3 meter stump convention", {
  product <- .mc_legacy_product(
    "metric", 1L, min_length = 2, max_length = 5, length_step = 1,
    min_boundary_length = 1, min_sed = 10, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "m3",
    scale_bark_basis = "ib"
  )
  result <- merchandise(
    30.48, 12.192, "demo.paraboloid", product, units = "metric", status = TRUE
  )
  expect_equal(result$trees$stump_height, 0.3)
  expect_true(all(result$logs$end_height <= result$trees$utilization_height + 1e-8))
})

test_that("post-hoc deductions remain multiplicative and auditable", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  result <- merchandise(
    12, 80, acceptance_model_id, acceptance_products_a(), spcd = 202,
    stump_ht = 1, status = TRUE
  )
  original <- result$logs$log_net_cubic_ib
  result <- apply_defect_pct(result, 10, "hidden_defect")
  result <- apply_defect_pct(result, 20, "breakage")
  expect_equal(result$logs$log_net_cubic_ib, original * 0.9 * 0.8)
  expect_identical(result$run_metadata$posthoc_calls$call_seq, 1:2)
  expect_identical(result$run_metadata$posthoc_calls$kind,
                   c("hidden_defect", "breakage"))
})

test_that("size-stable helpers work inside dplyr mutate", {
  data <- data.frame(value = c(10, 20), pct = c(5, 10))
  result <- dplyr::mutate(
    data, net = apply_defect_pct(value, pct, "hidden_defect")
  )
  expect_equal(result$net, c(9.5, 18))
  trees <- data.frame(dbh = c(12, 14), ht = c(80, 75))
  result <- dplyr::mutate(
    trees,
    pct = defect_pct_from_thirds(dbh, ht, "demo.paraboloid", 10, 20, 30)
  )
  expect_equal(nrow(result), 2L)
})

test_that("summaries retain required keys and expansion", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  result <- merchandise(
    c(12, 12), c(80, 80), acceptance_model_id, acceptance_products_a(),
    id = 1:2, spcd = 202, stump_ht = 1, status = TRUE
  )
  stem <- product_summary(result, expansion = as.double(c(2, 3)))
  expect_equal(stem$input_trees, 2L)
  expect_equal(stem$valid_trees, 2L)
  by_model <- product_summary(result, group = "model")
  expect_identical(by_model$model, acceptance_model_id)
  log <- product_summary(result, table = "logs", basis = "gross")
  expect_true(all(c("product", "scale_rule", "measurement_quantity", "scale_unit",
                    "scale_bark_basis") %in% names(log)))
})

test_that("priced results propagate deductions through every output table", {
  product <- .mc_legacy_product(
    "priced", 1L, lengths = 16, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib", price = 50, price_quantity = 10
  )
  result <- merchandise(
    12, 40, "demo.paraboloid", product, utilization_height = 17,
    currency = "USD", status = TRUE
  )
  reduced <- apply_defect_pct(result, 25, "handling")
  expect_equal(reduced$logs$net_scale, result$logs$net_scale * 0.75)
  expect_equal(reduced$scales$net, result$scales$net * 0.75)
  expect_equal(reduced$values$net_value, result$values$net_value * 0.75)
  expect_true("posthoc_deduction" %in% names(reduced$scales))
  expect_true("posthoc_deduction_value" %in% names(reduced$values))

  scale_summary <- product_summary(reduced, table = "scales", basis = "net")
  value_summary <- product_summary(reduced, table = "values", basis = "net")
  expect_equal(scale_summary$net, sum(reduced$scales$net))
  expect_equal(value_summary$net_value, sum(reduced$values$net_value))

  grouped <- product_summary(
    reduced, group = list(class = "sample"), table = "logs"
  )
  expect_identical(grouped$class, "sample")
  expect_error(product_summary(reduced, group = "absent"), "tree column")
  expect_error(product_summary(reduced, group = list("sample")), "uniquely named")
  expect_error(
    product_summary(reduced, group = list(class = c("a", "b"))),
    "align to trees"
  )
  expect_error(
    product_summary(reduced, group = list(product = "collision"), table = "logs"),
    "may not reuse"
  )
  expect_error(product_summary(1), "merch_result")
  expect_error(product_summary(reduced, expansion = -1), "nonnegative doubles")

  missing <- reduced
  missing$values$net_value[] <- NA_real_
  propagated <- product_summary(
    missing, table = "values", basis = "net", na_action = "propagate"
  )
  expect_true(is.na(propagated$net_value))
})

test_that("green-weight and cord measurement conversions execute", {
  weight <- .mc_legacy_product(
    "weight", 1L, lengths = 16, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "green_weight",
    scale_unit = "green_short_ton", scale_bark_basis = "ob"
  )
  valid <- merchandise(
    12, 40, "demo.paraboloid", weight, spcd = 202,
    utilization_height = 17, status = TRUE
  )
  expect_true(is.finite(valid$logs$gross_scale))
  unavailable <- merchandise(
    12, 40, "demo.paraboloid", weight,
    utilization_height = 17, status = TRUE
  )
  expect_identical(unavailable$trees$status, 411L)

  cord <- .mc_legacy_product(
    "cord", 1L, lengths = 4, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cord", scale_unit = "cord",
    scale_bark_basis = "ib", cord_solid_fraction = 0.75
  )
  cord_result <- merchandise(
    30.48, 12.192, "demo.paraboloid", cord,
    utilization_height = 5.2, units = "metric", status = TRUE
  )
  expect_true(is.finite(cord_result$logs$gross_scale))
  expect_identical(cord_result$logs$scale_unit, "cord")
})
