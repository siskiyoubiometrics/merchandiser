test_that("public bucking signatures match the binding interface", {
  expect_identical(
    names(formals(merchandise)),
    c(
      "dbh", "ht", "model", "products", "id", "spcd", "age", "pruned",
      "defects", "stump_ht", "utilization_top", "utilization_top_basis",
      "utilization_height", "curvature_scale", "report_also", "currency", "...",
      "units", "status", "species", "taper_map", "preset", "quiet",
      "region", "forest", "district", "scaling"
    )
  )
  expect_identical(
    names(formals(optimize_bucking)),
    c(
      "dbh", "ht", "model", "products", "id", "spcd", "age", "pruned",
      "defects", "stump_ht", "utilization_top", "utilization_top_basis",
      "utilization_height", "curvature_scale", "report_also", "currency",
      "objective", "unpriced", "...", "units", "status", "species",
      "taper_map", "preset", "quiet", "region", "forest", "district", "scaling"
    )
  )
  expect_true("compare_bucking_prices" %in% getNamespaceExports("merchandiser"))
})

test_that("the version 1.1 model-vector hash is pinned", {
  expect_identical(
    merchandiser:::.mc_model_hash(c("200FW2W108", "900CLKE001")),
    "3b688f37"
  )
})

test_that("metric product normalization is independent of construction route", {
  constructed <- .mc_legacy_product(
    "metric", 1L, lengths = 1.03, trim = 0.03, min_boundary_length = 0.53,
    min_sed = 0, diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "m3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  direct <- data.frame(
    product = "metric", priority = 1L, trim = 0.03,
    min_boundary_length = 0.53, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "m3",
    scale_bark_basis = "ib", allow_lower_products = FALSE,
    stringsAsFactors = FALSE
  )
  direct$lengths <- I(list(1.03))
  arguments <- list(
    dbh = 30.48, ht = 12.192, model = "demo.paraboloid", stump_ht = 0.3,
    utilization_height = 3, units = "metric", status = TRUE
  )
  by_constructor <- do.call(
    merchandise, c(arguments, list(products = constructed))
  )
  by_frame <- do.call(merchandise, c(arguments, list(products = direct)))
  expect_identical(by_constructor$logs, by_frame$logs)
  expect_identical(by_constructor$run_metadata$products_hash,
                   by_frame$run_metadata$products_hash)
  expect_equal(by_constructor$logs$nominal_length[1L], 1.0414)
})

test_that("renaming a product preserves its raw length snapshot", {
  product <- .mc_legacy_product(
    "old", 1L, lengths = 1.03, trim = 0.03, min_boundary_length = 0.53,
    min_sed = 0, diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "m3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  product$product <- "new"
  metric <- merchandiser:::.validate_products_units(product, "metric")
  expect_equal(metric$lengths[[1L]], 1.0414)
  expect_equal(metric$trim, 0.0254)
  expect_equal(metric$min_boundary_length, 0.5334)
})

test_that("zero-length calls use every normal typed table schema", {
  product <- .mc_legacy_product(
    "none", 1L, min_dbh = 100, lengths = 8, min_sed = 0,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
    price = 1
  )
  empty <- merchandise(
    double(), double(), character(), product, spcd = integer(),
    currency = "USD", status = TRUE
  )
  ordinary <- merchandise(
    12, 40, "demo.paraboloid", product, spcd = 202,
    currency = "USD", status = TRUE
  )
  for (name in c(
    "logs", "scales", "residuals", "defect_accounting", "values"
  )) {
    expect_identical(names(empty[[name]]), names(ordinary[[name]]), info = name)
    expect_identical(
      vapply(empty[[name]], typeof, character(1L)),
      vapply(ordinary[[name]], typeof, character(1L)),
      info = name
    )
  }
  expect_identical(names(empty$trees), names(ordinary$trees))
  expect_identical(names(empty$diagnostics), names(ordinary$diagnostics))
})

test_that("inside-bark merchandising does not require DOB capability", {
  product <- .mc_legacy_product(
    "inside", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  result <- merchandise(
    12, 80, "900CLKE001", product, stump_ht = 1,
    utilization_height = 9, status = TRUE
  )
  expect_identical(result$trees$status, 0L)
  expect_true(is.finite(result$logs$log_gross_cubic_ib))
  expect_true(is.na(result$logs$log_gross_cubic_ob))
  expect_true(is.na(result$trees$reconciled_cubic_ob))
})

test_that("mixed DOB capability preserves available outside-bark output", {
  product <- .mc_legacy_product(
    "inside", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  alone <- merchandise(
    12, 40, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 17, status = TRUE
  )
  mixed <- merchandise(
    c(12, 12), c(40, 80), c("demo.paraboloid", "900CLKE001"), product,
    stump_ht = 1, utilization_height = 17, status = TRUE
  )
  expect_identical(mixed$trees$status, c(0L, 0L))
  expect_equal(
    mixed$logs$log_gross_cubic_ob[mixed$logs$id == 1L],
    alone$logs$log_gross_cubic_ob
  )
  expect_true(all(is.na(
    mixed$logs$log_gross_cubic_ob[mixed$logs$id == 2L]
  )))
})

test_that("an outside-bark requirement retains the missing-DOB status", {
  product <- .mc_legacy_product(
    "outside", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ob", allow_lower_products = FALSE
  )
  result <- merchandise(
    12, 80, "900CLKE001", product, stump_ht = 1,
    utilization_height = 9, status = TRUE
  )
  expect_identical(result$trees$status, 53L)
  expect_identical(result$trees$log_count, 0L)
})

test_that("exact-height closure preserves distinct nearby boundaries", {
  product <- .mc_legacy_product(
    "log", 1L, lengths = 16, min_boundary_length = 1, min_sed = 0,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  upper <- 20 + 1e-10
  defect <- defect(1L, 20, upper, "cull")
  result <- merchandise(
    20, 60, "demo.paraboloid", product, id = 1L, defects = defect,
    stump_ht = 1, utilization_height = 40, status = TRUE
  )
  expect_true(any(result$logs$end_height == 20))
  expect_true(any(result$logs$start_height == upper))
  expect_true(any(result$residuals$from == 20 & result$residuals$to == upper))
  expect_identical(result$trees$status, 0L)
})

test_that("mixed prices use zero for unpriced logs without a failure status", {
  products <- products(
    .mc_legacy_product(
      "priced", 1L, lengths = 8, min_sed = 0, max_logs_per_segment = 1L,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
      price = 10
    ),
    .mc_legacy_product(
      "unpriced", 2L, lengths = 8, min_sed = 0, diameter_basis = "ib",
      scale_rule = "cubic", measurement_quantity = "cubic",
      scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
    )
  )
  result <- merchandise(
    12, 40, "demo.paraboloid", products, stump_ht = 1,
    utilization_height = 17, currency = "USD", status = TRUE
  )
  unpriced <- result$values$product == "unpriced"
  expect_true(any(unpriced))
  expect_true(all(result$values$net_value[unpriced] == 0))
  expect_identical(result$trees$status, 0L)
  expect_equal(result$trees$net_value, sum(result$values$net_value))
})

test_that("unpriced logs stay zero when their requested scale is undefined", {
  products <- products(
    .mc_legacy_product(
      "priced", 1L, lengths = 8, min_sed = 0, max_logs_per_segment = 1L,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
      price = 10
    ),
    .mc_legacy_product(
      "unpriced_weight", 2L, lengths = 8, min_sed = 0,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "green_weight", scale_unit = "green_short_ton",
      scale_bark_basis = "ib", allow_lower_products = FALSE
    )
  )
  result <- merchandise(
    12, 40, "demo.paraboloid", products, stump_ht = 1,
    utilization_height = 17, currency = "USD", status = TRUE
  )
  unpriced <- result$values$product == "unpriced_weight"
  expect_true(any(unpriced))
  expect_true(all(is.na(result$logs$net_scale[unpriced])))
  expect_true(all(result$values$net_value[unpriced] == 0))
  expect_true(is.finite(result$trees$net_value))
  expect_identical(result$trees$status, 411L)
})

test_that("undefined scale values remain undefined in every value aggregate", {
  product <- .mc_legacy_product(
    "weight", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "green_weight",
    scale_unit = "green_short_ton", scale_bark_basis = "ob", price = 100,
    allow_lower_products = FALSE
  )
  result <- merchandise(
    12, 40, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 9, currency = "USD", status = TRUE
  )
  expect_identical(result$trees$status, 411L)
  expect_true(all(is.na(result$logs$net_value)))
  expect_true(all(is.na(result$values$net_value)))
  expect_true(all(is.na(result$trees$net_value)))
})

test_that("inside-bark and outside-bark physical ledgers reconcile", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  result <- merchandise(
    14, 75, acceptance_model_id, acceptance_products_b(), id = 1L,
    spcd = 131, defects = defects_from_stoppers(
      1L, 75, saw_stop = 40, pulp_stop = 60, stump_ht = 1
    ), stump_ht = 1, status = TRUE
  )
  result <- apply_defect_pct(result, 12.5, "handling")
  ledger <- function(basis) {
    with(result$trees,
      get(paste0("log_net_cubic_", basis)) +
        get(paste0("stump_cubic_", basis)) +
        get(paste0("top_cubic_", basis)) +
        get(paste0("cull_cubic_", basis)) +
        get(paste0("break_cubic_", basis)) +
        get(paste0("located_deduction_cubic_", basis)) +
        get(paste0("posthoc_deduction_cubic_", basis))
    )
  }
  expect_equal(result$trees$gross_stem_cubic_ib, ledger("ib"), tolerance = 1e-8)
  expect_equal(result$trees$gross_stem_cubic_ob, ledger("ob"), tolerance = 1e-8)
  expect_true(all(result$trees$reconciled_cubic_ib))
  expect_true(all(result$trees$reconciled_cubic_ob))
  expect_equal(
    result$scales$gross,
    result$scales$net + result$scales$located_deduction +
      result$scales$posthoc_deduction
  )
})

test_that("product order cannot change indices or run metadata", {
  first <- .mc_legacy_product(
    "first", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  second <- .mc_legacy_product(
    "second", 2L, lengths = 4, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  run <- function(products) {
    merchandise(
      12, 40, "demo.paraboloid", products, stump_ht = 1,
      utilization_height = 9, status = TRUE
    )
  }
  forward <- run(products(first, second))
  reverse <- run(products(second, first))
  expect_identical(forward$logs$product_index, reverse$logs$product_index)
  expect_identical(forward$run_metadata$products_hash,
                   reverse$run_metadata$products_hash)
  expect_identical(forward$logs, reverse$logs)
})

test_that("canonical point defects remain validation-idempotent", {
  product <- .mc_legacy_product(
    "pulp", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    pulp_product = TRUE, scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
  )
  defects <- rbind(
    defect(1L, 10, NA_real_, "fork"),
    defect(1L, 30, 30, "break")
  )
  once <- validate_defects(defects, product, 40, 1L)
  twice <- validate_defects(once, product, 40, 1L)
  expect_identical(once, twice)
  expect_identical(once$to, once$from)
})

test_that("native scale test bridges reject malformed calls", {
  board <- .mc_legacy_product(
    "board", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "scribner_factor_split_20",
    measurement_quantity = "board_foot", scale_unit = "board_foot",
    scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  exact <- .mc_legacy_product(
    "exact", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  cpp <- merchandiser:::.mc_cpp_products(board, NULL)
  expect_error(
    merchandiser:::mc_nvel_log_scale_cpp(cpp, 1L, c(6, 7), 8),
    "inconsistent sizes"
  )
  expect_error(
    merchandiser:::mc_nvel_log_scale_cpp(cpp, 2L, 6, 8),
    "out of range"
  )
  expect_error(
    merchandiser:::mc_nvel_log_scale_cpp(cpp, 1L, NA_real_, 8),
    "finite and nonnegative"
  )
  expect_error(
    merchandiser:::mc_nvel_log_scale_cpp(cpp, 1L, -1, 8),
    "finite and nonnegative"
  )
  expect_error(
    merchandiser:::mc_nvel_log_scale_cpp(
      merchandiser:::.mc_cpp_products(exact, NULL), 1L, 6, 8
    ),
    "must use Scribner or International"
  )
  expect_error(
    merchandiser:::mc_round_dimension_cpp(1, c(1L, 2L), 0L, 1L),
    "inconsistent sizes"
  )
})
