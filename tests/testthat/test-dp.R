test_that("DP reproduces both contract acceptance log tables", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  calls <- list(
    list(
      dbh = 12, ht = 80, model = acceptance_model_id,
      products = acceptance_products_a(), spcd = 202, stump_ht = 1,
      status = TRUE
    ),
    list(
      dbh = 14, ht = 75, model = acceptance_model_id,
      products = acceptance_products_b(), id = 1L, spcd = 131,
      defects = defects_from_stoppers(
        1L, 75, saw_stop = 40, pulp_stop = 60, stump_ht = 1
      ),
      stump_ht = 1, status = TRUE
    )
  )
  columns <- c(
    "product", "stage", "length_kind", "start_height",
    "nominal_end_height", "end_height", "nominal_length",
    "physical_length", "log_gross_cubic_ib"
  )
  for (arguments in calls) {
    cascade <- do.call(merchandise, arguments)
    dynamic <- do.call(optimize_bucking, arguments)
    expect_identical(dynamic$logs[columns], cascade$logs[columns])
    expect_identical(dynamic$residuals, cascade$residuals)
    expect_identical(dynamic$trees$status, 0L)
  }
})

test_that("DP objective equals exhaustive enumeration on a short stem", {
  product <- .mc_legacy_product(
    "short", 1L, lengths = as.double(c(2, 3)), min_sed = 0,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
    price = 7, price_quantity = 2, allow_lower_products = FALSE
  )
  result <- optimize_bucking(
    12, 20, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 9, currency = "USD", objective = "value",
    status = TRUE
  )
  enumerate <- function(cursor) {
    lengths <- c(2, 3)
    lengths <- lengths[cursor + lengths <= 9]
    if (!length(lengths)) return(list(numeric()))
    unlist(lapply(lengths, function(length) {
      lapply(enumerate(cursor + length), function(suffix) c(length, suffix))
    }), recursive = FALSE)
  }
  patterns <- enumerate(1)
  objective <- vapply(patterns, function(lengths) {
    cursor <- 1
    sale <- 0
    for (length in lengths) {
      sale <- sale + merchandiser::stem_volume(
        12, 20, "demo.paraboloid", lower = cursor,
        lower_type = "height", upper = cursor + length,
        upper_type = "height", bark = "inside"
      )
      cursor <- cursor + length
    }
    sale * 7 / 2
  }, numeric(1L))
  expect_equal(sum(result$values$net_value), max(objective), tolerance = 1e-12)
  product$price <- NA_real_
  volume_result <- optimize_bucking(
    12, 20, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 9, objective = "net_cubic_ib", status = TRUE
  )
  expect_identical(volume_result$logs$nominal_length, c(3, 3, 2))
})

test_that("one-product cascade and DP agree", {
  product <- .mc_legacy_product(
    "only", 1L, lengths = as.double(c(8, 16)), min_boundary_length = 4,
    trim = 0.5, min_sed = 3, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  arguments <- list(
    dbh = 12, ht = 60, model = "demo.paraboloid", products = product,
    stump_ht = 1, utilization_height = 34, status = TRUE
  )
  cascade <- do.call(merchandise, arguments)
  dynamic <- do.call(optimize_bucking, arguments)
  expect_identical(dynamic$logs, cascade$logs)
  expect_identical(dynamic$residuals, cascade$residuals)
})

test_that("DP is invariant at one and eight threads", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  run <- function(threads) {
    merchandiser::with_threads(threads, optimize_bucking(
      rep(12, 2), rep(80, 2), acceptance_model_id,
      acceptance_products_a(), id = seq_len(2), spcd = 202,
      stump_ht = 1, status = TRUE
    ))
  }
  one <- run(1)
  eight <- run(8)
  expect_identical(one$logs, eight$logs)
  expect_identical(one$trees, eight$trees)
  expect_identical(one$residuals, eight$residuals)
  expect_identical(one$values, eight$values)
})

test_that("priced DP improves stand value over the cascade", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  products <- acceptance_products_a()
  products$price <- as.double(c(0, 10, 1))
  arguments <- list(
    dbh = rep(12, 2), ht = rep(80, 2), model = acceptance_model_id,
    products = products, id = seq_len(2), spcd = 202, stump_ht = 1,
    currency = "USD", status = TRUE
  )
  cascade <- do.call(merchandise, arguments)
  dynamic <- do.call(optimize_bucking, arguments)
  expect_gt(sum(dynamic$values$net_value), sum(cascade$values$net_value))
})

test_that("DP properties reconcile and respect every selected product", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  products <- acceptance_products_b()
  result <- optimize_bucking(
    rep(14, 2), rep(75, 2), acceptance_model_id, products,
    id = seq_len(2), spcd = 131,
    defects = defects_from_stoppers(
      seq_len(2), rep(75, 2), saw_stop = 40, pulp_stop = 60,
      stump_ht = 1
    ),
    stump_ht = 1, status = TRUE
  )
  expect_true(all(result$logs$end_height > result$logs$start_height))
  for (id in unique(result$logs$id)) {
    selected <- result$logs$id == id
    expect_true(all(diff(result$logs$start_height[selected]) >= 0))
  }
  product <- match(result$logs$product, products$product)
  expect_true(all(result$logs$led_ob >= products$min_led[product]))
  expect_true(all(result$logs$sed_ob >= products$min_sed[product]))
  counts <- aggregate(
    result$logs$log,
    result$logs[c("id", "segment", "stage", "product")],
    length
  )
  limits <- products$max_logs_per_segment[match(counts$product, products$product)]
  expect_true(all(is.na(limits) | counts$x <= limits))
  ledger <- with(result$trees,
    log_net_cubic_ib + stump_cubic_ib + top_cubic_ib + cull_cubic_ib +
      break_cubic_ib + located_deduction_cubic_ib +
      posthoc_deduction_cubic_ib
  )
  expect_equal(result$trees$gross_stem_cubic_ib, ledger, tolerance = 1e-8)
})

test_that("value outputs use flat product prices", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  products <- acceptance_products_a()
  products$price <- as.double(c(0, 1, 1))
  base <- optimize_bucking(
    12, 80, acceptance_model_id, products, spcd = 202, stump_ht = 1,
    currency = "USD", status = TRUE
  )
  expect_true(all(c(
    "gross_value", "located_net_value", "net_value"
  ) %in% names(base$logs)))
  expect_true(all(c(
    "gross_value", "located_net_value", "net_value"
  ) %in% names(base$trees)))
  expect_equal(base$trees$net_value, sum(base$logs$net_value))
  expect_equal(
    base$values$gross_value,
    base$values$net_value + base$values$located_deduction_value +
      base$values$posthoc_deduction_value
  )
  expect_equal(
    sum(product_summary(base, table = "values")$net_value),
    sum(base$values$net_value)
  )
})

test_that("DP validates objective and unpriced controls", {
  products <- products(
    .mc_legacy_product(
      "priced", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
      scale_rule = "cubic", measurement_quantity = "cubic",
      scale_unit = "ft3", scale_bark_basis = "ib", price = 1
    ),
    .mc_legacy_product(
      "unpriced", 2L, lengths = 8, min_sed = 0, diameter_basis = "ib",
      scale_rule = "cubic", measurement_quantity = "cubic",
      scale_unit = "ft3", scale_bark_basis = "ib"
    )
  )
  expect_error(
    optimize_bucking(
      12, 40, "demo.paraboloid", products, currency = "USD",
      objective = "value", unpriced = "error"
    ),
    "must be priced"
  )
  excluded <- optimize_bucking(
    12, 40, "demo.paraboloid", products, currency = "USD",
    objective = "value", unpriced = "exclude", status = TRUE
  )
  expect_true(all(excluded$logs$product == "priced"))
  expect_error(
    optimize_bucking(12, 40, "demo.paraboloid", products, objective = "bad"),
    "objective must be one of"
  )
})

test_that("priced DP evaluates every scale family", {
  for (rule in merchandiser:::.mc_scale_rules) {
    board <- grepl("^(scribner|international|doyle)", rule)
    product <- .mc_legacy_product(
      "scaled", 1L, lengths = 16, min_sed = 0, diameter_basis = "ib",
      scale_rule = rule,
      measurement_quantity = if (board) "board_foot" else "cubic",
      scale_unit = if (board) "board_foot" else "ft3",
      scale_bark_basis = "ib", price = 1, allow_lower_products = FALSE
    )
    result <- optimize_bucking(
      20, 40, "demo.paraboloid", product, stump_ht = 1,
      utilization_height = 17, currency = "USD", objective = "value",
      status = TRUE
    )
    expect_identical(result$trees$status, 0L, info = rule)
    expect_true(is.finite(result$values$net_value), info = rule)
  }
})

test_that("priced DP evaluates cord and green-weight measurement quantities", {
  green <- .mc_legacy_product(
    "green", 1L, lengths = 16, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "green_weight",
    scale_unit = "green_short_ton", scale_bark_basis = "ob", price = 100,
    allow_lower_products = FALSE
  )
  green_result <- optimize_bucking(
    12, 40, "demo.paraboloid", green, spcd = 202, stump_ht = 1,
    utilization_height = 17, currency = "USD", status = TRUE
  )
  expect_identical(green_result$trees$status, 0L)
  expect_equal(
    green_result$logs$net_value,
    green_result$logs$net_scale * 100
  )

  cord <- .mc_legacy_product(
    "cord", 1L, lengths = 16, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cord",
    scale_unit = "cord", scale_bark_basis = "ib", cord_solid_fraction = 0.75,
    price = 100, allow_lower_products = FALSE
  )
  cord_result <- optimize_bucking(
    12, 40, "demo.paraboloid", cord, stump_ht = 1,
    utilization_height = 17, currency = "USD", status = TRUE
  )
  expect_identical(cord_result$trees$status, 0L)
  expect_equal(
    cord_result$logs$net_value,
    cord_result$logs$net_scale * 100
  )
})

test_that("DP follows declared NVEL segmentation policies", {
  product <- .mc_legacy_product(
    "nvel", 1L, min_length = 8, max_length = 20, length_step = 1,
    length_parity = "even", trim = 0.5, min_sed = 0,
    diameter_basis = "ib", segmentation_policy = "nvel_opt_22",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  result <- optimize_bucking(
    20, 80, "demo.paraboloid", product, stump_ht = 1,
    utilization_height = 47, status = TRUE
  )
  expect_identical(result$logs$nominal_length, c(20, 12, 12))
  expect_identical(result$trees$status, 0L)
})
