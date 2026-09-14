test_that("imported measurement aliases survive validation and combination in metric calls", {
  frame <- data.frame(
    product = "imported", priority = 1, min_length = 5, max_length = 5,
    length_step = 1, min_sed = 1, diameter_basis = "ib", sold_by = "cubic"
  )
  for (alias in c("cubic", "green_ton")) {
    frame$sold_by <- alias
    run <- function(p) {
      merchandise(40, 25, "demo.paraboloid", p, spcd = 202,
                  units = "metric", status = TRUE)
    }
    direct <- run(frame)
    validated <- run(validate_products(frame))
    combined <- run(products(frame))
    expect_identical(validated$logs, direct$logs)
    expect_identical(combined$logs, direct$logs)
    expect_true(all(direct$logs$scale_unit ==
                      if (alias == "cubic") "m3" else "green_metric_ton"))
  }
})

test_that("named reporting rejects conflicting measurement declarations", {
  p <- product("saw", 1, lengths = 16, min_sed = 4,
               diameter_basis = "ib", sold_by = "cubic")
  for (field in c("scale_rule", "measurement_quantity", "scale_unit", "scale_bark_basis")) {
    definition <- data.frame(name = "ambiguous", sold_by = "cubic")
    definition[[field]] <- "conflicting"
    expect_error(
      merchandise(20, 80, "demo.paraboloid", p, report_also = definition),
      "Supply sold_by alone"
    )
  }
})

test_that("zero-trim profile volumes convert to the declared measurement quantity", {
  for (units in c("imperial", "metric")) {
    run <- function(sold_by, fraction = NA_real_) {
      definition <- .mc_sold_by(sold_by, units)
      # Preserve the fixed-unit arithmetic regression through the private adapter.
      p <- do.call(.mc_legacy_product, c(list(
        product = "saw", priority = 1, lengths = if (units == "imperial") 16 else 5,
        min_sed = 4, diameter_basis = "ib", cord_solid_fraction = fraction, price = 10
      ), as.list(definition[setdiff(names(definition), "sold_by")])))
      merchandise(40, 80, "demo.paraboloid", p, spcd = 202, units = units,
                  currency = "USD", status = TRUE)
    }
    feet <- run("cubic_ft_ib")
    meters <- run("cubic_m_ib")
    cords <- run("cord_ib", 0.5)
    expect_equal(meters$logs$net_scale, feet$logs$net_scale * 0.028316846592)
    expect_equal(cords$logs$net_scale, feet$logs$net_scale / (128 * 0.5))
    expect_equal(meters$values$net_value, meters$logs$net_scale * 10)
    expect_equal(cords$values$net_value, cords$logs$net_scale * 10)
  }
})

test_that("unavailable weight reports warn once or return integer status", {
  p <- product("saw", 1, lengths = 16, min_sed = 4,
               diameter_basis = "ib", sold_by = "cubic")
  run <- function(status) {
    merchandise(c(16, 20), 80, "demo.paraboloid", p, status = status,
                report_also = c("green_short_ton", "green_metric_ton"))
  }
  warnings <- character()
  result <- withCallingHandlers(run(FALSE), warning = function(warning) {
    warnings <<- c(warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1)
  expect_match(warnings, "scale_input_unavailable")
  expect_true(all(is.na(result$scales$net[!result$scales$is_product])))
  expect_no_warning(diagnosed <- run(TRUE))
  expect_identical(diagnosed$trees$status, c(411L, 411L))
})
