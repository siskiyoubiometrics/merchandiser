test_that("contract acceptance case A is reproduced", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  result <- merchandise(
    dbh = 12, ht = 80, model = acceptance_model_id,
    products = acceptance_products_a(), spcd = 202, stump_ht = 1,
    utilization_top = NULL, scaling = NULL, units = "imperial", status = TRUE
  )
  expected <- data.frame(
    product = c("domestic_saw", "pulp"),
    start_height = c(1, 42), nominal_end_height = c(41, 73.674383),
    end_height = c(42, 74.174383), nominal_length = c(40, 31.674383),
    physical_length = c(41, 32.174383),
    length_kind = c("ordinary", "boundary"), led_ib = c(11.047495, 7.661999),
    sed_ib = c(7.661999, 3), log_gross_cubic_ib = c(20.210063, 5.940681),
    trim_cubic_ib = c(0.324406, 0.025597)
  )
  expect_identical(result$logs$product, expected$product)
  expect_identical(result$logs$length_kind, expected$length_kind)
  numeric <- setdiff(names(expected), c("product", "length_kind"))
  for (name in numeric) {
    expect_equal(result$logs[[name]], expected[[name]], tolerance = 3e-5)
  }
  expect_equal(result$residuals$from, c(0, 74.174383), tolerance = 3e-5)
  expect_equal(result$residuals$to, c(1, 80), tolerance = 3e-5)
  expect_identical(result$residuals$cause, c("stump", "top"))
  expect_equal(result$residuals$cubic_ib, c(0.669877, 0.142982), tolerance = 3e-5)
  expect_equal(result$trees$gross_stem_cubic_ib, 26.963603, tolerance = 3e-5)
  expect_equal(sum(result$logs$log_gross_cubic_ib), 26.150744, tolerance = 3e-5)
  expect_identical(result$trees$status, 0L)
})

test_that("contract acceptance case B is reproduced", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  defects <- defects_from_stoppers(
    1L, 75, saw_stop = 40, pulp_stop = 60, stump_ht = 1
  )
  expect_identical(defects$defect_id, c(".stopper.pulp", ".stopper.cull"))
  result <- merchandise(
    dbh = 14, ht = 75, model = acceptance_model_id,
    products = acceptance_products_b(), id = 1L, spcd = 131,
    defects = defects, stump_ht = 1, utilization_top = NULL,
    scaling = NULL, units = "imperial", status = TRUE
  )
  expected <- data.frame(
    product = c("sawtimber", "pulpwood"), start_height = c(1, 40),
    nominal_end_height = c(39.5, 59.5), end_height = c(40, 60),
    nominal_length = c(38.5, 19.5), physical_length = c(39, 20),
    length_kind = c("boundary", "boundary"),
    led_ob = c(14.343308, 9.864328), sed_ob = c(9.864328, 6.457718),
    log_gross_cubic_ib = c(26.106008, 6.141145),
    trim_cubic_ib = c(0.216475, 0.093652)
  )
  expect_identical(result$logs$product, expected$product)
  expect_identical(result$logs$length_kind, expected$length_kind)
  numeric <- setdiff(names(expected), c("product", "length_kind"))
  for (name in numeric) {
    expect_equal(result$logs[[name]], expected[[name]], tolerance = 3e-5)
  }
  expect_identical(result$residuals$cause, c("stump", "cull"))
  expect_equal(result$residuals$from, c(0, 60), tolerance = 3e-5)
  expect_equal(result$residuals$to, c(1, 75), tolerance = 3e-5)
  expect_equal(result$residuals$cubic_ib, c(0.915031, 1.381758), tolerance = 3e-5)
  expect_equal(result$trees$gross_stem_cubic_ib, 34.543942, tolerance = 3e-5)
  expect_equal(sum(result$logs$log_gross_cubic_ib), 32.247154, tolerance = 3e-5)
  expect_identical(result$trees$status, 0L)
})
