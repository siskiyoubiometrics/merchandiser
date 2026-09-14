test_that("the frozen status table has exact codes and names", {
  expected_codes <- c(0L, 1:8, 50:54, 100:103, 300L)
  expected_names <- c(
    "ok", "na_input", "dbh_nonpositive", "ht_nonpositive", "h_out_of_range",
    "diameter_nonpositive", "empty_bounds", "unknown_species",
    "outside_divisions", "unknown_model",
    "missing_input", "species_out_of_scope", "capability_missing", "kernel_error",
    "above_tip", "below_stump", "not_unique", "no_convergence",
    "library_error_base"
  )
  table <- subset(status_codes(), source == "stem model")
  expect_identical(table$code, expected_codes)
  expect_identical(table$name, expected_names)
})
