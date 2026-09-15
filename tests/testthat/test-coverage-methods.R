test_that("coverage merch plots reject unknown tree identifiers", {
  x <- merchandise(1, 20, 80, 202, coverage_product(), model = "F00FW2W202")
  expect_error(plot(x, tree_id = 99), "tree_id")
})

test_that("coverage stem profile plots return invisibly under a pdf device", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  profile <- stem_profile(1, 20, 80, 202, model = "F00FW2W202")
  result <- withVisible(plot(profile))
  expect_false(result$visible)
  expect_identical(result$value, profile)
})

test_that("coverage height fit plots return invisibly under a pdf device", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  trees <- example_trees_pnw[example_trees_pnw$spcd == 202, ]
  fit <- fit_height(trees$dbh, trees$ht, trees$spcd)
  result <- withVisible(plot(fit))
  expect_false(result$visible)
  expect_identical(result$value, fit)
})

test_that("coverage product and defect print methods show their headers", {
  expect_output(print(products(coverage_product())), "<products>", fixed = TRUE)
  expect_output(print(defect(1, 10, 20, "cull")), "<merch_defects>", fixed = TRUE)
})

test_that("coverage status table columns retain their public order", {
  expect_identical(names(status_codes()), c("status", "name", "category", "description", "source"))
})

test_that("coverage merchandise retains its explicit public signature", {
  # Source: NAMESPACE export and R/merchandise.R public usage contract for 0.5.0.
  expect_identical(names(formals(merchandise)), c(
    "tree_id", "dbh", "ht", "spcd", "products", "model", "taper_map", "age",
    "pruned_ht", "defects", "stump_ht", "strategy", "quiet", "..."
  ))
})
