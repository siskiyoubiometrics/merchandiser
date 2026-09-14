test_that("twenty calls are identical to the installed 0.1.0 packages", {
  expected <- readRDS(testthat::test_path("..", "merge-baseline.rds"))
  actual <- merge_identity_calls()
  # Plot dispatch adds metadata. Compare every original profile value unchanged.
  expected$profile <- structure(expected$profile,
                                class = c("stem_profile", "data.frame"), units = "imperial")
  expect_length(actual, 20L)
  for (name in names(expected)) {
    if (is.data.frame(expected[[name]])) {
      names(expected[[name]]) <- sub("^piece_", "log_", names(expected[[name]]))
      renamed <- c(sale_gross = "gross_scale", sale_net = "net_scale")
      columns <- names(expected[[name]])
      at <- columns %in% names(renamed)
      columns[at] <- unname(renamed[columns[at]])
      names(expected[[name]]) <- columns
    }
    expect_identical(actual[[name]], expected[[name]], info = name)
  }
})
