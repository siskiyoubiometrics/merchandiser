for (case in c("negative start", "above tree", "unknown tree", "unknown product", "cull product")) {
  test_that(paste("coverage defect validation rejects", case), {
    record <- switch(case,
      "negative start" = defect(1, -1, 20, "cull"),
      "above tree" = defect(1, 81, 82, "cull"),
      "unknown tree" = defect(2, 10, 20, "cull"),
      "unknown product" = defect(1, 10, 20, "restrict", product = "absent"),
      "cull product" = defect(1, 10, 20, "cull", product = "saw")
    )
    # Source: validate_defects public codes in R/stem-status.R.
    expected <- switch(case, "negative start" = 401L, "above tree" = 401L,
                       "unknown tree" = 405L, "unknown product" = 407L, "cull product" = 409L)
    expect_identical(validate_defects(record, 1, 80, coverage_product())$status, expected)
  })
}

test_that("coverage an end record may have equal start and end heights", {
  record <- defect(1, 20, 20, "end")
  # Source: defect end convention explicitly permits equal bounds, status 0.
  expect_identical(validate_defects(record, 1, 80, coverage_product())$status, 0L)
})

test_that("coverage defect thirds aggregate volumes measured from height zero", {
  tree <- example_trees[1, ]
  height <- tree$ht
  pct <- c(10, 30, 70)
  # Source: aggregation consistency exemption. Third boundaries are ht / 3
  # measured from zero, and a weighted mean is sum(volume * percent) / sum(volume).
  lower <- stem_volume(tree$dbh, height, tree$spcd, model = tree$model,
                       from = 0, to = height / 3)$value
  middle <- stem_volume(tree$dbh, height, tree$spcd, model = tree$model,
                        from = height / 3, to = 2 * height / 3)$value
  upper <- stem_volume(tree$dbh, height, tree$spcd, model = tree$model,
                       from = 2 * height / 3, to = height)$value
  volumes <- c(lower, middle, upper)
  expect_true(length(volumes) == 3 && all(is.finite(volumes) & volumes > 0))
  x <- defect_by_thirds(tree$dbh, height, tree$spcd, pct[1], pct[2], pct[3], model = tree$model)
  # Source: a valid shipped tree and finite percentages produce a valid finite mean.
  expect_identical(x$status, 0L)
  expect_true(is.finite(x$value))
  expect_equal(x$value, sum(volumes * pct) / sum(volumes), tolerance = 1e-12)
})
