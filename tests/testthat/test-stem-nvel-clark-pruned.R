test_that("pruned double Clark fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c("clark_r8", "clark_r9")) {
    .check_clark_fixture(
      .clark_fixture_path(root, family, "double"), "double"
    )
  }
})

test_that("pruned single Clark fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c("clark_r8", "clark_r9")) {
    .check_clark_fixture(
      .clark_fixture_path(root, family, "single"), "single"
    )
  }
})
