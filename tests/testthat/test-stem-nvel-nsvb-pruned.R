test_that("pruned double NSVB fixtures agree", {
  root <- testthat::test_path("fixtures")
  .check_nsvb_fixture(.nsvb_fixture_path(root, "double"), "double")
})

test_that("pruned single NSVB fixtures agree", {
  root <- testthat::test_path("fixtures")
  .check_nsvb_fixture(.nsvb_fixture_path(root, "single"), "single")
})
