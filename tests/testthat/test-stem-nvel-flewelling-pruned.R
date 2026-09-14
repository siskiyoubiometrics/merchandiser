pruned_flewelling_exclusions <- list(
  flewelling_2pt = list(
    diameter = c(nonzero_errflag = 4L, upper_at_or_above_total = 0L),
    height = c(
      nonzero_errflag = 4L,
      upper_at_or_above_total = 0L,
      sf_hs_bark_height = 30L,
      sf_hs_conditioned_root = 0L,
      at_or_below_contract_stump = 0L
    )
  ),
  flewelling_3pt = list(
    diameter = c(nonzero_errflag = 0L, upper_at_or_above_total = 0L),
    height = c(
      nonzero_errflag = 0L,
      upper_at_or_above_total = 0L,
      sf_hs_bark_height = 0L,
      sf_hs_conditioned_root = 46L,
      at_or_below_contract_stump = 0L
    )
  )
)

test_that("pruned double Flewelling fixtures agree", {
  root <- testthat::test_path("fixtures")
  .check_flewelling_fixture(
    .flewelling_fixture_path(root, "flewelling_2pt", "double"), "double",
    pruned_flewelling_exclusions$flewelling_2pt
  )
  .check_flewelling_fixture(
    .flewelling_fixture_path(root, "flewelling_3pt", "double"), "double",
    pruned_flewelling_exclusions$flewelling_3pt
  )
})
test_that("pruned single Flewelling fixtures agree", {
  root <- testthat::test_path("fixtures")
  .check_flewelling_fixture(
    .flewelling_fixture_path(root, "flewelling_2pt", "single"), "single",
    pruned_flewelling_exclusions$flewelling_2pt
  )
  .check_flewelling_fixture(
    .flewelling_fixture_path(root, "flewelling_3pt", "single"), "single",
    pruned_flewelling_exclusions$flewelling_3pt
  )
})
