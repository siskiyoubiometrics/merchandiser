pruned_r10r4_exclusions <- list(
  r10_taper = list(
    diameter = c(missing_bru_dispatch = 77L),
    height = c(
      missing_bru_dispatch = 77L,
      unsupported_dispatch_species = 24L
    )
  ),
  r4_driver = list(
    diameter = c(nonzero_errflag = 0L),
    height = c(zero_outside_contract = 125L, nonfinite_oracle = 0L)
  )
)

test_that("pruned double Region 10 and Region 4 fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c("r10_taper", "r4_driver")) {
    .check_r10r4_fixture(
      .r10r4_fixture_path(root, family, "double"), family, "double",
      pruned_r10r4_exclusions[[family]]
    )
  }
})

test_that("pruned single Region 10 and Region 4 fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c("r10_taper", "r4_driver")) {
    double <- .check_r10r4_fixture(
      .r10r4_fixture_path(root, family, "double"), family, "double",
      pruned_r10r4_exclusions[[family]]
    )
    single_exclusions <- pruned_r10r4_exclusions[[family]]
    if (identical(family, "r4_driver")) {
      single_exclusions$height[["nonfinite_oracle"]] <- 2L
    }
    .check_r10r4_fixture(
      .r10r4_fixture_path(root, family, "single"), family, "single",
      single_exclusions, double
    )
  }
})
