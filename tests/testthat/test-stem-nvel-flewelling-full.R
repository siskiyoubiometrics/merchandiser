# This is a permitted guarded skip: complete oracle fixtures are protected and remain
# outside the package. Set TREEVOLUME_FIXTURES to exercise them.
full_flewelling_exclusions <- list(
  flewelling_2pt = list(diameter = c(
    nonzero_errflag = 7968L,
    upper_at_or_above_total = 0L
  ), height = c(
    nonzero_errflag = 6915L, upper_at_or_above_total = 0L,
    sf_hs_bark_height = 11525L, sf_hs_conditioned_root = 0L,
    at_or_below_contract_stump = 39L
  )),
  flewelling_3pt = list(
    diameter = c(nonzero_errflag = 0L, upper_at_or_above_total = 2300L),
    height = c(
      nonzero_errflag = 0L, upper_at_or_above_total = 4593L, sf_hs_bark_height = 0L,
      sf_hs_conditioned_root = 23082L, at_or_below_contract_stump = 0L
    )
  )
)

full_r10r4_exclusions <- list(r10_taper = list(diameter = c(
  missing_bru_dispatch = 14170L
), height = c(
  missing_bru_dispatch = 12286L,
  unsupported_dispatch_species = 3833L
)), r4_driver = list(
  diameter = c(nonzero_errflag = 0L),
  height = c(zero_outside_contract = 125L, nonfinite_oracle = 0L)
))

test_that("full Flewelling fixtures agree when supplied", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = Sys.getenv(
    "TREEVOLUME_FIXTURES",
    unset = ""
  ))
  if (!dir.exists(root)) {
    skip("MERCHANDISER_FIXTURES directory is missing. full Flewelling oracle tests not run")
  }
  for (family in c("flewelling_2pt", "flewelling_3pt")) {
    double_path <- .flewelling_fixture_path(root, family, "double")
    single_path <- .flewelling_fixture_path(root, family, "single")
    expect_true(file.exists(double_path), info = double_path)
    expect_true(file.exists(single_path), info = single_path)
    actual <- .check_flewelling_fixture(double_path, "double",
      full_flewelling_exclusions[[family]],
      record = TRUE
    )
    .check_flewelling_fixture(single_path, "single", full_flewelling_exclusions[[family]],
      actual,
      record = TRUE, expected_compat_precision = if (family == "flewelling_2pt")
        5L else 8L
    )
  }
  for (family in c("r10_taper", "r4_driver")) {
    double_path <- .r10r4_fixture_path(root, family, "double")
    single_path <- .r10r4_fixture_path(root, family, "single")
    expect_true(file.exists(double_path), info = double_path)
    expect_true(file.exists(single_path), info = single_path)
    actual <- .check_r10r4_fixture(double_path, family, "double",
      full_r10r4_exclusions[[family]],
      record = TRUE
    )
    single_exclusions <- full_r10r4_exclusions[[family]]
    if (identical(family, "r4_driver")) {
      single_exclusions$height[["nonfinite_oracle"]] <- 2L
    }
    .check_r10r4_fixture(single_path, family, "single", single_exclusions, actual,
      record = TRUE
    )
  }
})
