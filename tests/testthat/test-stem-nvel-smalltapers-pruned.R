test_that("pruned double regional small-taper fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c(
    "r1_taper", "r2_taper", "r5_taper", "r12_taper", "blm_taper",
    "behre_taper"
  )) {
    .check_smalltaper_fixture(.smalltaper_fixture_path(root, family, "double"), "double")
  }
})

test_that("pruned single regional small-taper fixtures agree", {
  root <- testthat::test_path("fixtures")
  for (family in c(
    "r1_taper", "r2_taper", "r5_taper", "r12_taper", "blm_taper",
    "behre_taper"
  )) {
    .check_smalltaper_fixture(.smalltaper_fixture_path(root, family, "single"), "single")
  }
})
