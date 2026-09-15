# This is a permitted guarded skip: complete oracle fixtures are protected and supplied
# outside the package checkout through TREEVOLUME_FIXTURES.
test_that("full regional small-taper fixtures agree when supplied", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = Sys.getenv(
    "TREEVOLUME_FIXTURES",
    unset = ""
  ))
  if (!dir.exists(root))
    skip("MERCHANDISER_FIXTURES directory is missing")
  for (precision in c("double", "single")) {
    for (family in c(
      "r1_taper", "r2_taper", "r5_taper", "r12_taper", "blm_taper",
      "behre_taper"
    )) {
      .check_smalltaper_fixture(.smalltaper_fixture_path(root, family, precision),
        precision,
        record = TRUE, expected_source_domain = if (identical(family, "r1_taper")) {
          1986L
        } else {
          0L
        }, expected_below_contract = switch(family,
          r1_taper = 12L,
          blm_taper = 3441L,
          behre_taper = 214L,
          0L
        )
      )
      gc(verbose = FALSE)
    }
  }
})
