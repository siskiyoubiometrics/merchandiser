# Complete oracle fixtures are protected and remain outside the package. Set
# TREEVOLUME_FIXTURES to exercise them. The divisions 210 and 230 fixture is
# intentionally double-only because the single-precision NVEL batch crashes.
test_that("full NSVB fixtures agree when supplied", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES",
                     unset = Sys.getenv("TREEVOLUME_FIXTURES", unset = ""))
  if (!dir.exists(root)) {
    skip("MERCHANDISER_FIXTURES directory is missing. Full NSVB oracle tests not run")
  }
  for (precision in c("double", "single")) {
    path <- .nsvb_fixture_path(root, precision)
    expect_true(file.exists(path), info = path)
    .check_nsvb_fixture(path, precision, full = TRUE, record = TRUE)
  }
})
