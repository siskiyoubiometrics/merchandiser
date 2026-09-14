test_that("mc_version returns the installed package version", {
  version <- mc_version()
  expect_type(version, "character")
  expect_length(version, 1L)
  expect_identical(
    version,
    as.character(utils::packageVersion("merchandiser"))
  )
})
