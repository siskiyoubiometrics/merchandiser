test_that("tv_version returns the installed package version", {
  v <- tv_version()
  expect_type(v, "character")
  expect_length(v, 1)
  expect_identical(v, as.character(utils::packageVersion("merchandiser")))
})
