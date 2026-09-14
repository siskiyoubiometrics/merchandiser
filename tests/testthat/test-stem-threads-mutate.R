test_that("thread count setters are scoped and validated", {
  original <- threads()
  on.exit(threads(original), add = TRUE)

  expect_identical(threads(2), original)
  expect_identical(threads(), 2L)
  value <- with_threads(3, threads())
  expect_identical(value, 3L)
  expect_identical(threads(), 2L)
  expect_error(threads(0), "at least one")
  expect_error(threads(1.5), "integer")
  expect_error(threads(1e20), "integer")
  expect_identical(merchandiser:::.thread_count_from_env("999999999999"), 1L)
})

test_that("compiled results are exactly invariant to thread count", {
  set.seed(42)
  dbh <- runif(1000, 5, 40)
  ht <- runif(1000, 30, 180)
  h <- runif(1000, 5, 25)
  h <- pmin(h, ht)

  one <- with_threads(1, dib(dbh, ht, h, "demo.paraboloid"))
  four <- with_threads(4, dib(dbh, ht, h, "demo.paraboloid"))
  expect_identical(four, one)

  volume_one <- with_threads(1, stem_volume(dbh, ht, "demo.paraboloid"))
  volume_four <- with_threads(4, stem_volume(dbh, ht, "demo.paraboloid"))
  expect_identical(volume_four, volume_one)
})

test_that("size-stable calls work inside dplyr mutate", {
  skip_if_not_installed("dplyr")
  trees <- data.frame(dbh = c(10, 12), ht = c(60, 80), h = c(10, 20))
  result <- dplyr::mutate(
    trees,
    dib_at_h = dib(dbh, ht, h, "demo.paraboloid"),
    volume_ib = stem_volume(dbh, ht, "demo.paraboloid")
  )
  expect_equal(nrow(result), nrow(trees))
  expect_true(all(is.finite(result$dib_at_h)))
  expect_true(all(is.finite(result$volume_ib)))
})

test_that("single-tree console calls return simple values", {
  expect_type(dib(12, 80, 20, "demo.paraboloid"), "double")
  expect_length(dob(12, 80, 20, "demo.paraboloid"), 1L)
  expect_length(height_at_dib(12, 80, 6, "demo.paraboloid"), 1L)
  expect_length(height_at_dob(12, 80, 6, "demo.paraboloid"), 1L)
  expect_length(stem_volume(12, 80, "demo.paraboloid"), 1L)
})
