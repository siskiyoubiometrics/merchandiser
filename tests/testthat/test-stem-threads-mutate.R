test_that("thread count setters are scoped and validated", {
  original <- threads()
  on.exit(threads(original), add = TRUE)
  cores <- parallel::detectCores()
  if (is.na(cores)) cores <- 1L

  expect_identical(suppressWarnings(threads(2)), original)
  expect_identical(threads(), min(2L, cores))
  value <- suppressWarnings(with_threads(3, threads()))
  expect_identical(value, min(3L, cores))
  expect_identical(threads(), min(2L, cores))
  expect_error(threads(0), "at least one")
  expect_error(threads(1.5), "integer")
  expect_warning(threads(1e+20), "n.*clamped")
  expect_warning(value <- merchandiser:::.thread_count_from_env("999999999999"), "clamped")
  expect_identical(value, threads())
})

test_that("compiled results are exactly invariant to thread count", {
  set.seed(42)
  dbh <- runif(1000, 5, 40)
  ht <- runif(1000, 30, 180)
  h <- runif(1000, 5, 25)
  h <- pmin(h, ht)

  one <- with_threads(1, dib(
    dbh = dbh, ht = ht, h = h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value)
  four <- with_threads(4, dib(
    dbh = dbh, ht = ht, h = h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value)
  expect_identical(four, one)

  volume_one <- with_threads(1, stem_volume(
    dbh = dbh, ht = ht, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value)
  volume_four <- with_threads(4, stem_volume(
    dbh = dbh, ht = ht, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value)
  expect_identical(volume_four, volume_one)
})

test_that("size-stable calls work inside dplyr mutate", {
  trees <- data.frame(dbh = c(10, 12), ht = c(60, 80), h = c(10, 20))
  result <- dplyr::mutate(trees, dib_at_h = dib(
    dbh = dbh, ht = ht, h = h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value, volume_ib = stem_volume(
    dbh = dbh,
    ht = ht, model = "demo.paraboloid", spcd = .interface_spcd("demo.paraboloid")
  )$value)
  expect_equal(nrow(result), nrow(trees))
  expect_true(all(is.finite(result$dib_at_h)))
  expect_true(all(is.finite(result$volume_ib)))
})

test_that("single-tree console calls return simple values", {
  expect_type(
    dib(
      dbh = 12, ht = 80, h = 20, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    "double"
  )
  expect_length(
    dob(
      dbh = 12, ht = 80, h = 20, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    1L
  )
  expect_length(
    height_at_dib(
      dbh = 12, ht = 80, dib = 6, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    1L
  )
  expect_length(
    height_at_dob(
      dbh = 12, ht = 80, dob = 6, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    1L
  )
  expect_length(
    stem_volume(
      dbh = 12, ht = 80, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    1L
  )
})
