test_that("regional profiles have source-consistent geometric properties", {
  ids <- c("100JB2W108", "200CZ2W202", "500WO2W202", "B00BEHW202", "616BEHW231")
  for (id in ids) {
    heights <- seq(4.5, 79, length.out = 40)
    values <- dib(12, 80, heights, id)
    expect_true(all(is.finite(values)))
    expect_true(all(values >= 0))
    expect_lte(max(diff(values)), 1e-10)
    targets <- values[c(8L, 20L, 32L)]
    inverted <- height_at_dib(12, 80, targets, id, status = TRUE)
    expect_true(all(inverted$status %in% c(0L, 102L)))
    expect_equal(
      dib(12, 80, inverted$value, id), targets,
      tolerance = .1
    )
  }
})

test_that("regional inverse defaults are precise and NVEL mode rounds", {
  ids <- c("200CZ2W202", "500WO2W202", "B00BEHW202", "616BEHW231")
  target_height <- 40.037
  targets <- dib(12, 80, target_height, ids)
  precise <- height_at_dib(12, 80, targets, ids)
  source <- .with_treevolume_compat(
    "nvel", height_at_dib(12, 80, targets, ids)
  )

  expect_equal(precise, rep(target_height, length(ids)), tolerance = 1e-4)
  expect_equal(source * 10, round(source * 10), tolerance = 0)
  expect_true(any(abs(source - target_height) > 0.01))
})

test_that("smaller-family compatibility retains the public inverse interval", {
  port <- height_at_dib(4, 15, 2, "B00BEHW011", status = TRUE)
  source <- .with_treevolume_compat(
    "nvel", height_at_dib(4, 15, 2, "B00BEHW011", status = TRUE)
  )

  expect_identical(source, port)
  expect_identical(source$status, 101L)
})

test_that("Region 12 defaults invert its CALCDIA fallback", {
  id <- "H00SN2W301"
  target_height <- 3
  target <- dib(12, 80, target_height, id)
  expect_equal(height_at_dib(12, 80, target, id), target_height, tolerance = 1e-4)
  expect_identical(
    .with_treevolume_compat("nvel", height_at_dib(12, 80, target, id)),
    0
  )
})

test_that("regional kernels are vectorized and thread invariant", {
  ids <- rep(c("100JB2W108", "200CZ2W202", "500WO2W202", "B00BEHW202"), 50)
  heights <- rep(seq(5, 75, length.out = 50), each = 4)
  old <- Sys.getenv("MERCHANDISER_THREADS", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("MERCHANDISER_THREADS")
    else Sys.setenv(MERCHANDISER_THREADS = old)
  }, add = TRUE)
  .tv_runtime$threads <- 1L
  serial <- dib(12, 80, heights, ids)
  .tv_runtime$threads <- 4L
  parallel <- dib(12, 80, heights, ids)
  expect_identical(parallel, serial)
})
test_that("CZ3 preserves nonfinite source diameters below the six-inch top", {
  # Pinned r2tap.f takes SQRT(DIBCOR) without a zero clamp when TOP6 > 0
  # and HTUP < TOP6. A standalone double-precision source probe returns NaN
  # for this inconsistent upper-stem measurement, not a zero diameter.
  result <- dib(
    19, 36, 1, "200CZ3W108", upper_ht1 = 34, upper_d1 = 18.5,
    status = TRUE
  )
  expect_identical(result$status, 54L)
  expect_true(is.na(result$value))
  expect_warning(
    dib(19, 36, 1, "200CZ3W108", upper_ht1 = 34, upper_d1 = 18.5),
    "kernel_error"
  )
})
