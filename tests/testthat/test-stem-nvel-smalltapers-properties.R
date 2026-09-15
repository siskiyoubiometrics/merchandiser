test_that("regional profiles have source-consistent geometric properties", {
  ids <- c("100JB2W108", "200CZ2W202", "500WO2W202", "B00BEHW202", "616BEHW231")
  for (id in ids) {
    heights <- seq(4.5, 79, length.out = 40)
    values <- .oracle_dib(
      dbh = 12, ht = 80, h = heights, model = id,
      spcd = .interface_spcd(id)
    )$value
    expect_true(all(is.finite(values)))
    expect_true(all(values >= 0))
    expect_lte(max(diff(values)), 1e-10)
    targets <- values[c(8L, 20L, 32L)]
    inverted <- .oracle_height_at_dib(
      dbh = 12, ht = 80, dib = targets, model = id,
      spcd = .interface_spcd(id)
    )
    expect_true(all(inverted$status %in% c(0L, 102L)))
    expect_equal(
      .oracle_dib(
        dbh = 12, ht = 80, h = inverted$value, model = id,
        spcd = .interface_spcd(id)
      )$value,
      targets,
      tolerance = 0.1
    )
  }
})

test_that("regional inverse defaults are precise and NVEL mode rounds", {
  ids <- c("200CZ2W202", "500WO2W202", "B00BEHW202", "616BEHW231")
  target_height <- 40.037
  targets <- .oracle_dib(
    dbh = 12, ht = 80, h = target_height, model = ids,
    spcd = .interface_spcd(ids)
  )$value
  precise <- .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = targets, model = ids,
    spcd = .interface_spcd(ids)
  )$value
  source <- .with_treevolume_compat("nvel", .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = targets,
    model = ids, spcd = .interface_spcd(ids)
  )$value)

  expect_equal(precise, rep(target_height, length(ids)), tolerance = 1e-04)
  expect_equal(source * 10, round(source * 10), tolerance = 0)
  expect_true(any(abs(source - target_height) > 0.01))
})

test_that("smaller-family compatibility retains the public inverse interval", {
  port <- .oracle_height_at_dib(
    dbh = 4, ht = 15, dib = 2, model = "B00BEHW011",
    spcd = .interface_spcd("B00BEHW011")
  )
  source <- .with_treevolume_compat("nvel", .oracle_height_at_dib(
    dbh = 4, ht = 15, dib = 2, model = "B00BEHW011",
    spcd = .interface_spcd("B00BEHW011")
  ))

  expect_identical(source, port)
  expect_identical(source$status, 101L)
})

test_that("Region 12 defaults invert its CALCDIA fallback", {
  id <- "H00SN2W301"
  target_height <- 3
  target <- .oracle_dib(
    dbh = 12, ht = 80, h = target_height, model = id,
    spcd = .interface_spcd(id)
  )$value
  expect_equal(
    .oracle_height_at_dib(
      dbh = 12, ht = 80, dib = target, model = id,
      spcd = .interface_spcd(id)
    )$value,
    target_height,
    tolerance = 1e-04
  )
  expect_identical(.with_treevolume_compat("nvel", .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = target,
    model = id, spcd = .interface_spcd(id)
  )$value), 0)
})

test_that("regional kernels are vectorized and thread invariant", {
  ids <- rep(c("100JB2W108", "200CZ2W202", "500WO2W202", "B00BEHW202"), 50)
  heights <- rep(seq(5, 75, length.out = 50), each = 4)
  old <- Sys.getenv("MERCHANDISER_THREADS", unset = NA_character_)
  on.exit(
    {
      if (is.na(old)) Sys.unsetenv("MERCHANDISER_THREADS") else Sys.setenv(
        MERCHANDISER_THREADS = old
      )
    },
    add = TRUE
  )
  .tv_runtime$threads <- 1L
  serial <- .oracle_dib(dbh = 12, ht = 80, h = heights, model = ids, spcd = .interface_spcd(
    ids
  ))$value
  .tv_runtime$threads <- 4L
  parallel <- .oracle_dib(
    dbh = 12, ht = 80, h = heights, model = ids,
    spcd = .interface_spcd(ids)
  )$value
  expect_identical(parallel, serial)
})
test_that("CZ3 preserves nonfinite source diameters below the six-inch top", {
  # Pinned r2tap.f takes SQRT(DIBCOR) without a zero clamp when TOP6 > 0 and HTUP < TOP6. A
  # standalone double-precision source probe returns NaN for this inconsistent upper-stem
  # measurement, not a zero diameter.
  result <- .oracle_dib(
    dbh = 19, ht = 36, h = 1, model = "200CZ3W108", upper_ht1 = 34, upper_d1 = 18.5,
    spcd = .interface_spcd("200CZ3W108")
  )
  expect_identical(result$status, 54L)
  expect_true(is.na(result$value))
  expect_silent(.oracle_dib(
    dbh = 19, ht = 36, h = 1, model = "200CZ3W108", upper_ht1 = 34, upper_d1 = 18.5,
    spcd = .interface_spcd("200CZ3W108")
  )$value)
})
