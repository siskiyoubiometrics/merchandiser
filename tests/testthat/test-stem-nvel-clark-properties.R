test_that("representative Clark profiles taper above breast height", {
  cases <- list(list("814CLKE100", 60), list("817CLKE100", 60), list(
    "818CLKE100",
    NULL
  ), list(
    "811CLKE100",
    NULL
  ), list("900CLKE001", NULL), list("900CLKE621", NULL))
  heights <- seq(4.5, 79, length.out = 200)
  for (case in cases) {
    arguments <- list(
      dbh = 12, ht = 80, h = heights, model = case[[1L]],
      spcd = .interface_spcd(case[[1L]])
    )
    if (!is.null(case[[2L]])) {
      arguments$upper_ht1 <- case[[2L]]
    }
    values <- do.call(.oracle_dib, arguments)$value
    expect_true(all(values >= 0), info = case[[1L]])
    expect_lte(tail(values, 1L), values[[1L]])
  }
})

test_that("Clark analytic volume is additive on geometric bounds", {
  ids <- c("811CLKE100", "900CLKE001", "900CLKE621", "900CLKE823")
  for (id in ids) {
    whole <- .oracle_stem_volume(
      dbh = 12, ht = 80, model = id, lower = 1, lower_type = "height",
      upper = 79, upper_type = "height", spcd = .interface_spcd(id)
    )$value
    lower <- .oracle_stem_volume(
      dbh = 12, ht = 80, model = id, lower = 1, lower_type = "height",
      upper = 40, upper_type = "height", spcd = .interface_spcd(id)
    )$value
    upper <- .oracle_stem_volume(
      dbh = 12, ht = 80, model = id, lower = 40, lower_type = "height",
      upper = 79, upper_type = "height", spcd = .interface_spcd(id)
    )$value
    expect_equal(whole, lower + upper, tolerance = 1e-12, info = id)
  }
})

test_that("overlapping Region 8 segments return the highest crossing", {
  result <- .oracle_height_at_dib(
    dbh = 6, ht = 100, dib = 5, model = "834CLKE110", upper_ht1 = 75,
    spcd = .interface_spcd("834CLKE110")
  )
  expect_identical(result$status, 102L)
  expect_gt(result$value, 4.5)
  expect_equal(.oracle_dib(
    dbh = 6, ht = 100, h = result$value, model = "834CLKE110", upper_ht1 = 75,
    spcd = .interface_spcd("834CLKE110")
  )$value, 5, tolerance = 1e-12)
})

test_that("one and four threads produce identical Clark results", {
  height <- rep(seq(1, 79, length.out = 200), 3)
  ids <- rep(c("814CLKE100", "811CLKE100", "900CLKE823"), each = 200)
  upper <- rep(60, 600)
  one <- with_threads(1, .oracle_dib(
    dbh = 12, ht = 80, h = height, model = ids, upper_ht1 = upper,
    spcd = .interface_spcd(ids)
  )$value)
  four <- with_threads(4, .oracle_dib(
    dbh = 12, ht = 80, h = height, model = ids, upper_ht1 = upper,
    spcd = .interface_spcd(ids)
  )$value)
  expect_identical(one, four)
})

test_that("Region 8 old-driver VOL1 remains internal", {
  ids <- c("814CLKE100", "817CLKE100", "818CLKE100", "819CLKE300")
  upper <- rep(60, length(ids))
  public <- .oracle_stem_volume(
    dbh = 12, ht = 80, model = ids, upper_ht1 = upper,
    spcd = .interface_spcd(ids)
  )$value
  record <- merchandiser:::nvel_volume(12, 80, ids, upper_ht1 = upper, status = TRUE)
  expect_true(all(public > 0))
  expect_identical(record$vol_total_cu, rep(0, length(ids)))
  expect_identical(record$vol_total_cu_status, integer(length(ids)))
})
