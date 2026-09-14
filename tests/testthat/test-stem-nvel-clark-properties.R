test_that("representative Clark profiles taper above breast height", {
  cases <- list(
    list("814CLKE100", 60), list("817CLKE100", 60),
    list("818CLKE100", NULL), list("811CLKE100", NULL),
    list("900CLKE001", NULL), list("900CLKE621", NULL)
  )
  heights <- seq(4.5, 79, length.out = 200)
  for (case in cases) {
    arguments <- list(12, 80, heights, case[[1L]])
    if (!is.null(case[[2L]])) {
      arguments$upper_ht1 <- case[[2L]]
    }
    values <- do.call(dib, arguments)
    expect_true(all(values >= 0), info = case[[1L]])
    expect_lte(tail(values, 1L), values[[1L]])
  }
})

test_that("Clark analytic volume is additive on geometric bounds", {
  ids <- c("811CLKE100", "900CLKE001", "900CLKE621", "900CLKE823")
  for (id in ids) {
    whole <- stem_volume(
      12, 80, id, lower = 1, lower_type = "height",
      upper = 79, upper_type = "height"
    )
    lower <- stem_volume(
      12, 80, id, lower = 1, lower_type = "height",
      upper = 40, upper_type = "height"
    )
    upper <- stem_volume(
      12, 80, id, lower = 40, lower_type = "height",
      upper = 79, upper_type = "height"
    )
    expect_equal(whole, lower + upper, tolerance = 1e-12, info = id)
  }
})

test_that("overlapping Region 8 segments return the highest crossing", {
  result <- height_at_dib(
    6, 100, 5, "834CLKE110", upper_ht1 = 75, status = TRUE
  )
  expect_identical(result$status, 102L)
  expect_gt(result$value, 4.5)
  expect_equal(
    dib(6, 100, result$value, "834CLKE110", upper_ht1 = 75),
    5, tolerance = 1e-12
  )
})

test_that("one and four threads produce identical Clark results", {
  height <- rep(seq(1, 79, length.out = 200), 3)
  ids <- rep(c("814CLKE100", "811CLKE100", "900CLKE823"), each = 200)
  upper <- rep(60, 600)
  one <- with_threads(1, dib(12, 80, height, ids, upper_ht1 = upper))
  four <- with_threads(4, dib(12, 80, height, ids, upper_ht1 = upper))
  expect_identical(one, four)
})

test_that("Region 8 old-driver VOL1 remains internal", {
  ids <- c("814CLKE100", "817CLKE100", "818CLKE100", "819CLKE300")
  upper <- rep(60, length(ids))
  public <- stem_volume(12, 80, ids, upper_ht1 = upper)
  record <- merchandiser:::nvel_volume(
    12, 80, ids, upper_ht1 = upper, status = TRUE
  )
  expect_true(all(public > 0))
  expect_identical(record$vol_total_cu, rep(0, length(ids)))
  expect_identical(record$vol_total_cu_status, integer(length(ids)))
})
