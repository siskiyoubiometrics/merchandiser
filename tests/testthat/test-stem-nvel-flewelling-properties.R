test_that("diameter at breast height honors an explicit bark ratio", {
  ids <- c("F00FW2W202", "I00FW2W122", "A00FW2W098")
  result <- dib(12, 80, 4.5, ids, bark_ratio = 0.9)
  expect_equal(result, rep(10.8, length(ids)), tolerance = 1e-12)
})

test_that("representative profiles decrease above the butt", {
  ids <- c(
    "F00FW2W202", "F00FW2W263", "F00FW2W242", "I00FW2W122",
    "200FW2W108", "A00FW2W098"
  )
  heights <- seq(8, 72, by = 4)
  for (id in ids) {
    values <- dib(12, 80, heights, id)
    expect_true(all(diff(values) <= 1e-10), info = id)
  }
})

test_that("height inversion round trips the highest crossing", {
  ids <- c("F00FW2W202", "I15FW2W122", "200FW2W108", "A02FW2W098")
  heights <- c(15, 25, 35, 45)
  targets <- dib(12, 80, heights, ids)
  recovered <- height_at_dib(12, 80, targets, ids, status = TRUE)
  expect_true(all(recovered$status %in% c(0L, 102L)))
  expect_equal(recovered$value, heights, tolerance = 1e-4)
})

test_that("conditioned height inversion round trips above its upper point", {
  heights <- c(45, 50, 60, 70)
  targets <- dib(
    12, 80, heights, "A00FW3W098", upper_ht1 = 40, upper_d1 = 8
  )
  recovered <- height_at_dib(
    12, 80, targets, "A00FW3W098", upper_ht1 = 40, upper_d1 = 8,
    status = TRUE
  )
  expect_identical(recovered$status, rep(0L, length(heights)))
  expect_equal(recovered$value, heights, tolerance = 1e-4)
})

test_that("materialized profiles reconcile with direct volume", {
  profile <- stem_profile(
    12, 40, "I00FW2W122", step = 24, lower = 0,
    lower_type = "height"
  )
  expected <- stem_volume(
    12, 40, "I00FW2W122", lower = 0, lower_type = "height"
  )
  expect_equal(tail(profile$cum_volume_ib, 1L), expected, tolerance = 1e-12)

  conditioned <- stem_profile(
    12, 80, "A00FW3W098", step = 24, lower = 0,
    lower_type = "height", upper_ht1 = 40, upper_d1 = 8
  )
  conditioned_volume <- stem_volume(
    12, 80, "A00FW3W098", lower = 0, lower_type = "height",
    upper_ht1 = 40, upper_d1 = 8
  )
  expect_equal(
    tail(conditioned$cum_volume_ib, 1L), conditioned_volume,
    tolerance = 1e-12
  )
})

test_that("one and four threads produce identical Flewelling values", {
  heights <- rep(seq(1, 79, length.out = 200), 2)
  ids <- rep(c("F06FW2W202", "I15FW2W122"), each = 200)
  one <- with_threads(1, dib(12, 80, heights, ids))
  four <- with_threads(4, dib(12, 80, heights, ids))
  expect_identical(one, four)
})
