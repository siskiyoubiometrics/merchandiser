test_that("diameter at breast height honors an explicit bark ratio", {
  ids <- c("F00FW2W202", "I00FW2W122", "A00FW2W098")
  result <- .oracle_dib(
    dbh = 12, ht = 80, h = 4.5, model = ids, bark_ratio = 0.9,
    spcd = .interface_spcd(ids)
  )$value
  expect_equal(result, rep(10.8, length(ids)), tolerance = 1e-12)
})

test_that("representative profiles decrease above the butt", {
  ids <- c(
    "F00FW2W202", "F00FW2W263", "F00FW2W242", "I00FW2W122", "200FW2W108",
    "A00FW2W098"
  )
  heights <- seq(8, 72, by = 4)
  for (id in ids) {
    values <- .oracle_dib(
      dbh = 12, ht = 80, h = heights, model = id,
      spcd = .interface_spcd(id)
    )$value
    expect_true(all(diff(values) <= 1e-10), info = id)
  }
})

test_that("height inversion round trips the highest crossing", {
  ids <- c("F00FW2W202", "I15FW2W122", "200FW2W108", "A02FW2W098")
  heights <- c(15, 25, 35, 45)
  targets <- .oracle_dib(
    dbh = 12, ht = 80, h = heights, model = ids,
    spcd = .interface_spcd(ids)
  )$value
  recovered <- .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = targets, model = ids,
    spcd = .interface_spcd(ids)
  )
  expect_true(all(recovered$status %in% c(0L, 102L)))
  expect_equal(recovered$value, heights, tolerance = 1e-04)
})

test_that("conditioned height inversion round trips above its upper point", {
  heights <- c(45, 50, 60, 70)
  targets <- .oracle_dib(
    dbh = 12, ht = 80, h = heights, model = "A00FW3W098", upper_ht1 = 40, upper_d1 = 8,
    spcd = .interface_spcd("A00FW3W098")
  )$value
  recovered <- .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = targets, model = "A00FW3W098", upper_ht1 = 40,
    upper_d1 = 8, spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(recovered$status, rep(0L, length(heights)))
  expect_equal(recovered$value, heights, tolerance = 1e-04)
})

test_that("materialized profiles reconcile with direct volume", {
  profile <- stem_profile(
    dbh = 12,
    ht = 40,
    model = "I00FW2W122",
    step = 2,
    from = 0,
    spcd = .interface_spcd("I00FW2W122"),
    tree_id = seq_along(rep_len(12, max(
      length(12),
      length(40)
    )))
  )
  expected <- .oracle_stem_volume(
    dbh = 12, ht = 40, model = "I00FW2W122", lower = 0, lower_type = "height",
    spcd = .interface_spcd("I00FW2W122")
  )$value
  expect_equal(tail(profile$cum_volume_ib, 1L), expected, tolerance = 1e-12)

  conditioned <- stem_profile(
    dbh = 12,
    ht = 80,
    model = "A00FW3W098",
    step = 2,
    from = 0,
    upper_ht1 = 40,
    upper_d1 = 8,
    spcd = .interface_spcd(
      "A00FW3W098"
    ),
    tree_id = seq_along(rep_len(12, max(length(12), length(80))))
  )
  conditioned_volume <- .oracle_stem_volume(
    dbh = 12, ht = 80, model = "A00FW3W098", lower = 0, lower_type = "height",
    upper_ht1 = 40, upper_d1 = 8, spcd = .interface_spcd("A00FW3W098")
  )$value
  expect_equal(tail(conditioned$cum_volume_ib, 1L), conditioned_volume, tolerance = 1e-12)
})

test_that("one and four threads produce identical Flewelling values", {
  heights <- rep(seq(1, 79, length.out = 200), 2)
  ids <- rep(c("F06FW2W202", "I15FW2W122"), each = 200)
  one <- with_threads(1, .oracle_dib(
    dbh = 12, ht = 80, h = heights, model = ids,
    spcd = .interface_spcd(ids)
  )$value)
  four <- with_threads(4, .oracle_dib(
    dbh = 12, ht = 80, h = heights, model = ids,
    spcd = .interface_spcd(ids)
  )$value)
  expect_identical(one, four)
})
