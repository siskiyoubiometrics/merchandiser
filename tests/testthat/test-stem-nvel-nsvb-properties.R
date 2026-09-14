test_that("NSVB biomass components reconcile", {
  result <- biomass(
    c(8, 12, 20, 30), c(45, 80, 100, 160),
    c(110, 202, 263, 621), c(1240, 1240, 1330, 1330),
    region = c(1, 6, 0, 1), forest = c(16, 12, 0, 16)
  )
  expect_equal(
    result$dry_stem_wood,
    result$dry_stump_wood + result$dry_saw_wood +
      result$dry_topwood_wood + result$dry_tip_wood,
    tolerance = 1e-10
  )
  expect_equal(
    result$dry_stem_bark,
    result$dry_stump_bark + result$dry_saw_bark +
      result$dry_topwood_bark + result$dry_tip_bark,
    tolerance = 1e-10
  )
  expect_equal(
    result$carbon,
    result$dry_agb_no_foliage *
      as.double(carbon_fraction(c(110, 202, 263, 621))),
    tolerance = 1e-11
  )
  expect_equal(result$co2e, result$carbon * 44 / 12, tolerance = 0)
})

test_that("dead-tree decay and cull follow NSVB treatment", {
  live <- biomass(16, 100, 202, 1240)
  dead <- biomass(16, 100, 202, 1240, decay_class = 3, cull = 10)
  dead_without_cull <- biomass(16, 100, 202, 1240, decay_class = 3)
  culled_live <- biomass(16, 100, 202, 1240, cull = 10)

  expect_identical(dead$dry_foliage, 0)
  expect_lt(dead$dry_stem_bark, live$dry_stem_bark)
  expect_lt(dead$dry_branches, live$dry_branches)
  expect_equal(dead, dead_without_cull, tolerance = 1e-12)
  expect_lt(culled_live$dry_agb_no_foliage, live$dry_agb_no_foliage)
  expect_equal(dead$carbon / dead$dry_agb_no_foliage, 0.506, tolerance = 1e-14)
})

test_that("NSVB reference fallbacks follow the Fortran first-match rules", {
  forest_specific <- biomass(16, 100, 202, 1240, region = 6, forest = 16)
  regional <- biomass(16, 100, 202, 1240, region = 6, forest = 0)
  expect_identical(
    forest_specific[c("dry_agb_no_foliage", "dry_stem_wood")],
    regional[c("dry_agb_no_foliage", "dry_stem_wood")]
  )
  expect_equal(
    forest_specific$green_stem_wood / regional$green_stem_wood,
    61 / 60,
    tolerance = 1e-14
  )

  zero_density <- biomass(12, 80, 5155, 0, status = TRUE)
  statuses <- zero_density[grep("_status$", names(zero_density))]
  expect_true(all(as.matrix(statuses) == 0L))
  expect_true(all(is.finite(as.double(zero_density[1, 1:30]))))
})

test_that("NSVB profiles round trip and volumes reconcile", {
  ids <- c("NVBM240110", "NVBM240202", "NVBM330263", "NVB0210122")
  heights <- c(15, 30, 45, 60)
  targets <- dib(16, 80, heights, ids)
  recovered <- height_at_dib(16, 80, targets, ids, status = TRUE)
  expect_identical(recovered$status, rep(0L, length(ids)))
  expect_equal(recovered$value, heights, tolerance = 1e-4)

  for (id in ids) {
    profile <- stem_profile(16, 80, id, step = 24)
    direct <- stem_volume(16, 80, id)
    expect_equal(tail(profile$cum_volume_ib, 1L), direct, tolerance = 1e-12)
    upper_dib <- dib(16, 80, seq(8, 72, by = 4), id)
    expect_true(all(diff(upper_dib) <= 0), info = id)
  }
})

test_that("one and four threads produce identical NSVB outputs", {
  count <- 200L
  dbh <- seq(6, 30, length.out = count)
  ht <- seq(30, 160, length.out = count)
  spcd <- rep(c(110, 202, 263, 621), length.out = count)
  division <- rep(c(1240, 1330), length.out = count)
  one <- with_threads(1, biomass(dbh, ht, spcd, division))
  four <- with_threads(4, biomass(dbh, ht, spcd, division))
  expect_identical(one, four)

  heights <- rep(seq(1, 79, length.out = 100), 2)
  ids <- rep(c("NVBM240202", "NVBM330263"), each = 100)
  one_profile <- with_threads(1, dib(16, 80, heights, ids))
  four_profile <- with_threads(4, dib(16, 80, heights, ids))
  expect_identical(one_profile, four_profile)
})
