test_that("NSVB equations and dynamic patterns resolve", {
  metadata <- utils::read.csv(
    system.file("extdata", "nsvb_models.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(metadata), 45L)
  expect_true(all(has_taper_model(metadata$id)))
  expect_true(has_taper_model("NVBM261202"))
  expect_identical(get_taper_model("NVBM261202")$form, "nsvb")
  expect_true(has_taper_model("NVB0210110P"))
  expect_identical(get_taper_model("NVB0210110P")$spcd, 110L)
})

test_that("biomass public shape follows the dry mass interface", {
  result <- biomass(c(12, 16), c(80, 100), c(202, 263), c(1240, 1330))
  expect_equal(nrow(result), 2L)
  expect_identical(names(result), c(paste0("dry_", c(
    "aboveground_no_foliage", "stem_wood", "stem_bark",
    "stump_wood", "stump_bark", "saw_wood", "saw_bark", "topwood_wood", "topwood_bark",
    "tip_wood", "tip_bark", "branches", "foliage", "top_and_limb"
  )), "carbon", "tco2e", "status"))
})


test_that("internal NVEL volume record and rules are stable", {
  rules <- merchandiser:::nvel_rules(primary_top = 6, secondary_top = 4, stump = 1)
  result <- merchandiser:::nvel_volume(12, 80, "NVBM240202", rules = rules, status = TRUE)
  expect_identical(names(result)[seq_len(18L)], c(
    merchandiser:::.nvel_volume_names, "n_logs_primary",
    "n_logs_secondary", "errflag"
  ))
  expect_identical(result$errflag, 0L)
  expect_gte(result$n_logs_primary, 0L)
  expect_gte(result$n_logs_secondary, 0L)

  metric <- merchandiser:::nvel_volume(12 * 2.54, 80 * 0.3048, "NVBM240202",
    rules = merchandiser:::nvel_rules(
      primary_top = 6 *
        2.54,
      secondary_top = 4 *
        2.54,
      stump = 0.3048
    ),
    measurement_system = "metric", status = TRUE
  )
  cubic <- c(1L, 4L, 5L, 7L, 8L, 14L, 15L)
  expect_equal(as.numeric(metric[1, cubic]), as.numeric(result[1, cubic]) * 0.028316846592,
    tolerance = 1e-14
  )
  expect_identical(metric$vol_bf_gross, result$vol_bf_gross)

  expect_identical(
    merchandiser:::nvel_volume(0.5, 80, "NVBM240202", rules = rules, status = TRUE)$errflag,
    3L
  )
  expect_identical(merchandiser:::nvel_volume(12, 80, "NVBM240202",
                     rules = rules, spcd = 10000,
                     status = TRUE
                   )$errflag, 6L)

  expect_error(merchandiser:::nvel_rules(option = 99), "segmentation")
  expect_error(
    merchandiser:::nvel_volume(12, 80, "NVBM240202", rules = list()),
    "nvel_rules"
  )
})


test_that("carbon fraction and species lookup are size stable", {
  fraction <- carbon_fraction(c(15, 202, 263, 999))
  expect_equal(as.double(fraction), c(0.51, 0.516, 0.507, 0.475))
  expect_match(attr(fraction, "source"), "NVB_CarbonFrac")
  raw_fraction <- .with_treevolume_compat("nvel", carbon_fraction(15))
  expect_equal(as.double(raw_fraction), 0.5097, tolerance = 0)
  expect_match(attr(raw_fraction, "source"), "raw Table S10")
  .gate1_record_values("nsvb", "carbon fraction", "double", as.double(raw_fraction), 0.5097,
    TRUE, 0,
    compat = "nvel"
  )
  expect_warning(unknown <- carbon_fraction(c(202, 10000)), "unknown_species for 1 of 2")
  expect_true(is.na(unknown[[2L]]))

  aliases <- c(204, 2042, 2098, 2242, 2263)
  canonical <- c(202, 42, 98, 242, 263)
  expect_identical(as.double(carbon_fraction(aliases)), as.double(carbon_fraction(
    canonical
  )))
})

test_that("NSVB DOB is useful by default and reproduces CALCDIA under compatibility", {
  inside <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "NVBM240202",
    spcd = .interface_spcd("NVBM240202")
  )$value
  outside <- .oracle_dob(
    dbh = 12, ht = 80, h = 40, model = "NVBM240202",
    spcd = .interface_spcd("NVBM240202")
  )$value
  source <- .with_treevolume_compat("nvel", .oracle_dob(
    dbh = 12, ht = 80, h = 40, model = "NVBM240202",
    spcd = .interface_spcd("NVBM240202")
  ))

  expect_gt(outside, inside)
  expect_identical(source$value, 0)
  expect_identical(source$status, 0L)
})

test_that("county and NVEL district division paths are exact", {
  expect_identical(nsvb_division(c(1, 6, 56), c(1, 93, 45))$value, c(231L, 1261L, 331L))
  expect_true(is.na(nsvb_division(2, 999)$value))
  expect_identical(merchandiser:::.nsvb_ecoprov(6, 12, 1), 1242L)
  expect_identical(merchandiser:::.nsvb_ecoprov(6, 99, 99), 1242L)
})

test_that("NSVB input failures have stable statuses", {
  missing <- .nsvb_native_biomass(NA_real_, 80, 202, 1240)
  expect_true(all(is.na(missing[1, 1:30])))
  expect_true(all(as.integer(missing[1, 31:60]) == 1L))

  small <- .nsvb_native_biomass(0.5, 80, 202, 1240)
  expect_true(all(is.na(small[1, 1:30])))
  expect_true(all(as.integer(small[1, 31:60]) == 303L))

  unknown <- .nsvb_native_biomass(12, 80, 10000, 1240)
  expect_true(all(is.na(unknown[1, 1:30])))
  expect_true(all(as.integer(unknown[1, 31:60]) == 7L))

  remapped <- .nsvb_native_biomass(12, 80, 2042, 1240)
  expect_true(all(as.integer(remapped[1, 31:60]) == 0L))
  expect_true(all(is.finite(as.double(remapped[1, 1:30]))))
})

test_that("NSVB taper dispatches through the registry", {
  id <- "NVBM240202"
  at_height <-
    .oracle_dib(dbh = 12, ht = 80, h = 30, model = id, spcd = .interface_spcd(id))$value
  outside <-
    .oracle_dob(dbh = 12, ht = 80, h = 30, model = id, spcd = .interface_spcd(id))$value
  expect_gt(outside, at_height)
  expect_equal(
    .oracle_height_at_dib(
      dbh = 12, ht = 80, dib = at_height, model = id,
      spcd = .interface_spcd(id)
    )$value,
    30,
    tolerance = 1e-04
  )
  expect_equal(
    .oracle_height_at_dob(
      dbh = 12, ht = 80, dob = outside, model = id,
      spcd = .interface_spcd(id)
    )$value,
    30,
    tolerance = 1e-04
  )
  expect_true(is.finite(.oracle_stem_volume(
    dbh = 12, ht = 80, model = id,
    spcd = .interface_spcd(id)
  )$value))

  profile <- stem_profile(
    dbh = 12,
    ht = 40,
    model = id,
    step = 5,
    spcd = .interface_spcd(id),
    tree_id = seq_along(rep_len(12, max(length(12), length(40))))
  )
  expect_true(nrow(profile) > 2L)
  expect_true(all(profile$status == 0L))
})
