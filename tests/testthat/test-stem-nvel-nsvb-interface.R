test_that("NSVB equations and dynamic patterns resolve", {
  metadata <- utils::read.csv(
    system.file("extdata", "nsvb_models.csv", package = "merchandiser"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(metadata), 45L)
  expect_true(all(has_taper_model(metadata$id)))
  expect_true(has_taper_model("NVBM261202"))
  expect_identical(get_taper_model("NVBM261202")$family, "nsvb")
  expect_true(has_taper_model("NVB0210110P"))
  expect_identical(get_taper_model("NVB0210110P")$species, 110L)
})

test_that("biomass public shape and component accessor are frozen", {
  result <- biomass(
    c(12, 16), c(80, 100), c(202, 263), c(1240, 1330),
    id = c("a", "b"), status = TRUE
  )
  expect_equal(nrow(result), 2L)
  expect_identical(result$id, c("a", "b"))
  expect_equal(sum(grepl("_status$", names(result))), 30L)
  expect_identical(
    names(result)[2:31],
    c(
      paste0("dry_", c(
        "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood",
        "stump_bark", "saw_wood", "saw_bark", "topwood_wood",
        "topwood_bark", "tip_wood", "tip_bark", "branches", "foliage",
        "top_and_limb"
      )),
      "carbon", "co2e",
      paste0("green_", c(
        "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood",
        "stump_bark", "saw_wood", "saw_bark", "topwood_wood",
        "topwood_bark", "tip_wood", "tip_bark", "branches", "foliage",
        "top_and_limb"
      ))
    )
  )
  component <- biomass_component(
    c(12, 16), c(80, 100), c(202, 263), c(1240, 1330),
    "dry_stem_wood", status = TRUE
  )
  expect_identical(names(component), c("value", "status"))
  expect_equal(component$value, result$dry_stem_wood)
  expect_identical(component$status, c(0L, 0L))
})

test_that("internal NVEL volume record and rules are stable", {
  rules <- merchandiser:::nvel_rules(
    primary_top = 6, secondary_top = 4, stump = 1
  )
  result <- merchandiser:::nvel_volume(
    12, 80, "NVBM240202", rules = rules, status = TRUE
  )
  expect_identical(
    names(result)[seq_len(18L)],
    c(
      merchandiser:::.nvel_volume_names,
      "n_logs_primary", "n_logs_secondary", "errflag"
    )
  )
  expect_identical(result$errflag, 0L)
  expect_gte(result$n_logs_primary, 0L)
  expect_gte(result$n_logs_secondary, 0L)

  metric <- merchandiser:::nvel_volume(
    12 * 2.54, 80 * 0.3048, "NVBM240202",
    rules = merchandiser:::nvel_rules(
      primary_top = 6 * 2.54, secondary_top = 4 * 2.54,
      stump = 0.3048
    ), units = "metric", status = TRUE
  )
  cubic <- c(1L, 4L, 5L, 7L, 8L, 14L, 15L)
  expect_equal(
    as.numeric(metric[1, cubic]), as.numeric(result[1, cubic]) * 0.028316846592,
    tolerance = 1e-14
  )
  expect_identical(metric$vol_bf_gross, result$vol_bf_gross)

  expect_identical(
    merchandiser:::nvel_volume(
      0.5, 80, "NVBM240202", rules = rules, status = TRUE
    )$errflag,
    3L
  )
  expect_identical(
    merchandiser:::nvel_volume(
      12, 80, "NVBM240202", rules = rules, spcd = 10000,
      status = TRUE
    )$errflag,
    6L
  )

  expect_error(merchandiser:::nvel_rules(option = 99), "segmentation")
  expect_error(merchandiser:::nvel_volume(
    12, 80, "NVBM240202", rules = list()
  ), "nvel_rules")
})

test_that("biomass metric conversion and co2e units are explicit", {
  imperial <- biomass(12, 80, 202, 1240)
  metric <- biomass(
    12 * 2.54, 80 * 0.3048, 202, 1240, units = "metric"
  )
  expect_equal(
    unname(as.matrix(metric)),
    unname(as.matrix(imperial)) * 0.45359237,
    tolerance = 1e-11
  )
  expected_tonnes <- imperial$carbon * 0.45359237 / 1000 * 44 / 12
  expect_equal(co2e(12, 80, 202, 1240), expected_tonnes, tolerance = 1e-14)
  expect_equal(
    co2e(12 * 2.54, 80 * 0.3048, 202, 1240, units = "metric"),
    expected_tonnes, tolerance = 1e-14
  )
})

test_that("carbon fraction and species lookup are size stable", {
  fraction <- carbon_fraction(c(15, 202, 263, 999))
  expect_equal(as.double(fraction), c(0.510, 0.516, 0.507, 0.475))
  expect_match(attr(fraction, "source"), "NVB_CarbonFrac")
  raw_fraction <- .with_treevolume_compat("nvel", carbon_fraction(15))
  expect_equal(as.double(raw_fraction), 0.5097, tolerance = 0)
  expect_match(attr(raw_fraction, "source"), "raw Table S10")
  .gate1_record_values(
    "nsvb", "carbon fraction", "double",
    as.double(raw_fraction), 0.5097, TRUE, 0, compat = "nvel"
  )
  expect_warning(
    unknown <- carbon_fraction(c(202, 10000)),
    "unknown_species for 1 of 2"
  )
  expect_true(is.na(unknown[[2L]]))

  records <- species_lookup(c(202, 999, 10000))
  expect_identical(records$spcd[1:2], c(202L, 999L))
  expect_true(all(is.na(records[3L, ])))
  expect_equal(
    species_lookup(c(202, 999), to = "wood_density"), c(28.08, 32.45)
  )

  aliases <- c(204, 2042, 2098, 2242, 2263)
  canonical <- c(202, 42, 98, 242, 263)
  expect_identical(
    as.double(carbon_fraction(aliases)),
    as.double(carbon_fraction(canonical))
  )
})

test_that("NSVB DOB is useful by default and reproduces CALCDIA under compatibility", {
  inside <- dib(12, 80, 40, "NVBM240202")
  outside <- dob(12, 80, 40, "NVBM240202")
  source <- .with_treevolume_compat(
    "nvel", dob(12, 80, 40, "NVBM240202", status = TRUE)
  )

  expect_gt(outside, inside)
  expect_identical(source$value, 0)
  expect_identical(source$status, 0L)
})

test_that("county and NVEL district division paths are exact", {
  expect_identical(nsvb_division(c(1, 6, 56), c(1, 93, 45)), c(231L, 1261L, 331L))
  expect_true(is.na(nsvb_division(2, 999)))
  expect_identical(merchandiser:::.nsvb_ecoprov(6, 12, 1), 1242L)
  expect_identical(merchandiser:::.nsvb_ecoprov(6, 99, 99), 1242L)
})

test_that("NSVB input failures have stable statuses", {
  missing <- biomass(NA_real_, 80, 202, 1240, status = TRUE)
  expect_true(all(is.na(missing[1, 1:30])))
  expect_true(all(as.integer(missing[1, 31:60]) == 1L))

  small <- biomass(0.5, 80, 202, 1240, status = TRUE)
  expect_true(all(is.na(small[1, 1:30])))
  expect_true(all(as.integer(small[1, 31:60]) == 303L))

  unknown <- biomass(12, 80, 10000, 1240, status = TRUE)
  expect_true(all(is.na(unknown[1, 1:30])))
  expect_true(all(as.integer(unknown[1, 31:60]) == 7L))

  remapped <- biomass(12, 80, 2042, 1240, status = TRUE)
  expect_true(all(as.integer(remapped[1, 31:60]) == 0L))
  expect_true(all(is.finite(as.double(remapped[1, 1:30]))))
})

test_that("NSVB taper dispatches through the registry", {
  id <- "NVBM240202"
  at_height <- dib(12, 80, 30, id)
  outside <- dob(12, 80, 30, id)
  expect_gt(outside, at_height)
  expect_equal(height_at_dib(12, 80, at_height, id), 30, tolerance = 1e-4)
  expect_equal(height_at_dob(12, 80, outside, id), 30, tolerance = 1e-4)
  expect_true(is.finite(stem_volume(12, 80, id)))

  profile <- stem_profile(12, 40, id, step = 60)
  expect_true(nrow(profile) > 2L)
  expect_true(all(profile$status == 0L))
})
