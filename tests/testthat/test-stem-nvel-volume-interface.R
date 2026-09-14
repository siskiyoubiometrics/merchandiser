test_that("internal NVEL volume record has frozen shape and statuses", {
  result <- merchandiser:::nvel_volume(
    c(12, 12), c(80, 80), c("300FW2W202", "900DVEE122"),
    id = c("profile", "direct"), status = TRUE
  )
  expect_identical(result$id, c("profile", "direct"))
  expect_identical(
    names(result)[2:19],
    c(merchandiser:::.nvel_volume_names, "n_logs_primary",
      "n_logs_secondary", "errflag")
  )
  expect_identical(result$errflag, c(0L, 50L))
  expect_true(is.finite(result$vol_total_cu[[1L]]))
  expect_true(is.finite(result$vol_stump_cu[[1L]]))
  expect_true(all(is.na(result[1L, merchandiser:::.nvel_volume_names[2:9]])))
  expect_identical(result$vol_bf_gross_status, c(53L, 50L))
  expect_identical(result$vol_total_cu_status, c(0L, 50L))
})

test_that("merchant rules validate NVEL domains", {
  rules <- nvel_rules()
  expect_s3_class(rules, "treevolume_nvel_rules")
  expect_named(rules, c(
    "even_or_odd", "option", "maximum_length", "minimum_length",
    "minimum_top_length", "merchantable_length", "primary_top",
    "secondary_top", "stump", "trim", "bark_ratio",
    "minimum_board_foot_dbh", "scribner", "prod", "ht_type", "live",
    "ctype", "cull", "forest", "district"
  ))
  expect_error(nvel_rules(even_or_odd = 3), "1, 2")
  expect_error(nvel_rules(cull = 101), "zero and 100")
  expect_error(nvel_rules(stump = -1), "nonnegative")
  expect_error(nvel_rules(prod = "1"), "two-digit")
  expect_error(nvel_rules(ht_type = "X"), "empty, F, or L")
  expect_error(nvel_rules(forest = "01"), "numeric")
})

test_that("regional merchant defaults and cull routing follow NVEL", {
  defaults <- merchandiser:::.nvel_reference("nvel_merchant_defaults.csv")
  expect_identical(nrow(defaults), 11L)
  expect_false(anyNA(defaults$source_line_start))
  expect_identical(defaults$scribner[defaults$region == 7L], "factor")
  rules <- merchandiser:::nvel_rules()
  values <- lapply(rules, rep, length.out = 3L)
  resolved <- merchandiser:::.nvel_rule_defaults(
    values, c("B00BEHW202", "811CLKE100", "900CLKE400"),
    c(10, 12, 12), c(202L, 100L, 400L), c(7L, 8L, 9L), "imperial"
  )
  expect_equal(resolved$primary_top, c(4, 7, 9.6))
  expect_equal(resolved$stump, c(1, 1, 1))

  region1_rules <- merchandiser:::nvel_rules(
    prod = c("01", "08", "08", "01"),
    primary_top = c(NA, NA, NA, 4),
    secondary_top = c(NA, NA, NA, 6)
  )
  region1_values <- lapply(region1_rules, rep, length.out = 4L)
  region1 <- merchandiser:::.nvel_rule_defaults(
    region1_values,
    c("100JB2W202", "100JB2W202", "300FW2W202", "100JB2W202"),
    rep(12, 4), rep(202L, 4), rep(1L, 4), "imperial"
  )
  expect_equal(region1$option, c(12, 12, 22, 12))
  expect_equal(region1$maximum_length, c(20, 20, 16, 20))
  expect_equal(region1$minimum_length, c(10, 10, 16, 10))
  expect_equal(region1$minimum_top_length, c(2, 2, 16, 2))
  expect_equal(region1$merchantable_length, c(10, 10, 16, 10))
  expect_equal(region1$secondary_top, c(4, 4, 4, 4))

  nsvb <- "NVBM240202"
  sound <- merchandiser:::nvel_volume(
    12, 80, nsvb, rules = merchandiser:::nvel_rules(cull = 0), status = TRUE
  )
  culled <- merchandiser:::nvel_volume(
    12, 80, nsvb, rules = merchandiser:::nvel_rules(cull = 10), status = TRUE
  )
  expect_equal(
    as.numeric(culled[c(1L, 14L, 15L)]),
    as.numeric(sound[c(1L, 14L, 15L)]) * 0.9,
    tolerance = 1e-12
  )

  profile <- "300FW2W202"
  unculled_profile <- merchandiser:::nvel_volume(
    12, 80, profile, rules = merchandiser:::nvel_rules(cull = 0), status = TRUE
  )
  culled_profile <- merchandiser:::nvel_volume(
    12, 80, profile, rules = merchandiser:::nvel_rules(cull = 10), status = TRUE
  )
  expect_equal(
    culled_profile[c(1L, 14L)], unculled_profile[c(1L, 14L)], tolerance = 0
  )
})
