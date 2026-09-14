test_that("old Region 8 bounded volume follows R8CUBIC geometry", {
  arguments <- list(
    dbh = 12, ht = 80, model = "814CLKE100", upper_ht1 = 60
  )
  analytic <- do.call(stem_volume, c(arguments, list(
    lower = 1, lower_type = "height", upper = 40, upper_type = "height",
    status = TRUE
  )))
  quadrature <- stats::integrate(
    function(height) {
      diameter <- do.call(dib, c(arguments, list(h = height)))
      pi * diameter^2 / 576
    },
    lower = 1, upper = 40, subdivisions = 2000L, rel.tol = 1e-10
  )$value

  expect_identical(analytic$status, 0L)
  expect_equal(analytic$value, 18.172835714569931, tolerance = 1e-12)
  expect_equal(analytic$value, quadrature, tolerance = 5e-7)
})

test_that("Clark public volume honors inside bark for E, O, and 0 variants", {
  ids <- c("811CLKE100", "811CLKO100", "811CLK0100")
  inside <- stem_volume(12, 80, ids, bark = "inside", status = TRUE)
  outside <- stem_volume(
    12, 80, ids, bark = "outside", bark_ratio = .9, status = TRUE
  )

  expect_identical(inside$status, integer(3L))
  expect_equal(inside$value, rep(24.055372281704294, 3L), tolerance = 1e-12)
  expect_equal(outside$value, inside$value / .9^2, tolerance = 1e-12)
})

test_that("Clark exact default bounds are geometric only in the public API", {
  exact <- stem_volume(12, 80, "900CLKE001")
  adjacent <- stem_volume(
    12, 80, "900CLKE001", lower = 1 + 1e-11,
    lower_type = "height"
  )
  record <- merchandiser:::nvel_volume(
    12, 80, "900CLKE001", status = TRUE
  )

  expect_equal(exact, adjacent, tolerance = 1e-9)
  expect_equal(exact, 24.749856128758822, tolerance = 1e-12)
  expect_equal(record$vol_total_cu, 25.7, tolerance = 1e-14)
  expect_identical(record$vol_total_cu_status, 0L)

  unscaled <- stem_volume(12, 80, "900CLKE621")
  unscaled_record <- merchandiser:::nvel_volume(
    12, 80, "900CLKE621", status = TRUE
  )
  expect_equal(
    unscaled_record$vol_total_cu, round(unscaled * 10) / 10, tolerance = 1e-14
  )
})

test_that("Clark top-code-1 NVEL stump volume uses unscaled R9CUFT", {
  record <- merchandiser:::nvel_volume(
    2, 15, "811CLKE100", status = TRUE
  )
  expect_equal(record$vol_total_cu, .4, tolerance = 1e-15)
  expect_equal(record$vol_stump_cu, .14359763038997606, tolerance = 1e-15)
  expect_identical(record$vol_stump_cu_status, 0L)

  variants <- merchandiser:::nvel_volume(
    12, 80, c("811CLKE100", "811CLKO100", "811CLK0100"), status = TRUE
  )
  expect_equal(variants$vol_total_cu, c(24.1, 30.1, 30.1), tolerance = 1e-14)
  expect_equal(
    variants$vol_stump_cu, rep(.86969333096644519, 3L), tolerance = 1e-14
  )

  fallback <- merchandiser:::nvel_volume(
    c(2, 2), c(30, 130), c("811CLKE330", "811CLKE370"), status = TRUE
  )
  expect_equal(
    fallback$vol_stump_cu,
    c(.034911375375197742, .015957106155581297), tolerance = 1e-15
  )
  expect_identical(fallback$vol_stump_cu_status, integer(2L))
})

test_that("Region 4 public ground intervals are additive", {
  arguments <- list(dbh = 12, ht = 80, model = "400MATW202")
  ground_tip <- do.call(stem_volume, c(arguments, list(
    lower = 0, lower_type = "height", upper_type = "tip"
  )))
  ground_one <- do.call(stem_volume, c(arguments, list(
    lower = 0, lower_type = "height", upper = 1, upper_type = "height"
  )))
  one_tip <- do.call(stem_volume, c(arguments, list(
    lower = 1, lower_type = "height", upper_type = "tip"
  )))
  record <- do.call(merchandiser:::nvel_volume, c(arguments, list(status = TRUE)))

  expect_equal(ground_tip, ground_one + one_tip, tolerance = 1e-12)
  expect_equal(ground_tip, 25.601762826985762, tolerance = 1e-12)
  expect_equal(record$vol_total_cu, 24.959558507873304, tolerance = 1e-12)
  expect_identical(record$vol_total_cu_status, 0L)

  short_ground_tip <- stem_volume(
    12, 5.5, "400MATW202", lower = 0, lower_type = "height"
  )
  short_ground_one <- stem_volume(
    12, 5.5, "400MATW202", lower = 0, lower_type = "height",
    upper = 1, upper_type = "height"
  )
  short_one_tip <- stem_volume(
    12, 5.5, "400MATW202", lower = 1, lower_type = "height"
  )
  short_record <- merchandiser:::nvel_volume(
    12, 5.5, "400MATW202", status = TRUE
  )
  expect_equal(
    short_ground_tip, short_ground_one + short_one_tip, tolerance = 1e-12
  )
  expect_equal(
    short_record$vol_total_cu, 12^2 * 5.5 * .00272708, tolerance = 1e-14
  )
})

test_that("Gate 1 dynamic patterns retain their family capabilities", {
  clark_models <- .clark_models()
  r10r4_models <- .r10r4_models()
  patterns <- rbind(
    .clark_patterns(),
    .flewelling_patterns(),
    .smalltaper_patterns(),
    .r10r4_patterns()
  )
  models <- list(
    .clark_pattern_model("811CLKE100"),
    .flewelling_pattern_model("F00FW3W202"),
    .smalltaper_pattern_model("B04BEHW202"),
    .smalltaper_pattern_model("616BEHW202"),
    .r10r4_pattern_model("400MATW202")
  )

  expect_identical(
    vapply(models, `[[`, character(1), "family"),
    c("clark_r9", "flewelling_3pt", "blm_taper", "behre_taper", "r4_driver")
  )
  expect_true(any(vapply(clark_models, function(model) {
    identical(model$id, "900CLKE001")
  }, logical(1))))
  expect_true(any(vapply(r10r4_models, function(model) {
    identical(model$id, "400MATW202")
  }, logical(1))))
  expect_true(all(c(
    "clark_r8", "clark_r9", "flewelling_3pt", "blm_taper", "behre_taper",
    "r4_driver"
  ) %in% patterns$family))
  expect_true(all(vapply(models, function(model) {
    isTRUE(model$kernel$has_integral)
  }, logical(1))))
})
