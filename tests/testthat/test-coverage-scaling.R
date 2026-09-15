for (rule in c("scribner", "international", "doyle")) {
  for (rounding in c("default", "down", "nearest", "none")) {
    test_that(paste("coverage scaling", rule, rounding, "uses the reported diameter"), {
      id <- coverage_paraboloid()
      on.exit(unregister_taper_model(id), add = TRUE)
      p <- coverage_product(min_length = 16, max_logs = 1, volume_unit = rule,
                            round = rounding)
      x <- merchandise(1, 24, 80, 202, p, model = id)
      # Source: the closed-form paraboloid at stump + nominal length.
      raw <- 0.9 * 24 * sqrt((80 - (1 + 16)) / (80 - 4.5))
      expect_gt(raw - floor(raw), 0.55)
      expect_lt(raw - floor(raw), 0.95)
      reported <- switch(rounding, down = floor(raw), nearest = floor(raw + 0.5), raw)
      # The engine rounds the small end half up to a whole inch before Scribner
      # and International regardless of product round. Thus none is a no-op for
      # those rules. Doyle uses the reported diameter as is.
      if (rule != "doyle") reported <- floor(reported + 0.5)
      expect_equal(x$logs$scaling_diameter, reported, tolerance = 1e-12)
      d <- x$logs$scaling_diameter
      # Sources: requested R rule references in R/scaling.R. Corrected Scribner
      # is board feet, Decimal C times ten. Doyle is (d - 4)^2 * L / 16.
      expected <- switch(rule,
        scribner = merchandiser:::.mc_scribner(d, 16, corrected = TRUE),
        international = merchandiser:::.mc_intl14(d, 16),
        doyle = (d - 4)^2 * 16 / 16
      )
      expect_equal(x$logs$scale, expected, tolerance = 1e-12)
    })
  }
}

test_that("coverage split Scribner sums independently scaled small ends", {
  p <- coverage_product(min_length = 40, max_length = 40, max_logs = 1,
                        volume_unit = "scribner")
  whole <- merchandise(1, 30, 140, 202, p, model = "F00FW2W202")
  p$split_scale <- TRUE
  split <- merchandise(1, 30, 140, 202, p, model = "F00FW2W202")
  # Source: dib at the two segment small ends, half-up rounding, then the
  # requested corrected Scribner reference. No published table is transcribed.
  d <- floor(dib(30, 140, c(21, 41), 202, model = "F00FW2W202")$value + 0.5)
  segments <- vapply(d, function(x) {
    merchandiser:::.mc_scribner(x, 20, corrected = TRUE)
  }, numeric(1))
  expected_whole <- merchandiser:::.mc_scribner(d[2], 40, corrected = TRUE)
  # Derived rule results also identified in coverage review 2026-09-14:
  # segment diameters 24 and 21 give 500 + 380 = 880, versus 760 whole.
  expect_equal(d, c(24, 21))
  expect_equal(segments, c(500, 380))
  expect_equal(expected_whole, 760)
  expect_equal(sum(segments), 880)
  expect_equal(whole$logs$scale, expected_whole)
  expect_equal(split$logs$scale, sum(segments))
  expect_gt(split$logs$scale, whole$logs$scale)
})
