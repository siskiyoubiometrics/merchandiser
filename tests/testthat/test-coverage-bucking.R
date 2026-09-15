test_that("coverage cubic products integrate the closed-form paraboloid", {
  id <- coverage_paraboloid()
  on.exit(unregister_taper_model(id), add = TRUE)
  p <- products(coverage_product(product = "butt", max_logs = 1, trim = 0.5),
                coverage_product(product = "upper", max_length = 8))
  x <- merchandise(1, 24, 80, 202, p, model = id)
  expect_setequal(x$logs$product, p$product)
  lo <- x$logs$start_height
  hi <- lo + x$logs$length
  limits <- p[match(x$logs$product, p$product), ]
  expect_true(length(lo) == nrow(x$logs) && length(hi) == nrow(x$logs) &&
                all(is.finite(lo) & is.finite(hi) & lo >= 1 & hi <= 80 &
                      x$logs$length >= limits$min_length & x$logs$length <= limits$max_length))
  # Source: integrate A(h) = pi * (0.9 * dbh / 24)^2 * (ht - h)/(ht - 4.5).
  # Independently evaluate the antiderivative at both ends, without the callback.
  coefficient <- pi * (0.9 * 24 / 24)^2 / (80 - 4.5)
  expected <- coefficient * ((80 * hi - hi^2 / 2) - (80 * lo - lo^2 / 2))
  expect_equal(round(x$logs$scale, 8), round(expected, 8), tolerance = 0)
})

for (unit in c("scribner", "doyle", "green_ton", "cord")) {
  test_that(paste("coverage optimize prices every", unit, "log"), {
    p <- coverage_product(volume_unit = unit, min_sed = 4, price = 75, price_per = 10,
                          cord_solid_fraction = 0.7)
    x <- merchandise(1, 24, 80, 202, p, model = "F00FW2W202", strategy = "optimize")
    expect_gt(nrow(x$logs), 0)
    expect_equal(nrow(x$status), 0)
    expect_true(all(is.finite(x$logs$scale) & x$logs$scale > 0))
    # Source: price is quoted per price_per units, so value = scale * price / price_per.
    expect_equal(x$logs$value, x$logs$scale * p$price / p$price_per)
  })
}

test_that("coverage Doyle optimization attains an exhaustive pattern maximum", {
  p <- coverage_product(min_length = 8, max_length = 9, volume_unit = "doyle",
                        price = 75, price_per = 10)
  dbh <- 24
  ht <- 28
  stump <- 1
  heights <- seq(stump, ht, by = 0.5)
  diameters <- dib(dbh, ht, heights, 202, model = "F00FW2W202")$value
  # Source: enumerate every cut sequence from the stump on the half foot grid.
  # Each node may stop or append any fitting length. No helper-optimize.R code
  # is used. Doyle is max(d - 4, 0)^2 * floor(L) / 16 in board feet.
  walk <- function(start) {
    patterns <- list(data.frame(start = numeric(), length = numeric(), value = numeric()))
    for (len in seq(p$min_length, p$max_length, by = 0.5)) {
      end <- start + len
      if (end > ht) next
      d <- diameters[match(end, heights)]
      value <- max(d - 4, 0)^2 * floor(len) / 16 * p$price / p$price_per
      for (tail in walk(end)) {
        patterns[[length(patterns) + 1L]] <- rbind(
          data.frame(start = start, length = len, value = value), tail
        )
      }
    }
    patterns
  }
  patterns <- walk(stump)
  totals <- vapply(patterns, function(x) sum(x$value), numeric(1))
  x <- merchandise(1, dbh, ht, 202, p, model = "F00FW2W202", strategy = "optimize")
  expect_equal(nrow(x$status), 0)
  expect_gt(nrow(x$logs), 0)
  chosen <- vapply(patterns, function(pattern) {
    identical(pattern$start, x$logs$start_height) && identical(pattern$length, x$logs$length)
  }, logical(1))
  expect_true(any(chosen))
  expect_equal(sum(x$logs$value), max(totals), tolerance = 1e-12)
  expect_equal(totals[chosen], max(totals), tolerance = 1e-12)
})

test_that("coverage mixed pricing leaves exactly the unpriced rows missing", {
  p <- products(coverage_product(product = "priced", max_logs = 1, price = 75),
                coverage_product(product = "unpriced"))
  x <- merchandise(1, 24, 80, 202, p, model = "F00FW2W202")
  expect_setequal(x$logs$product, p$product)
  expect_identical(is.na(x$logs$value), is.na(p$price[match(x$logs$product, p$product)]))
})

test_that("coverage identical trees expand the single-tree cuts", {
  id <- coverage_paraboloid()
  on.exit(unregister_taper_model(id), add = TRUE)
  p <- coverage_product()
  one <- merchandise(1, 24, 80, 202, p, model = id)
  many <- merchandise(seq_len(1000), 24, 80, 202, p, model = id)
  # Sources: cascade takes four 16-foot logs from stump 1, then the remaining
  # 15 feet to height 80. No trim or diameter restriction shortens those logs.
  # Diameters and volumes follow the analytic paraboloid, not merchandise output.
  lo <- 1 + 16 * (0:4)
  hi <- pmin(lo + 16, 80)
  expected_one <- data.frame(
    log = seq_along(lo), product = "saw", start_height = lo, end_height = hi,
    length = hi - lo, scaling_length = hi - lo,
    sed = 0.9 * 24 * sqrt((80 - hi) / (80 - 4.5)),
    led = 0.9 * 24 * sqrt((80 - lo) / (80 - 4.5)),
    scaling_diameter = NA_real_, inside_bark = TRUE,
    scale = pi * (0.9 * 24 / 24)^2 / (80 - 4.5) *
      (hi - lo) * (80 - (hi + lo) / 2), volume_unit = "cubic"
  )
  expect_equal(one$logs[names(expected_one)], expected_one, tolerance = 1e-12)
  # Source: 1000 repetitions of the independently derived single-tree table.
  expected <- expected_one[rep(seq_len(nrow(expected_one)), 1000), ]
  actual <- many$logs
  actual$tree_id <- NULL
  rownames(expected) <- NULL
  expect_equal(actual, expected, tolerance = 1e-12)
  expect_identical(many$logs$tree_id, rep(seq_len(1000), each = nrow(expected_one)))
})

test_that("coverage a tiny positive cull preserves exact-height closure", {
  records <- defect(1, 20, 20 + 1e-10, "cull")
  p <- coverage_product()
  # Source: a positive interval is valid under validate_defects, status 0.
  expect_identical(validate_defects(records, 1, 80, p)$status, 0L)
  x <- merchandise(1, 24, 80, 202, p, model = "F00FW2W202", defects = records)
  cull <- x$residuals[x$residuals$cause == "cull", ]
  expect_equal(nrow(cull), nrow(records))
  expect_true(all(cull$end_height > cull$start_height))
  # Source: cutting restarts at the cull's upper boundary, within 1e-9 of 20 feet.
  restart <- min(x$logs$start_height[x$logs$start_height >= 20])
  expect_equal(restart, 20, tolerance = 1e-9)
  expect_equal(nrow(x$status), 0)
})
