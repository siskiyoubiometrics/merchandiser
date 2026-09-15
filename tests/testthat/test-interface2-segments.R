test_that("trim is an unscaled piece after the nominal body", {
  for (trim in c(0, 0.5)) {
    p <- product("saw",
      min_length = 32, max_length = 32, trim = trim, min_sed = 0, max_logs = 1,
      volume_unit = "cubic"
    )
    x <- merchandise("tree", 24, 120, 202, p, model = "F00FW2W202", quiet = TRUE)
    expect_identical(x$logs$start_height, 1)
    expect_identical(x$logs$end_height, 33 + trim)
    expect_identical(x$logs$scaling_length, 32)
    expect_identical(x$logs$length, 32)
    expected <- stem_volume(
      24,
      120,
      202,
      model = "F00FW2W202",
      from = 1,
      to = 33
    )
    expect_equal(x$logs$scale, expected$value, tolerance = 1e-12)
    residual <- x$residuals[x$residuals$cause == "trim", ]
    expect_identical(nrow(residual), if (trim == 0)
                       0L else 1L)
    if (trim > 0) {
      expect_identical(residual$start_height, 33)
      expect_identical(residual$end_height, 33.5)
    }
  }
})

test_that("a short butt restriction restarts saw products at its upper boundary", {
  p <- products(
    product("saw", min_length = 32, max_length = 32, min_sed = 8, volume_unit = "cubic"),
    product("pulp", min_length = 16, max_length = 16, min_sed = 3, volume_unit = "cubic")
  )
  x <- merchandise("tree", 24, 120, 202, p, model = "F00FW2W202", defects = defect("tree",
                     1, 8, "restrict",
                     product = "pulp"
                   ), quiet = TRUE)
  expect_identical(x$logs$product[1], "saw")
  expect_identical(x$logs$start_height[1], 8)
  butt <- x$residuals[x$residuals$cause == "restricted", ]
  expect_identical(butt$start_height, 1)
  expect_identical(butt$end_height, 8)
  expect_false(any(x$logs$start_height < 8))
})

test_that("cull and restriction boundaries restart per-segment log counts", {
  p <- products(product("saw",
                  min_length = 16, max_length = 16, min_sed = 0, max_logs = 1,
                  volume_unit = "cubic"
                ), product("pulp",
                  min_length = 8, max_length = 8, min_sed = 0,
                  volume_unit = "cubic"
                ))
  for (effect in c("cull", "restrict")) {
    d <- defect(1, 25, 33, effect, product = if (effect == "restrict")
                  "pulp" else NULL)
    x <- merchandise(1, 24, 120, 202, p, model = "F00FW2W202", defects = d, quiet = TRUE)
    saw <- x$logs[x$logs$product == "saw", ]
    expect_identical(saw$start_height, c(1, 33))
    expect_false(any(x$logs$start_height < 25 & x$logs$end_height > 25))
    expect_false(any(x$logs$start_height < 33 & x$logs$end_height > 33))
  }
})

test_that("pruning is a height and sweep is a percentage", {
  p <- products(product("clear",
                  requires_pruned = TRUE, min_length = 16, max_length = 16,
                  min_sed = 0, max_sweep = 10, volume_unit = "cubic"
                ), product("pulp",
                  min_length = 8,
                  max_length = 16, min_sed = 0, volume_unit = "cubic"
                ))
  x <- merchandise(1, 24, 120, 202, p, model = "F00FW2W202", pruned_ht = 33, quiet = TRUE)
  expect_true(all(x$logs$end_height[x$logs$product == "clear"] <= 33))
  x <- merchandise(1, 24, 120, 202, p,
    model = "F00FW2W202", pruned_ht = 33,
    defects = defect(1,
      1, 33, "sweep",
      percent = 20
    ), quiet = TRUE
  )
  expect_false(any(x$logs$product == "clear"))
})

test_that("optimized runs require a price for every product", {
  p <- product("saw", min_length = 16, max_length = 32, min_sed = 0, volume_unit = "cubic")
  expect_error(merchandise(1, 24, 120, 202, p, strategy = "optimize"), "saw.*price")
  p$price <- 1
  x <- merchandise(1, 24, 120, 202, p, strategy = "optimize", quiet = TRUE)
  expect_identical(names(x), c("logs", "residuals", "status", "assumptions", "call"))
  expect_true(all(x$logs$value >= 0))
})

test_that("value optimization fills a short stem and preserves threaded results", {
  specification <- product(
    product = "wood", min_length = 8, max_length = 10,
    min_sed = 0, volume_unit = "cubic", price = 1
  )
  run <- function(strategy, workers) {
    with_threads(workers, merchandise(
      tree_id = 1, dbh = 12, ht = 25, spcd = 202,
      products = specification, model = "demo.paraboloid",
      strategy = strategy
    ))
  }
  cascade <- run("cascade", 1)
  optimal <- run("optimize", 1)
  expect_identical(optimal, run("optimize", 4))
  expect_identical(cascade$logs$length, c(10, 10))
  expect_identical(optimal$logs$length, c(8, 8, 8))
  expect_gt(sum(optimal$logs$value), sum(cascade$logs$value))
  expected <- stem_volume(
    12,
    25,
    202,
    model = "demo.paraboloid",
    from = 1,
    to = 25
  )$value
  expect_equal(sum(optimal$logs$scale), expected, tolerance = 1e-13)
})

test_that("an end below the stump leaves disjoint context pieces", {
  specification <- product(
    product = "wood", min_length = 8, max_length = 16,
    min_sed = 0, volume_unit = "cubic"
  )
  result <- merchandise(
    tree_id = 1, dbh = 12, ht = 80, spcd = 202,
    products = specification, model = "demo.paraboloid",
    defects = defect(1, 0.5, NA_real_, "end")
  )
  expect_equal(nrow(result$logs), 0)
  expect_identical(result$residuals$start_height, c(0, 0.5))
  expect_identical(result$residuals$end_height, c(0.5, 80))
  expect_identical(result$residuals$cause, c("stump", "end"))
})
