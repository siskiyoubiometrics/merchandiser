test_that("decimal segment boundaries retain the longest fitting log", {
  saw <- product("saw", min_length = 16, max_length = 16, min_sed = 6,
    volume_unit = "scribner", price = 500, price_per = 1000
  )
  pulp <- product("pulp", min_length = 8, max_length = 8, min_sed = 3,
    volume_unit = "cubic", price = 1
  )
  for (strategy in c("cascade", "optimize")) {
    for (butt in c(1.4, 1.9, 2.4, 5.7)) {
      records <- defects_from_stoppers(1, 60, "pulp", saw_stop = butt + 16, jump_butt = butt)
      result <- merchandise(1, 16, 60, 202, products(saw, pulp), model = "F00FW2W202",
        defects = records, strategy = strategy
      )
      expect_identical(result$logs$product[1], "saw")
      expect_equal(result$logs$start_height[1], butt, tolerance = 1e-12)
      expect_equal(result$logs$end_height[1], butt + 16, tolerance = 1e-12)
    }
    result <- merchandise(1, 16, 60, 202, products(saw, pulp), model = "F00FW2W202",
      stump_ht = 0.4, defects = defect(1, 16.4, 20, "cull"), strategy = strategy
    )
    expect_identical(result$logs$product[1], "saw")
    expect_equal(result$logs$end_height[1], 16.4, tolerance = 1e-12)
  }
})

test_that("zero prices are rejected for optimization with the product name", {
  p <- hardening_product()
  p$price <- 0
  expect_error(merchandise(1, 16, 60, 202, p, strategy = "optimize"), "saw.*positive price")
  expect_gt(nrow(merchandise(1, 16, 60, 202, p, quiet = TRUE)$logs), 0)
})

test_that("cascade tops use the smallest diameter regardless of product order", {
  saw <- hardening_product()
  pulp <- product("pulp", min_length = 8, max_length = 8, min_sed = 3,
    volume_unit = "cubic", price = 1
  )
  tops <- vapply(list(products(saw, pulp), products(pulp, saw)), function(p) {
    result <- merchandise(1, 20, 100, 202, p, model = "F00FW2W202")
    top <- result$residuals[result$residuals$cause == "top", ]
    expect_identical(top$end_height, 100)
    top$start_height
  }, numeric(1))
  expect_identical(tops[1], tops[2])
  expected <- height_at_dib(20, 100, 3, 202, model = "F00FW2W202")$value
  expect_equal(tops[1], expected, tolerance = 1e-4)
  pulp$min_sed <- 0
  result <- merchandise(1, 20, 100, 202, products(pulp, saw), model = "F00FW2W202")
  expect_false(any(result$residuals$cause == "top"))
  pulp$min_sed <- saw$min_sed
  pulp$inside_bark <- FALSE
  for (p in list(products(saw, pulp), products(pulp, saw))) {
    result <- merchandise(1, 20, 100, 202, p, model = "F00FW2W202")
    expected <- if (p$inside_bark[1]) {
      height_at_dib(20, 100, 6, 202, model = "F00FW2W202")$value
    } else {
      height_at_dob(20, 100, 6, 202, model = "F00FW2W202")$value
    }
    expect_equal(result$residuals$start_height[result$residuals$cause == "top"],
      expected, tolerance = 1e-4
    )
  }
})
