test_that("free assignment matches exhaustive short stem maximizers and ties", {
  for (case in seq_len(24)) {
    p <- products(
      product("saw", min_length = 2, max_length = 3, min_sed = case %% 3,
              trim = (case %% 2) / 2, volume_unit = "cubic", price = 2,
              max_logs = if (case %% 4 == 0) 1 else NA_real_),
      product("pulp", min_length = 1.5, max_length = 2, min_sed = 0,
              volume_unit = "cubic", price = 1)
    )
    if (case %% 2 == 0) {
      p <- products(p, product("third", min_length = 2, max_length = 2.5,
                               min_sed = 0, volume_unit = "cubic", price = 2, max_logs = 1))
    }
    ht <- 5 + (case %% 5) / 2
    dbh <- 8 + case %% 4
    patterns <- .optimize_patterns(p, ht, dbh)
    values <- vapply(patterns, function(x) sum(x$value), numeric(1))
    maximum <- max(values)
    winners <- patterns[abs(values - maximum) <= 1e-13]
    counts <- vapply(winners, nrow, integer(1))
    winners <- winners[counts == min(counts)]
    for (j in seq_len(nrow(winners[[1]]))) {
      lengths <- vapply(winners, function(x) x$length[j], numeric(1))
      winners <- winners[lengths == max(lengths)]
    }
    x <- merchandise(case, dbh, ht, 202, p, model = "demo.paraboloid", strategy = "optimize")
    expect_equal(sum(x$logs$value), maximum, tolerance = 1e-13, info = as.character(case))
    matches <- vapply(winners, function(w) {
      identical(x$logs$product, w$product) && identical(x$logs$start_height, w$start) &&
        identical(x$logs$length, w$length)
    }, logical(1))
    expect_true(any(matches), info = as.character(case))
  }
})

test_that("one product with flat prices agrees when longest cuts fill the stem", {
  p <- product("wood", min_length = 8, max_length = 16, min_sed = 0,
               volume_unit = "cubic", price = 1)
  for (height in c(17, 25, 33, 41, 49)) {
    a <- merchandise(1, 12, height, 202, p, model = "demo.paraboloid")
    b <- merchandise(1, 12, height, 202, p, model = "demo.paraboloid", strategy = "optimize")
    expect_identical(a[1:4], b[1:4])
  }
})

test_that("product permutations preserve free assignment including equal prices", {
  p <- products(
    product("saw", min_length = 8, max_length = 16, min_sed = 6,
            volume_unit = "cubic", price = 2, max_logs = 2),
    product("pulp", min_length = 8, max_length = 16, min_sed = 0,
            volume_unit = "cubic", price = 1),
    product("other", min_length = 8, max_length = 16, min_sed = 6,
            volume_unit = "cubic", price = 2, max_logs = 1)
  )
  run <- function(p) {
    merchandise(1, 12, 65, 202, p, model = "demo.paraboloid",
                strategy = "optimize")
  }
  expected <- run(p)
  for (order in list(c(3, 2, 1), c(2, 1, 3), c(2, 3, 1), c(1, 3, 2), c(3, 1, 2))) {
    expect_identical(run(p[order, ]), expected)
  }
  # The higher priced product can resume above a lower priced butt restriction.
  x <- merchandise(1, 12, 41, 202, p, model = "demo.paraboloid", strategy = "optimize",
                   defects = defect(1, 1, 9, "restrict", product = "pulp"))
  expect_identical(x$logs$product[1], "pulp")
  expect_true(any(x$logs$product[-1] != "pulp"))
})

test_that("optimize honors butt rot, saw stops, pulp trees, broken tops and sweep", {
  p <- .optimize_specs()
  run <- function(d) {
    merchandise(1, 24, 81, 202, p, model = "demo.paraboloid",
                strategy = "optimize", defects = d)
  }
  butt <- run(defect(1, 1, 8, "restrict", product = "pulp"))
  expect_identical(butt$logs$start_height[1], 8)
  expect_identical(butt$logs$product[1], "saw")
  expect_identical(butt$residuals$start_height, c(0, 1, 80))
  expect_identical(butt$residuals$end_height, c(1, 8, 81))
  expect_identical(butt$residuals$cause, c("stump", "restricted", "short_remainder"))
  saw_stop <- run(defect(1, 33, NA, "restrict", product = "pulp"))
  expect_identical(saw_stop$logs$start_height[1:3], c(1, 17, 33))
  expect_true(all(saw_stop$logs$product[saw_stop$logs$start_height >= 33] == "pulp"))
  whole <- run(defect(1, 0, NA, "restrict", product = "pulp"))
  expect_identical(whole$logs$start_height[1], 1)
  expect_true(all(whole$logs$product == "pulp"))
  sweep <- run(defect(1, 1, 17, "sweep", percent = 20))
  expect_identical(sweep$logs$start_height[1:3], c(1, 9, 17))
  expect_identical(sweep$logs$product[1:3], c("pulp", "pulp", "saw"))
  for (x in list(saw_stop, whole, sweep)) {
    expect_identical(x$residuals$start_height, 0)
    expect_identical(x$residuals$end_height, 1)
    expect_identical(x$residuals$cause, "stump")
  }
  broken <- run(defect(1, 33, NA, "end"))
  expect_identical(broken$logs$start_height, c(1, 17))
  expect_identical(broken$residuals$start_height, c(0, 33))
  expect_identical(broken$residuals$end_height, c(1, 81))
  expect_identical(broken$residuals$cause, c("stump", "end"))
})

test_that("capped products stop and counts restart at culls and restrictions", {
  p <- .optimize_specs()
  p$max_logs[1] <- 2
  run <- function(d = NULL) {
    merchandise(1, 24, 97, 202, p, model = "demo.paraboloid",
                strategy = "optimize", defects = d)
  }
  expect_equal(sum(run()$logs$product == "saw"), 2)
  for (effect in c("cull", "restrict")) {
    x <- run(defect(1, 33, 41, effect, product = if (effect == "restrict") "pulp" else NULL))
    expect_identical(x$logs$start_height[x$logs$product == "saw"], c(1, 17, 41, 57))
  }
})

test_that("PNW optimize is identical at one and eight threads with mapped defects", {
  trees <- example_trees_pnw
  p <- .optimize_specs()
  run <- function(n) {
    with_threads(n, merchandise(trees$tree_id, trees$dbh, trees$ht, trees$spcd,
                                p, age = trees$age, defects = example_defects_pnw,
                                strategy = "optimize", quiet = TRUE))
  }
  expect_identical(run(1), run(8))
})

test_that("optimized positions, product limits, pruning and full coverage hold", {
  for (inside in c(TRUE, FALSE)) {
    for (trim in c(0, 0.5)) {
      p <- .optimize_specs()
      p$inside_bark <- inside
      p$trim <- trim
      p$min_sed <- c(8, 3)
      p$max_sed <- c(30, NA)
      p$min_led <- c(8, 0)
      p$max_led <- c(40, NA)
      p$requires_pruned[1] <- TRUE
      p$spcd[[1]] <- 202
      p$min_age[1] <- 20
      p$max_age[1] <- 60
      p$min_dbh[1] <- 12
      p$max_dbh[1] <- 30
      d <- rbind(defect(1, 10, 20, "cull"), defect(1, 60, 80, "restrict", product = "pulp"),
                 defect(1, 33, 49, "sweep", percent = 20))
      x <- merchandise(1, 24, 120, 202, p, model = "F00FW2W202", defects = d,
                       pruned_ht = 60, age = 40, strategy = "optimize")
      logs <- x$logs
      spec <- p[match(logs$product, p$product), ]
      expect_gt(nrow(logs), 0)
      expect_true(all(diff(logs$start_height) >= 0))
      expect_true(all(head(logs$end_height, -1) <= tail(logs$start_height, -1)))
      expect_equal(logs$end_height, logs$start_height + logs$length + spec$trim)
      expect_true(all(logs$length >= spec$min_length & logs$length <= spec$max_length))
      expect_true(all(logs$sed >= spec$min_sed & logs$led >= spec$min_led))
      expect_true(all(is.na(spec$max_sed) | logs$sed <= spec$max_sed))
      expect_true(all(is.na(spec$max_led) | logs$led <= spec$max_led))
      expect_true(all(logs$end_height[spec$requires_pruned] <= 60))
      expect_false(any(logs$product == "saw" & logs$start_height < 49 & logs$end_height > 33))
      pieces <- rbind(data.frame(start_height = logs$start_height,
                                 end_height = logs$start_height + logs$length),
                      x$residuals[c("start_height", "end_height")])
      pieces <- pieces[order(pieces$start_height), ]
      expect_identical(pieces$start_height[1], 0)
      expect_identical(tail(pieces$end_height, 1), 120)
      expect_true(all(pieces$end_height > pieces$start_height))
      expect_equal(head(pieces$end_height, -1), tail(pieces$start_height, -1))
    }
  }
})

test_that("fractional segment starts and trim preserve the shared cut geometry", {
  for (trim in c(0.25, 0.5)) {
    p <- product("wood", min_length = 8, max_length = 8, min_sed = 0,
                 volume_unit = "cubic", price = 1, trim = trim)
    run <- function(strategy) {
      merchandise(1, 12, 40, 202, p, model = "demo.paraboloid", stump_ht = 1.1,
                  defects = defect(1, 17.3, 20.3, "cull"), strategy = strategy)
    }
    expect_identical(run("cascade")[1:4], run("optimize")[1:4])
  }
})
