test_that("product order is priority and fields are closed", {
  a <- product("saw",
    spcd = c(202, 263), min_length = 16, max_length = 40, min_sed = 8,
    volume_unit = "scribner"
  )
  b <- product("pulp", min_length = 8, max_length = 40, min_sed = 3, volume_unit = "cubic")
  expect_identical(names(a), names(formals(product)))
  expect_identical(products(b, a)$product, c("pulp", "saw"))
  expect_identical(a$spcd[[1]], c(202, 263))
  expect_identical(a$length_round, 1)
  expect_identical(a$trim, 0)
  expect_identical(a$requires_pruned, FALSE)
  expect_error(product("bad",
                 spcd = "Douglas-fir", min_length = 16, max_length = 32, min_sed = 6,
                 volume_unit = "cubic"
               ), "spcd")
  b$unknown_field <- 1
  expect_error(.mc_validate_products(b), "unknown|Unknown|Unsupported|unsupported")
  expect_error(products(a, a), "unique|duplicate")
  expect_error(products(), "Supply at least one product.", fixed = TRUE)
})

test_that("product bounds and measurement choices are checked", {
  p <- product("saw", min_length = 16, max_length = 32, min_sed = 6, volume_unit = "cubic")
  cases <- list(
    min_length = 0, max_length = 8, length_round = 0, trim = -1, min_dbh = -1,
    max_dbh = -1, min_sed = -1, max_sed = 3, min_led = -1, max_led = -1, max_sweep = 101,
    max_logs = 1.5, price = -1, price_per = 0
  )
  for (name in names(cases)) {
    invalid <- p
    invalid[[name]] <- cases[[name]]
    expect_error(.mc_validate_products(invalid), info = name)
  }
  expect_error(
    product("cord", min_length = 8, max_length = 16, min_sed = 3, volume_unit = "cord"),
    "cord_solid_fraction"
  )
  for (name in c("inside_bark", "requires_pruned", "split_scale")) {
    invalid <- p
    invalid[[name]] <- NA
    expect_error(.mc_validate_products(invalid), info = name)
  }
})

test_that("diameter and age limits qualify the tree and pruning qualifies the cut", {
  p <- products(product("young",
                  spcd = 131, min_dbh = 8, max_dbh = 14, min_age = 10, max_age = 20,
                  min_length = 16, max_length = 16, min_sed = 0, volume_unit = "cubic"
                ), product("older",
                  spcd = 131, min_dbh = 8, min_age = 20, min_length = 16, max_length = 16,
                  min_sed = 0,
                  volume_unit = "cubic"
                ))
  x <- merchandise(1:3, c(12, 12, 7), 80, 131, p, age = c(15, 25, 25), quiet = TRUE)
  expect_true(all(x$logs$product[x$logs$tree_id == 1] == "young"))
  expect_true(all(x$logs$product[x$logs$tree_id == 2] == "older"))
  expect_identical(x$status$tree_id, 3L)
  expect_identical(x$status$status, 400L)
})

test_that("defect records preserve tree identifiers and validate the four effects", {
  p <- product("pulp", min_length = 8, max_length = 16, min_sed = 3, volume_unit = "cubic")
  d <- defect(c("a", "a", "b", "b"), c(1, 20, 50, 10), c(8, NA, NA, 30), c(
    "cull", "restrict",
    "end", "sweep"
  ), product = c(NA, "pulp", NA, NA), percent = c(NA, NA, NA, 25))
  expect_identical(names(d), c("tree_id", "start_height", "end_height", "effect", "product",
                               "percent"))
  checked <- validate_defects(
    d,
    c("a", "b"),
    c(80, 80),
    p
  )
  expect_identical(checked$status, integer(4))
  expect_identical(validate_defects(
    checked,
    c("a", "b"),
    c(80, 80),
    p
  ), checked)
  expect_identical(validate_defects(
    d,
    "other",
    80,
    p
  )$status, rep(405L, nrow(d)))
  bad <- d
  bad$percent[4] <- 101
  expect_identical(validate_defects(
    bad,
    c("a", "b"),
    c(80, 80),
    p
  )$status[4], 403L)
  bad <- d
  bad$start_height[1] <- 9
  expect_identical(validate_defects(
    bad,
    c("a", "b"),
    c(80, 80),
    p
  )$status[1], 402L)
})

test_that("stopping heights translate into the new defect model", {
  d <- defects_from_stoppers(
    "a",
    100,
    topwood_product = "pulpwood",
    saw_stop = 50,
    pulp_stop = 80,
    jump_butt = 8
  )
  expect_identical(d$effect, c("cull", "restrict", "end"))
  expect_identical(d$start_height, c(1, 50, 80))
  expect_identical(d$product[2], "pulpwood")
  expect_true(is.na(d$end_height[2]))
  whole <- defects_from_stoppers(
    "a",
    100,
    topwood_product = "pulpwood",
    pulp_tree = TRUE
  )
  expect_identical(whole$start_height, 0)
  expect_true(is.na(whole$end_height))
  expect_error(
    defects_from_stoppers(
      "a",
      100,
      topwood_product = "pulpwood",
      saw_stop = 80,
      pulp_stop = 50
    ),
    "increase|below"
  )
  expect_error(
    defects_from_stoppers(
      "a",
      100,
      topwood_product = "pulpwood",
      saw_stop = 50,
      pulp_tree = TRUE
    ),
    "whole pulp"
  )
})

test_that("empty and failed input rows retain typed tables", {
  p <- product("saw", min_length = 16, max_length = 32, min_sed = 6, volume_unit = "cubic")
  empty <- merchandise(character(), numeric(), numeric(), numeric(), p, quiet = TRUE)
  expect_identical(names(empty), c("logs", "residuals", "status", "assumptions", "call"))
  for (table in empty[1:4]) {
    expect_identical(names(table)[1], "tree_id")
    expect_identical(nrow(table), 0L)
    expect_type(table$tree_id, "character")
  }
  x <- merchandise(c("bad", "good"), c(NA, 24), 120, 202, p, quiet = TRUE)
  expect_identical(x$status$tree_id, "bad")
  expect_identical(x$status$status, 1L)
  expect_true(all(x$logs$tree_id == "good"))
  expect_identical(names(x$status), c("tree_id", "status", "name", "description"))
  expect_identical(assumptions(x), x$assumptions)
  expect_error(merchandise(c("a", "a"), 24, 120, 202, p), "unique")
  expect_error(merchandise(1:2, 1:3, 120, 202, p), "size-one")
})

test_that("per-tree helpers and biomass splice into a tree list", {
  trees <- example_trees
  volume <- stem_volume(trees$dbh, trees$ht, trees$spcd)
  expected <- stem_volume(trees$dbh, trees$ht, trees$spcd, stump_ht = 1)
  expect_identical(volume, expected)
  output <- dplyr::mutate(trees, biomass(dbh, ht, spcd))
  expect_identical(output$tree_id, trees$tree_id)
  expect_true("tco2e" %in% names(output))
  invalid <- biomass(c(12, NA, -1, 12), 80, c(202, 202, 202, -1))
  expect_identical(invalid$status, c(0L, 1L, 2L, 7L))
  expect_identical(nrow(invalid), 4L)
})

test_that("replaying a call and changing thread count preserve the result", {
  p <- product("saw",
    min_length = 16, max_length = 32, min_sed = 6, price = 100, price_per = 100,
    volume_unit = "scribner"
  )
  trees <- example_trees
  x <- with_threads(1, merchandise(trees$tree_id, trees$dbh, trees$ht, trees$spcd, p,
    model = trees$model,
    quiet = TRUE
  ))
  y <- with_threads(4, do.call(merchandise, c(x$call, list(quiet = TRUE))))
  expect_identical(y, x)
  expect_true(all(c("age", "pruned_ht") %in% names(x$call)))
})

test_that("felled plots preserve an existing panel layout", {
  p <- product("saw",
    min_length = 16, max_length = 32, min_sed = 6, trim = 0.5, price = 100,
    volume_unit = "scribner"
  )
  d <- rbind(defect("a", 1, 8, "cull"), defect("a", 40, 50, "restrict",
               product = "saw"
             ), defect("a",
               70, 90, "sweep",
               percent = 20
             ), defect("a", 100, NA, "end"))
  x <- merchandise("a", 24, 120, 202, p, defects = d, quiet = TRUE)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2, 2))
  expect_silent(plot(x))
  expect_identical(graphics::par("mfrow"), c(2L, 2L))
  expect_output(print(x), "log")
})
