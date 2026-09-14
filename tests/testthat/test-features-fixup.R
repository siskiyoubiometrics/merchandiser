fixup_product <- function(...) {
  .mc_legacy_product("saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
                     scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
                     scale_bark_basis = "ib", ...)
}

test_that("excluding every unpriced product raises an R error in isolation", {
  result <- callr::r(function(package_path) {
    if (file.exists(file.path(package_path, "R", "stem-zzz.R"))) {
      pkgload::load_all(package_path, quiet = TRUE)
    } else {
      library(merchandiser)
    }
    saw <- product("saw", 1, lengths = 16, min_sed = 4, volume_unit = "cubic")
    tryCatch(
      optimize_bucking(12, 60, "demo.paraboloid", saw, objective = "value",
                       unpriced = "exclude", currency = "USD"),
      error = function(e) list(message = conditionMessage(e), survived = TRUE)
    )
  }, args = list(package_path = find.package("merchandiser")))
  expect_true(result$survived)
  expect_match(result$message, "No products remain after excluding unpriced products")
  expect_match(result$message, "Add a product price")
})

test_that("summary missing warnings name every quantity and group once per call", {
  x <- merchandise(c(12, 14), 60, "demo.paraboloid", fixup_product(),
                   id = c("a", "b"), status = TRUE)
  x$logs$net_scale[] <- NA_real_
  x$logs$log_net_cubic_ib[] <- NA_real_
  messages <- character()
  out <- withCallingHandlers(
    product_summary(x, "logs", group = list(tree_group = c("a", "b"))),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(messages, 1)
  for (quantity in c("net_scale", "log_net_cubic_ib")) {
    for (id in c("a", "b")) expect_match(messages, paste0(quantity, " \\[tree_group=", id))
  }
  expect_true(all(is.na(out$net_scale)))
  expect_no_warning(silent <- product_summary(
    x, "logs", group = list(tree_group = c("a", "b")), na_action = "propagate"
  ))
  expect_identical(out, silent)
  trees <- data.frame(id = c("a", "b"), dbh = NA_real_, species = c("fir", "pine"))
  for (fun in list(stand_table, stock_table)) {
    messages <- character()
    out <- withCallingHandlers(fun(x, trees, 1, by = "species"), warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_length(messages, 1)
    for (quantity in c("basal_area_ft2_per_acre", "net_scale_ft3_per_acre")) {
      for (species in c("fir", "pine")) {
        expect_match(messages, paste0(quantity, " \\[species=", species))
      }
    }
    expect_true(all(is.na(out$net_scale_ft3_per_acre)))
    expect_no_warning(silent <- fun(x, trees, 1, by = "species", na_action = "propagate"))
    expect_identical(out, silent)
  }
})

test_that("stand tables recover cascade status and preserve empty schemas", {
  trees <- data.frame(id = "a", dbh = 12, spcd = 202)
  x <- merchandise(12, 60, "demo.paraboloid", fixup_product(), id = "a")
  expect_null(x$trees$status)
  out <- stand_table(x, trees, 2)
  expect_identical(out$merchandising_status, "valid")
  expect_equal(out$logs_per_acre, 2 * nrow(x$logs))
  x$trees <- x$trees[FALSE, ]
  x$trees$status <- integer()
  x$logs <- x$logs[FALSE, ]
  for (fun in list(stand_table, stock_table)) {
    empty <- fun(x, trees[FALSE, ], numeric(), by = "species")
    expect_equal(nrow(empty), 0)
    expect_named(empty, c("species", "merchandising_status", "trees_per_acre",
                          "failed_trees_per_acre", "basal_area_ft2_per_acre",
                          "logs_per_acre", "net_scale_ft3_per_acre"))
    expect_type(empty$net_scale_ft3_per_acre, "double")
    expect_identical(attr(empty, "failed_trees"),
                     data.frame(id = character(), status = integer(), expansion = numeric()))
  }
  expect_identical(product_summary(x), data.frame())
})

test_that("stand validation diagnoses invalid inputs and ambiguous output units", {
  trees <- data.frame(id = "a", dbh = 12, spcd = 202)
  x <- merchandise(12, 60, "demo.paraboloid", fixup_product(), id = "a", status = TRUE)
  expect_error(stand_table(list(), trees, 1), "merchandise result")
  expect_error(stand_table(x, trees), "Supply expansion")
  expect_error(stand_table(x, trees, 1, by = "unknown"), "distinct tree columns")
  expect_error(stock_table(x, trees, 1, by = c("id", "id")), "distinct tree columns")
  expect_error(stand_table(x, trees, 1, area_unit = NA_character_), "area_unit")
  expect_error(stock_table(x, trees, 1, na_action = "exclude"), "na_action")
  x$logs$scale_unit[1] <- "a-b"
  x$run_metadata$call$products$scale_unit[1] <- "a.b"
  expect_error(stand_table(x, trees, 1), "Measurement units produce duplicate column names")
})

test_that("plot dispatch draws named colors and defect geometry", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  x <- optimize_bucking(12, 60, "demo.paraboloid", fixup_product(), id = "a", status = TRUE)
  x$run_metadata$call$defects <- data.frame(
    id = "a", from = c(NA, 20, 30), to = c(NA, 20, 40),
    effect = c("break", "fork", "rot"), percent = c(NA, NA, 15),
    category = c(NA, "major", NA)
  )
  points <- bands <- drawn_text <- list()
  local_mocked_bindings(
    points = function(x, y, ...) points[[length(points) + 1L]] <<- list(x = x, y = y),
    polygon = function(x, y, col, ...) {
      bands[[length(bands) + 1L]] <<- list(x = x, y = y, col = col)
    },
    text = function(x, y, labels, ...) {
      drawn_text[[length(drawn_text) + 1L]] <<- list(x = x, y = y, label = labels)
    },
    .package = "graphics"
  )
  expect_identical(withVisible(plot(x, col = c(saw = "orange"), legend = FALSE)),
                   list(value = x, visible = FALSE))
  expect_length(points, 1)
  expect_equal(points[[1]]$x, 20 - x$trees$stump_height)
  expect_true(any(vapply(bands, function(band) identical(band$col, "orange"), logical(1))))
  rot <- bands[[which(vapply(bands, function(band) identical(band$col, "firebrick"), logical(1)))]]
  expect_equal(range(rot$x), c(30, 40) - x$trees$stump_height)
  text_labels <- vapply(drawn_text, `[[`, character(1), "label")
  expect_true(all(c("fork major", "rot 15%") %in% text_labels))
  expect_false("break" %in% text_labels)
  expect_error(plot(x, col = c(other = "red")), "color for every product")
  expect_error(plot(x, col = c("red", "blue")), "color for every product")
  expect_error(plot(x, legend = NA), "status")
})

test_that("profile plots label failures and respect diameter limits", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  profile <- stem_profile(12, 60, "demo.paraboloid", bark_ratio = 0.9, status = TRUE)
  frames <- labels <- list()
  original_plot <- graphics::plot.default
  local_mocked_bindings(
    plot.default = function(...) {
      frames[[length(frames) + 1L]] <<- list(...)
      original_plot(...)
    },
    text = function(x, y, ...) labels[[length(labels) + 1L]] <<- list(...),
    .package = "graphics"
  )
  plot(profile)
  expect_equal(frames[[1]]$xlim, c(0, max(profile$dob) * 1.05))
  plot(profile, xlim = c(0, 40))
  expect_equal(frames[[2]]$xlim, c(0, 40))
  profile$dib[] <- NA_real_
  expect_identical(withVisible(plot(profile)), list(value = profile, visible = FALSE))
  expect_length(frames, 2)
  expect_match(tail(labels, 1)[[1]][[1]], "Stem profile unavailable")
  expect_error(plot(profile, trees = character()), "tree ids")
  old_fit <- structure(list(), class = "height_fit")
  expect_error(plot(old_fit), "Refit with fit_height")
  expect_error(plot(old_fit, units = "bad"), "units")
})

test_that("shipped tree data have documented schemas and missing value meaning", {
  common <- c("stand", "plot", "tree", "spcd", "species", "dbh", "ht_observed", "ht_simulated",
              "age", "pruned", "region", "forest", "district", "longitude", "latitude",
              "expansion_factor")
  expect_equal(dim(example_trees_pnw), c(300, 16))
  expect_named(example_trees_pnw, common)
  expect_equal(which(!is.na(example_trees_pnw$ht_observed)), seq(1L, 300L, 3L))
  expect_false(anyNA(example_trees_pnw[setdiff(common, "ht_observed")]))
  expect_equal(dim(example_trees_south), c(400, 19))
  expect_setequal(names(example_trees_south), c(common, "saw_stop", "pulp_stop", "jump_butt"))
  expect_equal(colSums(!is.na(example_trees_south[c("saw_stop", "pulp_stop", "jump_butt")])),
               c(saw_stop = 60, pulp_stop = 32, jump_butt = 1))
  expect_false(anyNA(example_trees_south[common]))
  expect_equal(dim(example_trees), c(6, 6))
  expect_named(example_trees, c("tree", "spcd", "species", "dbh", "ht", "model"))
  for (trees in list(example_trees_pnw, example_trees_south, example_trees)) {
    expect_identical(anyDuplicated(trees$tree), 0L)
    expect_true(all(trees$dbh > 0))
  }
})
