feature_product <- function(...) {
  .mc_legacy_product("saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
                     scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
                     scale_bark_basis = "ib", ...)
}

test_that("summaries distinguish missing quantities from observed zero", {
  x <- merchandise(c(12, 14), 60, "demo.paraboloid", feature_product(), status = TRUE)
  x$logs$net_scale[] <- NA_real_
  expect_warning(out <- product_summary(x, table = "logs"), "net_scale.*product=saw")
  expect_true(is.na(out$net_scale))
  x$logs$net_scale[1] <- 0
  expect_equal(product_summary(x, table = "logs")$net_scale, 0)
  expect_true(is.na(product_summary(x, table = "logs", na_action = "propagate")$net_scale))
  expect_true(all(c("input_trees", "valid_trees", "failed_trees",
                    "valid_zero_log_trees") %in% names(product_summary(x))))
})

test_that("only defined segmentation labels can enter a product", {
  make <- function(option) {
    .mc_legacy_product(
      "saw", 1, min_length = 8, max_length = 32, length_step = 2,
      min_sed = 4, diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
      segmentation_policy = paste0("nvel_opt_", option)
    )
  }
  for (option in c(11:14, 21:24)) expect_s3_class(make(option), "merch_products")
  for (option in 15:20) expect_error(make(option), "nvel_opt_11.*nvel_opt_14.*nvel_opt_24")
})

test_that("Doyle retains the nominal end on a trimmed log", {
  saw <- .mc_legacy_product(
    "saw", 1, lengths = 16, trim = 2, min_sed = 4, diameter_basis = "ib",
    max_logs_per_segment = 1, allow_lower_products = FALSE,
    scale_rule = "doyle_formula", measurement_quantity = "board_foot",
    scale_unit = "board_foot", scale_bark_basis = "ib"
  )
  x <- merchandise(20, 80, "demo.paraboloid", saw, stump_ht = 1, status = TRUE)
  expect_equal(nrow(x$logs), 1)
  expect_equal(x$logs$end_height, 19)
  expect_equal(x$logs$gross_scale, 154.81717565903617)
  physical_diameter <- dib(20, 80, 19, "demo.paraboloid")
  expect_gt(abs(x$logs$gross_scale - (physical_diameter - 4)^2), 1)
})

test_that("local species names resolve through the species reference", {
  expect_equal(species_lookup("Douglas-fir", "common", "spcd"), 202)
  expect_equal(species_lookup("PSME", "symbol", "spcd"), 202)
  expect_equal(species_lookup("loblolly pine", "common", "spcd"), 131)
  expect_equal(species_lookup("Pinus taeda", "scientific", "spcd"), 131)
})

test_that("stand counts include zero logs, failures, and caller-only trees", {
  trees <- data.frame(id = letters[1:4], dbh = c(12, 12, 12, 12), spcd = 202)
  x <- merchandise(trees$dbh[1:3], c(60, 10, NA), "demo.paraboloid",
                   feature_product(), id = trees$id[1:3], spcd = 202, status = TRUE)
  expect_equal(x$trees$status, c(0, 410, 1))
  shuffled <- trees[c(4, 2, 1, 3), ]
  expect_warning(out <- stand_table(x, shuffled, c(7, 3, 2, 5), by = "species"),
                 "net_scale_ft3_per_acre.*merchandising_status=failed")
  expect_equal(sum(out$trees_per_acre), 17)
  valid <- out[out$merchandising_status == "valid", ]
  expect_equal(valid$trees_per_acre, 5)
  expect_equal(valid$basal_area_ft2_per_acre, 5 * pi / 4)
  expect_equal(valid$logs_per_acre, nrow(x$logs) * 2)
  expect_equal(valid$net_scale_ft3_per_acre, sum(x$logs$net_scale) * 2)
  expect_true(all(is.na(out$net_scale_ft3_per_acre[out$merchandising_status != "valid"])))
  expect_equal(attr(out, "failed_trees")$id, c("d", "c"))
  expect_warning(by_product <- stand_table(x, shuffled, c(7, 3, 2, 5)),
                 "net_scale_ft3_per_acre.*merchandising_status=failed")
  expect_equal(sum(by_product$trees_per_acre), 17)
  zero <- is.na(by_product$product) & by_product$merchandising_status == "valid"
  expect_equal(by_product$logs_per_acre[zero], 0)
  expect_equal(by_product$trees_per_acre[zero], 3)
  expect_error(stand_table(x, trees, NA), "expansion")
  expect_error(stand_table(x, trees, 1, class_width = 0), "class_width")
  expect_error(stand_table(x, trees[1:2, ], 1), "matching")
})

test_that("stand totals count trees once across products and retain units", {
  trees <- example_trees[1:2, ]
  trees$id <- trees$tree
  saw <- feature_product(max_logs_per_segment = 1)
  pulp <- .mc_legacy_product("pulp", 2, lengths = 8, min_sed = 2, diameter_basis = "ib",
                             scale_rule = "cubic", measurement_quantity = "green_weight",
                             scale_unit = "green_short_ton", scale_bark_basis = "ib",
                             pulp_product = TRUE)
  x <- with(trees, optimize_bucking(dbh, ht, model, products(saw, pulp),
                                    id = id, spcd = spcd, status = TRUE))
  out <- stand_table(x, trees, c(2, 3), by = character())
  expect_equal(out$trees_per_acre, 5)
  expect_equal(out$logs_per_acre, sum(c(2, 3)[match(x$logs$id, trees$id)]))
  expect_true(all(c("net_scale_ft3_per_acre",
                    "net_scale_green_short_ton_per_acre") %in% names(out)))
  expect_equal(stock_table(x, trees, 1), stand_table(x, trees, 1, by = c("product", "dbh_class")))
  x$logs$net_scale[1] <- NA_real_
  expect_true(is.na(stand_table(x, trees, 1, by = character())$net_scale_ft3_per_acre))
})

test_that("metric tables use square meters and caller area labels", {
  trees <- data.frame(id = "a", dbh = 30, spcd = 202)
  saw <- .mc_legacy_product("saw", 1, lengths = 4, min_sed = 10, diameter_basis = "ib",
                            scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "m3",
                            scale_bark_basis = "ib")
  x <- merchandise(30, 20, "demo.paraboloid", saw, id = "a", spcd = 202,
                   units = "metric", status = TRUE)
  out <- stand_table(x, trees, 10, area_unit = "hectare", by = "dbh_class")
  expect_equal(out$basal_area_m2_per_hectare, 10 * pi * 0.15^2)
  expect_equal(out$trees_per_hectare, 10)
})

test_that("plots handle defects, multiple trees, absent bark, and failed profiles", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  trees <- example_trees[1:2, ]
  trees$id <- trees$tree
  x <- with(trees, merchandise(dbh, ht, model, feature_product(trim = 0.5),
                               id = id, spcd = spcd,
                               defects = defect(id[2], 20, 30, "rot", percent = 15),
                               status = TRUE))
  original <- graphics::par("mfrow")
  expect_invisible(plot(x, tree = trees$id[2], main = "Rot and cuts"))
  expect_equal(nrow(x$logs[x$logs$id == trees$id[2], ]), 3)
  expect_invisible(plot(x, trees = trees$id))
  expect_identical(graphics::par("mfrow"), original)
  expect_error(plot(x, tree = "unknown"), "tree ids")
  expect_error(plot(x, tree = trees$id), "exactly one")
  expect_error(plot(x, tree = trees$id[1], trees = trees$id), "not both")
  no_defect <- with(trees, merchandise(dbh, ht, model, feature_product(),
                                       id = id, spcd = spcd, status = TRUE))
  expect_invisible(plot(no_defect))
  profile <- with(trees, stem_profile(dbh, ht, model, id = id))
  expect_s3_class(profile, "stem_profile")
  expect_invisible(plot(profile, trees = trees$id))
  profile$dob[] <- NA_real_
  expect_invisible(plot(profile))
  bad <- merchandise(12, NA_real_, "demo.paraboloid", feature_product(), status = TRUE)
  expect_invisible(plot(bad))
})

test_that("height fits retain measurements and display pooled species", {
  measured <- example_trees_pnw[!is.na(example_trees_pnw$ht_observed), ]
  fit <- with(measured, fit_height(dbh, ht_observed, spcd, group = plot, min_n = 50))
  expect_equal(nrow(fit$data), nrow(measured))
  expect_true(length(fit$pooled_species) > 0)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  expect_invisible(plot(fit))
  expect_invisible(plot(fit, units = "metric"))
})


test_that("status volume preserves optimizer controls", {
  trees <- example_trees[1:2, ]
  trees$id <- trees$tree
  large <- .mc_legacy_product(
    "large", 2, min_dbh = 100, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib", price = 1
  )
  specifications <- products(feature_product(), large)
  expect_warning(
    x <- with(trees, optimize_bucking(
      dbh, ht, model, specifications, id = id, spcd = spcd,
      objective = "value", unpriced = "exclude", currency = "USD"
    )),
    "no_entry_product"
  )
  expect_warning(out <- stand_table(x, trees, 5, by = character()),
                 "logs_per_acre.*merchandising_status=failed")
  expect_identical(out$merchandising_status, "failed")
  expect_equal(out$failed_trees_per_acre, 10)
  expect_true(is.na(out$logs_per_acre))
})
