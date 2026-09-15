test_that("coverage outside bark assumptions use species or caller ratios", {
  p <- coverage_product(inside_bark = FALSE)
  id <- "831CLKE131"
  expect_false(get_taper_model(id)$kernel$has_dob)
  x <- merchandise(1, 20, 100, 131, p, model = id)
  bark <- x$assumptions[x$assumptions$assumption == "bark_ratio", ]
  # Source: shipped species_reference bark_ratio for loblolly pine.
  ratio <- species_reference$bark_ratio[match(131, species_reference$spcd)]
  expect_equal(bark$value, ratio)
  expect_identical(bark$source, "species_reference")
  expect_gt(nrow(x$logs), 0)
  inside <- dib(20, 100, x$logs$end_height, 131, model = id)$value
  expect_true(length(inside) == nrow(x$logs) && all(is.finite(inside)))
  expect_equal(x$logs$sed, inside / ratio, tolerance = 1e-10)
  supplied <- 0.85
  caller <- merchandise(1, 20, 100, 131, p, model = id, bark_ratio = supplied)
  bark <- caller$assumptions[caller$assumptions$assumption == "bark_ratio", ]
  expect_equal(bark$value, supplied)
  expect_identical(bark$source, "caller")
  expect_gt(nrow(caller$logs), 0)
  inside <- dib(20, 100, caller$logs$end_height, 131, model = id)$value
  expect_true(length(inside) == nrow(caller$logs) && all(is.finite(inside)))
  expect_equal(caller$logs$sed, inside / supplied, tolerance = 1e-10)
})

test_that("coverage taper maps select models and explicit models take precedence", {
  mapping <- data.frame(spcd = 202, model = "demo.paraboloid")
  p <- coverage_product()
  mapped <- merchandise(1, 20, 100, 202, p, taper_map = mapping, quiet = TRUE)
  explicit <- merchandise(1, 20, 100, 202, p, taper_map = mapping, model = "F00FW2W202")
  expect_identical(mapped$assumptions$model[mapped$assumptions$assumption == "species_model"],
                   mapping$model)
  expect_identical(
    explicit$assumptions$model[explicit$assumptions$assumption == "species_model"],
    "F00FW2W202"
  )
  mapping$model <- "coverage.unregistered"
  expect_error(merchandise(1, 20, 100, 202, p, taper_map = mapping), "taper_map")
  ## The unregistered model id is named so the user can see which row is wrong.
  expect_error(merchandise(1, 20, 100, 202, p, taper_map = mapping), "coverage.unregistered")
})

test_that("coverage an unmapped species has its own unresolved status", {
  mapping <- data.frame(spcd = 202, model = "F00FW2W202")
  x <- merchandise(c("mapped", "unmapped"), 20, 100, c(202, 131), coverage_product(),
                   taper_map = mapping, quiet = TRUE)
  # Source: status_codes model_unresolved is 404, only on the unmapped input.
  expect_identical(x$status$tree_id, "unmapped")
  expect_identical(x$status$status, 404L)
  expect_identical(unique(x$logs$tree_id), "mapped")
})

test_that("coverage species defaults reach merchandising assumptions and statuses", {
  missing <- dplyr::anti_join(species_reference, default_taper_models, by = "spcd")$spcd[1]
  species <- c(202, 131, 121, missing)
  x <- merchandise(species, 20, 100, species, coverage_product(), quiet = TRUE)
  # Source: shipped data/default_taper_models.rda, not output from merchandise.
  expected <- default_taper_models$model[match(species, default_taper_models$spcd)]
  expected[is.na(expected)] <- ""
  selected <- x$assumptions[x$assumptions$assumption == "species_model", ]
  expect_identical(selected$model, expected)
  expect_identical(x$status$tree_id, species[species == missing])
  # Source: public status_codes model_unresolved = 404. Other trees succeed.
  expect_identical(x$status$status, 404L)
  expect_setequal(unique(x$logs$tree_id), species[species != missing])
})

test_that("coverage default-equation messages obey quiet", {
  p <- coverage_product()
  expect_message(merchandise(1, 20, 100, 202, p, model = NULL),
                 "Selected taper equations by species")
  expect_message(merchandise(1, 20, 100, 202, p, model = NULL, quiet = TRUE), NA)
})
