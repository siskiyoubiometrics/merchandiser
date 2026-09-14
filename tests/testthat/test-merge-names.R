test_that("deprecated names forward to the new names", {
  saw <- .mc_legacy_product(
    "saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  result <- merchandise(12, 80, "demo.paraboloid", saw, status = TRUE)
  calls <- list(
    merch_product = list("saw", 1, lengths = 16, min_sed = 4,
                         diameter_basis = "ib", scale_rule = "cubic",
                         measurement_quantity = "cubic", scale_unit = "ft3",
                         scale_bark_basis = "ib"),
    merch_products = list(saw),
    preset = list("pnw"), list_presets = list(),
    merch_defect = list(1, 10, 20, "rot", percent = 10),
    merch_summary = list(result),
    merch_summary_by_product = list(
      12, 80, species = 202, model = "demo.paraboloid", products = saw
    ),
    merch_volume = list(12, 80, species = 202, model = "demo.paraboloid", products = saw),
    merch_piece_count = list(12, 80, species = 202, model = "demo.paraboloid", products = saw),
    mc_status_codes = list(), tv_status_codes = list(),
    tv_models = list(), taper_model = list("demo.paraboloid"),
    has_model = list("demo.paraboloid"), check_model_ids = list("demo.paraboloid"),
    registry_manifest = list(), check_registry_manifest = list(taper_manifest()),
    tv_threads = list()
  )
  replacements <- c(
    merch_product = "product", merch_products = "products",
    preset = "product_preset", list_presets = "example_product_names",
    merch_defect = "defect", merch_summary = "product_summary",
    merch_summary_by_product = "product_summary_by_tree",
    merch_volume = "tree_cubic_volume", merch_piece_count = "tree_log_count",
    mc_status_codes = "status_codes", tv_status_codes = "status_codes",
    tv_models = "taper_models", taper_model = "get_taper_model",
    has_model = "has_taper_model", check_model_ids = "check_taper_models",
    registry_manifest = "taper_manifest", check_registry_manifest = "check_taper_manifest",
    tv_threads = "threads"
  )
  for (old in names(calls)) {
    new <- replacements[[old]]
    expected <- do.call(get(new), calls[[old]])
    actual <- suppressMessages(do.call(get(old), calls[[old]]))
    expect_identical(actual, expected, info = old)
  }
  expect_identical(suppressMessages(merchandiser::tv_species), species_reference)
})

test_that("model constructor and registry aliases retain behavior", {
  inside <- function(dbh, ht, h, aux) as.double(dbh * (1 - h / ht))
  model <- new_taper_model("merge.alias", "local", inside)
  old <- suppressMessages(taper_model_spec("merge.alias", "local", inside))
  expect_identical(old, model)
  register_taper_model(model)
  expect_true(has_taper_model(model$id))
  unregister_taper_model(model$id)
  suppressMessages(register_model(old))
  expect_identical(get_taper_model(model$id), model)
  suppressMessages(unregister_model(model$id))
  expect_false(has_taper_model(model$id))
})

test_that("deprecation messages occur once per session and name replacements", {
  key <- "merch_product()"
  if (exists(key, .merge_deprecations, inherits = FALSE)) {
    rm(list = key, envir = .merge_deprecations)
  }
  args <- list(
    "saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib"
  )
  expect_message(do.call(merch_product, args), "Use product\\(\\)")
  expect_message(do.call(merch_product, args), NA)
})

test_that("product field and summary table spellings have transition support", {
  current <- .mc_legacy_product(
    "saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib", min_boundary_length = 8,
    allow_lower_products = FALSE, max_logs_per_segment = 2
  )
  previous <- suppressMessages(.mc_legacy_product(
    "saw", 1, lengths = 16, min_sed = 4, diameter_basis = "ib",
    scale_rule = "exact_profile", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib", min_top_length = 8,
    continue_to_top = FALSE, max_pieces = 2
  ))
  expect_identical(previous, current)
  result <- merchandise(12, 80, "demo.paraboloid", current, status = TRUE)
  expect_identical(suppressMessages(product_summary(result, "pieces")),
                   product_summary(result, "logs"))
  expect_identical(suppressMessages(product_summary(result, "stems")),
                   product_summary(result, "trees"))
})


test_that("species alias diagnostics distinguish access from namespace inspection", {
  if (exists("tv_species", .merge_deprecations, inherits = FALSE)) {
    rm(list = "tv_species", envir = .merge_deprecations)
  }
  expect_message(get("tv_species", asNamespace("merchandiser")), NA)
  expect_message(Filter(function(name) {
    is.function(asNamespace("merchandiser")[[name]])
  }, "tv_species"), NA)
  expect_message(merchandiser::tv_species, "Use species_reference")
  expect_message(merchandiser::tv_species, NA)
})
