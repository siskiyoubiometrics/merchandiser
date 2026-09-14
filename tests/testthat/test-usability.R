test_that("front door resolves ordinary species inputs and integer trees", {
  expect_message(
    result <- merchandise(
      c(12L, 14L), c(80L, 85L), factor(c("PSME", "Douglas-fir")),
      age = c(25L, 30L), pruned = c(0L, 1L), status = 1L
    ),
    "pnw.*assumed model\\(s\\) F00FW2W202"
  )
  expect_s3_class(result, "merch_result")
  expect_identical(result$trees$model, rep("F00FW2W202", 2L))
  expect_identical(result$trees$age, c(25, 30))
  expect_identical(result$trees$status, c(0L, 0L))

  quiet <- expect_silent(
    merchandise(12L, 80L, "Douglas-fir", quiet = TRUE, status = TRUE)
  )
  expect_identical(quiet$trees$model, "F00FW2W202")
})

test_that("southern defaults select a model, preset, and bark ratio", {
  result <- merchandise(
    12L, 80L, "loblolly pine", quiet = TRUE, status = TRUE
  )
  expect_identical(result$trees$model, "831CLKE131")
  expect_identical(result$trees$status, 0L)
  expect_true(nrow(result$logs) > 0L)
  expect_identical(
    result$run_metadata$front_door$preset,
    "us_south"
  )
})

test_that("every result carries the disclosed assumption row types", {
  expected_ratio <- merchandiser::species_lookup(
    131L, from = "spcd", to = "bark_ratio"
  )
  expect_message(
    result <- merchandise(12, 80, species = 131L, status = TRUE),
    "Filled bark_ratio from the species table for species code\\(s\\) 131"
  )
  recorded <- assumptions(result)
  expect_identical(recorded, result$run_metadata$assumptions)
  expect_true(all(c(
    "species_model", "bark_ratio", "product_preset", "stump_height",
    "utilization_top", "unit_system"
  ) %in% recorded$assumption))

  mapping <- recorded[recorded$assumption == "species_model", ]
  expect_identical(mapping$spcd, 131L)
  expect_identical(mapping$model, "831CLKE131")
  expect_identical(mapping$source, "nvel_default")
  expect_identical(mapping[c("region", "forest", "district")],
                   data.frame(region = 8L, forest = 8L, district = 0L))

  bark <- recorded[recorded$assumption == "bark_ratio", ]
  expect_identical(bark$spcd, 131L)
  expect_equal(bark$value, expected_ratio)
  expect_identical(bark$source, "species_table")

  explicit_top <- merchandise(
    12, 80, species = 131L, utilization_top = 3,
    quiet = TRUE, status = TRUE
  )
  expect_false(
    "utilization_top" %in% assumptions(explicit_top)$assumption
  )

  expect_error(assumptions(data.frame()), "merch_result")
})

test_that("taper maps resolve species and explicit models take precedence", {
  products <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 1, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  taper_map <- data.frame(
    spcd = c(202L, 263L),
    species = c("Douglas-f-fir", "western hemlock"),
    model = c("F00FW2W202", "F03FW2W263"),
    stringsAsFactors = FALSE
  )
  expect_error(
    merchandise(
      12, 40, species = 202L, taper_map = taper_map, products = products,
      quiet = TRUE
    ),
    "missing or unrecognized species"
  )
  taper_map$species[[1L]] <- "Douglas-fir"
  mapped <- merchandise(
    c(12, 12), c(40, 40), species = c("PSME", "western hemlock"),
    taper_map = taper_map, products = products, quiet = TRUE, status = TRUE
  )
  expect_identical(mapped$trees$model, taper_map$model)
  mapping <- assumptions(mapped)
  mapping <- mapping[mapping$assumption == "species_model", ]
  expect_true(all(mapping$source == "caller_taper_map"))

  names_only <- taper_map[c("species", "model")]
  by_name <- merchandise(
    12, 40, species = "Douglas-fir", taper_map = names_only,
    products = products, quiet = TRUE, status = TRUE
  )
  expect_identical(by_name$trees$model, "F00FW2W202")

  explicit <- merchandise(
    12, 40, model = "demo.paraboloid", products = products, species = 202L,
    taper_map = taper_map, quiet = TRUE, status = TRUE
  )
  expect_identical(explicit$trees$model, "demo.paraboloid")
  explicit_mapping <- assumptions(explicit)
  explicit_mapping <- explicit_mapping[
    explicit_mapping$assumption == "species_model", , drop = FALSE
  ]
  expect_identical(explicit_mapping$source, "caller_model")

  ignored_invalid_map <- merchandise(
    12, 40, model = "demo.paraboloid", products = products, species = 202L,
    taper_map = data.frame(unused = "invalid"), quiet = TRUE, status = TRUE
  )
  expect_identical(ignored_invalid_map$trees$model, "demo.paraboloid")

  invalid <- data.frame(spcd = 202L, model = "not.registered")
  expect_error(
    merchandise(
      12, 40, species = 202L, taper_map = invalid, products = products
    ),
    "unregistered treevolume model id"
  )
})

test_that("taper map validation reports every structural problem", {
  products <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 1, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  call_with_map <- function(taper_map, species = 202L) {
    merchandise(
      12, 40, species = species, taper_map = taper_map,
      products = products, quiet = TRUE, status = TRUE
    )
  }

  expect_error(call_with_map("not a data frame"), "must be a data frame")
  expect_error(
    call_with_map(data.frame(spcd = 202L)),
    "must contain model and either spcd or species"
  )
  expect_error(
    call_with_map(data.frame(model = "F00FW2W202")),
    "must contain model and either spcd or species"
  )
  for (bad_model in list(1, NA_character_, "")) {
    expect_error(
      call_with_map(data.frame(spcd = 202L, model = bad_model)),
      "must contain nonmissing registered model ids"
    )
  }
  expect_error(
    call_with_map(data.frame(
      spcd = 202L, species = "western hemlock", model = "F00FW2W202"
    )),
    "spcd and taper_map\\$species do not agree"
  )
  expect_error(
    call_with_map(data.frame(
      spcd = c(202L, 202L), model = c("F00FW2W202", "F00FW2W202")
    )),
    "may appear in taper_map only once"
  )
  expect_error(
    merchandise(
      12, 40,
      taper_map = data.frame(spcd = 202L, model = "F00FW2W202"),
      products = products, quiet = TRUE, status = TRUE
    ),
    "species is required.*use taper_map"
  )

  factor_map <- data.frame(spcd = 202L, model = factor("F00FW2W202"))
  mapped <- call_with_map(factor_map)
  expect_identical(mapped$trees$model, "F00FW2W202")
  explicit <- merchandise(
    12, 40, model = factor("demo.paraboloid"), products = products,
    quiet = TRUE, status = TRUE
  )
  expect_identical(explicit$trees$model, "demo.paraboloid")
})

test_that("unresolved and caller-product messages describe their assumptions", {
  expect_message(
    unresolved <- merchandise(
      12, 80, species = 263L,
      taper_map = data.frame(spcd = 202L, model = "F00FW2W202"),
      status = TRUE
    ),
    "taper-map model\\(s\\) none"
  )
  expect_identical(unresolved$trees$status, 404L)

  outside_product <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 1, diameter_basis = "ob",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ob",
    allow_lower_products = FALSE
  )
  expect_message(
    mapped <- merchandise(
      12, 40, species = 131L,
      taper_map = data.frame(spcd = 131L, model = "831CLKE131"),
      products = outside_product, status = TRUE
    ),
    "caller-supplied products.*Filled bark_ratio"
  )
  expect_identical(mapped$trees$status, 0L)
})

test_that("assumption helpers preserve typed empty and missing-species behavior", {
  expect_identical(
    merchandiser:::.mc_assumption_rows(character(), size = 0L),
    merchandiser:::.mc_empty_assumptions()
  )
  expect_identical(merchandiser:::.mc_species_names(NULL), character())
  expect_warning(
    merchandiser:::.mc_status_warning("helper", 404L),
    paste0(
      "helper\\(\\): model_unresolved \\[404\\] for 1 stem\\(s\\). ",
      "Species code\\(s\\): missing. Supply model or taper_map and try again."
    )
  )
})

test_that("explicit products and models continue to override defaults", {
  products <- .mc_legacy_product(
    "log", 1, lengths = 16L, min_sed = 1L, diameter_basis = factor("ib"),
    scale_rule = factor("cubic"), measurement_quantity = factor("cubic"),
    scale_unit = factor("ft3"), scale_bark_basis = factor("ib"),
    allow_lower_products = 0
  )
  result <- expect_silent(merchandise(
    12L, 60L, "demo.paraboloid", products, id = "tree-a",
    age = 25L, status = 1
  ))
  expect_identical(result$trees$id, "tree-a")
  expect_identical(result$trees$model, "demo.paraboloid")
  expect_true(all(result$logs$product == "log"))

  species_result <- merchandise(
    12L, 60L, "PSME", products, quiet = TRUE, status = TRUE
  )
  expect_identical(species_result$trees$model, "F00FW2W202")
  expect_identical(species_result$trees$status, 0L)
})

test_that("presets are visible, illustrative, and ordinary product tables", {
  listed <- product_presets()
  expect_identical(listed$name, c("pnw", "us_south", "douglas_fir"))
  expect_true(all(listed$status == "illustrative"))
  for (name in listed$name) {
    products <- product_preset(name)
    expect_s3_class(products, "merch_products")
    if (name == "douglas_fir") {
      expect_true(all(is.na(products$specification_source)))
    } else {
      expect_true(all(grepl("^illustrative_", products$specification_source)))
    }
    expect_identical(validate_products(products), products)
  }
  expect_error(product_preset("unknown"), "example_product_names")
})

test_that("species and ids accept their ordinary representations", {
  expect_identical(
    merchandiser:::.mc_resolve_species(c(202, 263L, NA_real_)),
    c(202L, 263L, NA_integer_)
  )
  expect_identical(
    merchandiser:::.mc_resolve_species(factor(c("202", "TSHE"))),
    c(202L, 263L)
  )
  expect_identical(
    merchandiser:::.mc_resolve_species(c("Douglas fir", "loblolly pine")),
    c(202L, 131L)
  )
  expect_identical(merchandiser:::.mc_resolve_species("202.0"), 202L)

  products <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 1, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib", allow_lower_products = FALSE
  )
  date_id <- as.Date("2026-09-09")
  result <- merchandise(
    12, 40, "demo.paraboloid", products, id = date_id, status = TRUE
  )
  expect_identical(result$trees$id, date_id)
  for (id in list(TRUE, as.raw(1L), 1 + 2i, factor("tree"))) {
    result <- merchandise(
      12, 40, "demo.paraboloid", products, id = id, status = TRUE
    )
    expect_identical(result$trees$id, id)
  }
})

test_that("an unresolved species stays aligned without a placeholder model", {
  result <- merchandise(
    c(12L, 12L), c(80L, 80L), species = c(202L, NA_integer_),
    quiet = TRUE, status = TRUE
  )
  expect_identical(result$trees$status, c(0L, 404L))
  expect_true(is.na(result$trees$model[[2L]]))
  expect_identical(result$trees$log_count[[2L]], 0L)
  expect_false(any(result$logs$id == 2L))
  expect_identical(result$diagnostics$id, 2L)
  expect_identical(result$diagnostics$spcd, NA_integer_)
  expect_identical(result$diagnostics$status, 404L)
  expect_identical(result$diagnostics$status_name, "model_unresolved")
})

test_that("an incomplete taper map warns once and names unresolved species", {
  products <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 1, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  taper_map <- data.frame(spcd = 202L, model = "F00FW2W202")
  warnings <- character()
  result <- withCallingHandlers(
    merchandise(
      c(12, 12), c(40, 40), products = products,
      species = c(202L, 263L), taper_map = taper_map, quiet = TRUE
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(
    warnings,
    paste0(
      "merchandise(): model_unresolved [404] for 1 stem(s). ",
      "Species code(s): 263. Supply model or taper_map and try again."
    )
  )
  expect_identical(nrow(result$logs[result$logs$id == 2L, ]), 0L)
})

test_that("single-call summaries and per-tree helpers are size stable", {
  direct <- merchandise(
    c(12L, 16L), c(80L, 90L), c("PSME", "Douglas-fir"),
    quiet = TRUE, status = TRUE
  )
  summary <- product_summary_by_tree(
    c(12L, 16L), c(80L, 90L), c("PSME", "Douglas-fir"), quiet = TRUE
  )
  expect_s3_class(summary, "data.frame")
  expect_true(all(
    c("product", "log_net_cubic_ib", "input_trees") %in% names(summary)
  ))
  expect_identical(
    summary,
    product_summary(direct, table = "logs")
  )

  volume <- tree_cubic_volume(
    c(12L, 16L), c(80L, 90L), 202L, quiet = TRUE
  )
  logs <- tree_log_count(
    c(12L, 16L), c(80L, 90L), 202L, quiet = TRUE
  )
  expect_type(volume, "double")
  expect_type(logs, "integer")
  expect_length(volume, 2L)
  expect_length(logs, 2L)
  expect_equal(volume, direct$trees$log_net_cubic_ib)
  expect_identical(logs, direct$trees$log_count)
  gross <- tree_cubic_volume(
    c(12L, 16L), c(80L, 90L), 202L, quiet = TRUE, basis = "gross"
  )
  expect_equal(
    gross,
    merchandiser:::.mc_sum_by_index(
      direct$logs$log_gross_cubic_ib,
      match(direct$logs$id, direct$trees$id), nrow(direct$trees)
    )
  )

  taper_map <- data.frame(spcd = 202L, model = "F00FW2W202")
  mapped_dbh <- c(12L, 16L)
  expect_equal(
    tree_cubic_volume(
      mapped_dbh, c(80L, 90L), c("PSME", "Douglas-fir"),
      taper_map = taper_map, quiet = TRUE
    ),
    volume
  )
  expect_identical(
    tree_log_count(
      mapped_dbh, c(80L, 90L), c("PSME", "Douglas-fir"),
      taper_map = taper_map, quiet = TRUE
    ),
    logs
  )
  expect_identical(
    product_summary_by_tree(
      mapped_dbh, c(80L, 90L), c("PSME", "Douglas-fir"),
      taper_map = taper_map, quiet = TRUE
    ),
    summary
  )
})

test_that("per-tree helpers warn when failed trees become missing values", {
  expect_warning(
    volume <- tree_cubic_volume(
      c(12L, 12L), c(80L, 80L), c(202L, NA_integer_), quiet = TRUE
    ),
    paste0(
      "tree_cubic_volume\\(\\): model_unresolved \\[404\\] for 1 stem\\(s\\). ",
      "Species code\\(s\\): missing. Supply model or taper_map and try again."
    )
  )
  expect_true(is.finite(volume[[1L]]))
  expect_true(is.na(volume[[2L]]))
  expect_silent(tree_cubic_volume(
    c(12L, 12L), c(80L, 80L), c(202L, NA_integer_),
    quiet = TRUE, status = TRUE
  ))
})

test_that("price scenarios compare fixed cuts with rebucking", {
  products <- product_preset("pnw")
  products$price <- c(10L, 8L, 2L)
  base <- merchandise(
    16, 90, 202, products = products, currency = factor("USD"),
    quiet = TRUE, status = TRUE
  )
  scenarios <- data.frame(
    scenario = factor(rep(c("saw_high", "pulp_high"), each = 3L)),
    product = rep(products$product, 2L),
    price = c(12L, 8L, 2L, 6L, 5L, 8L),
    stringsAsFactors = FALSE
  )
  comparison <- compare_bucking_prices(base, scenarios)
  expect_s3_class(comparison, "bucking_price_comparison")
  expect_identical(comparison$scenario, c("saw_high", "pulp_high"))
  expect_true(all(comparison$rebuck_gain >= -1e-10))
  rebucked <- attr(comparison, "rebucked_results")
  expect_length(rebucked, 2L)
  expected_repriced <- vapply(
    comparison$scenario,
    function(scenario) {
      table <- scenarios[scenarios$scenario == scenario, ]
      at <- match(base$logs$product, table$product)
      sum(base$logs$net_scale * table$price[at] /
            products$price_per[match(base$logs$product, products$product)])
    },
    numeric(1L)
  )
  expect_equal(comparison$repriced_value, unname(expected_repriced))
  expect_equal(
    comparison$rebucked_value,
    unname(vapply(rebucked, function(x) sum(x$values$net_value), numeric(1L)))
  )
})

test_that("compact prints and status lookup expose the common answer", {
  products <- product_preset("pnw")
  product_output <- capture.output(print(products))
  expect_match(product_output[[1L]], "<products>")
  expect_true(any(grepl("Illustrative preset", product_output)))
  subset_output <- capture.output(print(products[c("product", "priority")]))
  expect_true(any(grepl("priority", subset_output)))

  result <- merchandise(12, 80, "PSME", quiet = TRUE, status = TRUE)
  result_output <- capture.output(print(result))
  expect_match(result_output[[1L]], "<merch_result>")
  expect_true(any(grepl("Species-to-equation assumptions", result_output)))
  expect_true(any(grepl("caller_taper_map|nvel_default", result_output)))
  expect_true(any(grepl("Other recorded assumptions:", result_output)))
  expect_false(any(grepl("run_metadata", result_output, fixed = TRUE)))

  codes <- status_codes()
  expect_true(all(
    c("code", "name", "category", "description", "source") %in% names(codes)
  ))
  expect_true(all(c(0L, 400L, 404L, 410L) %in% codes$code))
  expect_false(anyNA(
    codes$description[codes$code %in% c(400L, 404L, 410L)]
  ))
})

test_that("status-free result printing does not call failed trees valid", {
  products <- .mc_legacy_product(
    "log", 1L, lengths = 8, min_sed = 0, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
  result <- suppressWarnings(merchandise(
    12, 40, "missing.model", products
  ))
  output <- capture.output(print(result))
  expect_true(any(grepl("Status detail omitted", output, fixed = TRUE)))
  expect_false(any(grepl("Valid trees:", output, fixed = TRUE)))
})
