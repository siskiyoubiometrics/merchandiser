test_that("diameter, species scope, pruning, and division failures are visible", {
  expect_identical(dob(1e9, 60, 20, 202), dib(1e9, 60, 20, 202))
  args <- hardening_trees()
  args$spcd[3] <- 263
  result <- do.call(merchandise, args)
  expect_identical(result$status$tree_id, 3L)
  expect_identical(result$status$status, 52L)
  expect_true(3L %in% result$logs$tree_id)
  args <- hardening_trees()
  args$pruned_ht <- replace(rep(40, 6), 3, 61)
  args$products$requires_pruned <- TRUE
  result <- do.call(merchandise, args)
  expect_identical(result$status$status, 408L)
  expect_identical(result$status$tree_id, 3L)
  expect_setequal(unique(result$logs$tree_id), (1:6)[-3])
  mass <- biomass(rep(16, 6), 60, 202, division = c(0, 0, 999, 0, 0, 0))
  expect_identical(mass$status, replace(integer(6), 3, 8L))
  expect_true(is.na(mass$dry_stem_wood[3]))
  expect_true(all(is.finite(mass$dry_stem_wood[-3])))
})

test_that("profile required inputs cannot disappear silently", {
  base <- list(tree_id = 1, dbh = 16, ht = 60, spcd = 202)
  for (name in c("dbh", "ht", "model", "step")) {
    for (value in list(numeric(), character())) {
      args <- base
      args[name] <- list(value)
      expect_error(do.call(stem_profile, args), name)
    }
  }
  for (name in c("dbh", "ht")) {
    args <- base
    args[name] <- list(NULL)
    expect_error(do.call(stem_profile, args), name)
  }
})

test_that("third percentages require numeric inputs before arithmetic", {
  base <- list(dbh = 16, ht = 60, spcd = 202, lower = 0, middle = 0, upper = 0)
  for (name in c("dbh", "ht", "lower", "middle", "upper")) {
    for (value in list(TRUE, factor("10"), "10")) {
      args <- base
      args[[name]] <- value
      expect_error(do.call(defect_by_thirds, args), name)
    }
  }
})

test_that("green weight override limits are inclusive and named", {
  for (name in c("specific_gravity", "bark_specific_gravity", "moisture_pct",
                 "bark_moisture_pct", "bark_volume_pct")) {
    limit <- switch(name, moisture_pct = 300, bark_moisture_pct = 300,
                    bark_volume_pct = 100, 2)
    args <- list(volume = 10, spcd = 202)
    args[[name]] <- limit
    expect_identical(do.call(green_weight, args)$status, 0L)
    args[[name]] <- limit + 0.01
    expect_error(do.call(green_weight, args), name)
  }
})

test_that("measured height validation is truthful and nonfinite fit rows warn", {
  expect_warning(value <- complete_heights(16, c(-60, 0, 5, 60, 501), 202),
    "invalid_height.*NA returned"
  )
  expect_identical(value, c(NA_real_, NA_real_, 5, 60, NA_real_))
  trees <- example_trees_pnw
  fit <- function(value) {
    fit_height(trees$dbh, replace(trees$ht, 3, value), trees$spcd, form = "curtis")
  }
  expect_warning(missing <- fit(NA_real_), "missing or nonfinite.*omitted")
  expect_warning(infinite <- fit(Inf), "missing or nonfinite.*omitted")
  expect_identical(missing$data, infinite$data)
  expect_identical(missing$fixed_effects, infinite$fixed_effects)
})

test_that("explicit models do not bypass taper map validation", {
  for (model in list(NULL, "F00FW2W202")) {
    for (map in list("bad", data.frame(spcd = "202", model = "F00FW2W202"),
                     data.frame(spcd = 202, model = "bad"))) {
      expect_error(merchandise(1, 16, 60, 202, hardening_product(),
                     model = model, taper_map = map
                   ), "taper_map")
    }
  }
})

test_that("regional variants and FIA tops are validated before lookup", {
  for (region in 1:10) {
    expect_error(nvel_default_equation(region, 12, 0, 202, variant = "ZZ"),
      paste0("variant.*ZZ.*region ", region)
    )
    expect_type(nvel_default_equation(region, 12, 0, 202), "character")
  }
  expect_type(nvel_default_equation(1, 12, 0, 202, variant = "CI"), "character")
  expect_identical(nvel_default_equation(9, 12, 0, 202, variant = "SN"), "900CLKE202")
  expect_error(nvel_default_equation(6, 12, 0, 202, variant = "CI"), "variant.*region 6")
  expect_error(nvel_from_fia_code("BD000006", 202, primary_top = 1e9), "primary_top")
  expect_error(nvel_from_fia_code("BD000006", 202, primary_top = 99.01), "primary_top")
  expect_equal(nrow(nvel_from_fia_code("BD000006", 202, primary_top = c(0, 99))), 2)
})

test_that("NVEL rules are scalar and ordered", {
  defaults <- nvel_rules()
  for (name in names(defaults)) {
    value <- defaults[[name]]
    args <- stats::setNames(list(rep(value, 2)), name)
    expect_error(do.call(nvel_rules, args), name)
  }
  expect_error(nvel_rules(minimum_length = 40, maximum_length = 16),
    "minimum_length.*maximum_length"
  )
  expect_s3_class(nvel_rules(minimum_length = 16, maximum_length = 16), "treevolume_nvel_rules")
})

test_that("products must admit a positive tree and at least one log", {
  p <- as.list(hardening_product())
  p$spcd <- NULL
  for (name in c("max_logs", "max_dbh")) {
    args <- p
    args[[name]] <- 0
    expect_error(do.call(product, args), name)
  }
  expect_error(products(), "Supply at least one product.", fixed = TRUE)
})

test_that("defects keep range errors as statuses and normalize redundant culls", {
  p <- hardening_product()
  for (args in list(list(tree_id = list(1)), list(start_height = "1"),
                    list(end_height = "2"), list(effect = "rot"), list(product = 1),
                    list(percent = TRUE))) {
    base <- list(tree_id = 1, start_height = 1, end_height = 2, effect = "cull")
    base[names(args)] <- args
    expect_error(do.call(defect, base), names(args))
  }
  d <- defect(c(1, 1, 1), c(10, 15, 22), c(20, 25, 30), "cull")
  checked <- validate_defects(d, 1L, 60, p)
  expect_identical(checked$status, 0L)
  expect_identical(checked$start_height, 10)
  expect_identical(checked$end_height, 30)
  expect_message(duplicate <- validate_defects(rbind(d, d), 1L, 60, p), "exact duplicate")
  expect_identical(duplicate, checked)
  result <- merchandise(1, 16, 60, 202, p, defects = d, quiet = TRUE)
  culls <- result$residuals[result$residuals$cause == "cull", ]
  expect_identical(culls$start_height, 10)
  expect_identical(culls$end_height, 30)
  expect_error(validate_defects(defect("1", 10, 20, "cull"), 1, 60, p), "tree_id.*mode")
  expect_identical(validate_defects(defect(1, 10, 20, "cull"), 1L, 60, p)$status, 0L)
})

test_that("compiled models validate callback fields during registration", {
  model <- get_taper_model("F00FW2W202")
  model$id <- "hardening.bad.callback"
  model$dib <- "a"
  expect_error(register_taper_model(model), "dib.*function")
  expect_false(has_taper_model(model$id))
})

test_that("FIA identifiers have exactly eight characters", {
  for (code in c("BD0000061", "BD00000", " BD000006", "BD000006 ", "")) {
    expect_error(nvel_from_fia_code(code, 202), "fia_code.*eight")
  }
  expect_identical(nvel_from_fia_code("BD000006", 202)$errflag, 0L)
})

test_that("NVEL location codes require numeric whole numbers", {
  for (name in c("region", "forest", "district")) {
    for (value in list(TRUE, list(1), list(1, 2), 1.5, "1", factor("1"))) {
      args <- list(region = 6, forest = 12, district = 0, spcd = 202)
      args[[name]] <- value
      expect_error(do.call(nvel_default_equation, args), name)
      expect_error(do.call(nvel_rules, setNames(list(value), name)), name)
    }
  }
})

test_that("defect helpers respect physical height bounds", {
  for (height in c(-1, 0, 501, 1e9)) {
    result <- defect_by_thirds(16, c(60, height, 60), 202, 10, 0, 0,
      model = "F00FW2W202"
    )
    expect_identical(result$status, c(0L, 3L, 0L))
    expect_true(all(is.finite(result$value[c(1, 3)])))
  }
  for (name in c("ht", "saw_stop", "pulp_stop", "jump_butt", "stump_ht")) {
    for (height in c(-1, Inf, -Inf, NaN, 501)) {
      args <- list(tree_id = 1:3, ht = 60, topwood_product = "pulp")
      args[[name]] <- height
      expect_error(do.call(defects_from_stoppers, args), name)
    }
  }
  expect_s3_class(defects_from_stoppers(1, 60, "pulp", saw_stop = NA_real_),
    "merch_defects"
  )
})
