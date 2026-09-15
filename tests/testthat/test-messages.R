test_that("recycling errors identify the argument and conflicting sizes", {
  expect_error(dib(c(12, 14), c(60, 70, 80), 20, 202), "dbh.*size 2.*size.*3")
  expect_error(dib(numeric(), c(60, 70), 20, 202), "dbh.*size 0.*size.*2")
  expect_error(check_taper_models(rep("F00FW2W202", 3), c(202, 263)),
    "spcd.*size 2.*size.*3"
  )
  expect_error(nvel_default_equation(c(6, 6), 12, c(0, 0, 0), 202),
    "region.*size 2.*size.*3"
  )
  expect_length(nvel_default_equation(c(6, 6), numeric(), 0, 202), 0)
  expect_error(stem_profile(1:2, rep(16, 3), 60, 202), "tree_id.*size 2.*size.*3")
  for (name in c("dib", "dob")) {
    args <- list(dbh = rep(16, 3), ht = 60, spcd = 202)
    args[[name]] <- c(6, 8)
    fn <- paste0("height_at_", name)
    expect_error(do.call(fn, args), paste0(name, ".*size 2.*size.*3"))
    args[[name]] <- "bad"
    expect_error(do.call(fn, args), paste0("^", name, " must be numeric"))
  }
  for (fn in c("stem_volume", "stem_profile")) {
    for (name in c("from", "to", "from_dib", "from_dob", "to_dib", "to_dob")) {
      args <- list(dbh = rep(16, 3), ht = 60, spcd = 202)
      if (fn == "stem_profile") args$tree_id <- 1:3
      args[[name]] <- c(6, 8)
      expect_error(do.call(fn, args), paste0(name, ".*size 2.*size.*3"))
    }
  }
  measurements <- as.list(example_stem_measurements[c("tree_id", "dbh", "ht", "h", "dib", "spcd")])
  for (name in c("spcd", "weights")) {
    args <- measurements
    args[[name]] <- c(1, 2)
    expect_error(do.call(fit_taper, args), paste0(name, ".*size 2.*size"))
  }
})

test_that("product errors name fields and list choices", {
  args <- as.list(hardening_product())
  args$spcd <- NULL
  args$max_length <- c(16, 32)
  expect_error(do.call(product, args), "max_length.*one value")
  args$max_length <- 16
  args$volume_unit <- "unknown"
  expect_error(do.call(product, args),
    "volume_unit.*scribner.*international.*doyle.*cubic.*green_ton.*cord"
  )
  args$volume_unit <- "cubic"
  args$round <- "unknown"
  expect_error(do.call(product, args), "round.*default.*down.*nearest.*none")
})

test_that("auxiliary names and coefficient errors use the public argument names", {
  expect_error(dib(16, 60, 20, 202, dhb = 16), "undeclared auxiliary input: dhb", fixed = TRUE)
  expect_error(merchandise(1, 16, 60, 202, hardening_product(), dhb = 16, quiet = TRUE),
    "undeclared auxiliary input: dhb", fixed = TRUE
  )
  expect_error(taper_model_from_coefficients("bad", "max_burkhart", c(a = 1)), "coefficients")
  expect_error(defects_from_stoppers(1, 60, "pulp", stump_ht = "a"), "stump_ht")
})

test_that("the catalog covers every new status and unknown codes have a name", {
  codes <- status_codes()
  expected <- c(`55` = "auxiliary_out_of_domain", `405` = "defect_unknown_tree",
    `407` = "defect_unknown_product", `408` = "invalid_pruned_height"
  )
  rows <- match(as.integer(names(expected)), codes$status)
  expect_identical(codes$name[rows], unname(expected))
  expect_identical(codes$category[rows], c("model", "defect", "defect", "input"))
  expect_true(all(nzchar(codes$description[rows])))
  expect_identical(.mc_status_name(c(0L, -999L)), c("ok", "unknown_status"))
  expect_identical(.mc_status_name(NA_integer_), "unknown_status")
})

test_that("coefficient models validate stump heights before unit conversion", {
  coefficients <- c(b1 = -3, b2 = 2, b3 = -1, b4 = 1, a1 = 0.7, a2 = 0.2)
  for (value in list("20", "a", NA_character_, character(), list(1, 2), TRUE)) {
    expect_error(
      taper_model_from_coefficients(
        "bad", "max_burkhart", coefficients, stump_ht = value
      ),
      "stump_ht"
    )
  }
})
