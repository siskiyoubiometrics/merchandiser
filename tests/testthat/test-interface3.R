test_that("shipped defaults resolve only species-specific complete models", {
  codes <- c(202L, 263L, 131L, 122L, 121L, 111L, 316L, 621L, 833L, 12L)
  expected <- c(
    "F00FW2W202", "F03FW2W263", "831CLKE131", "616BEHW122", "831CLKE121",
    "831CLKE111", "831CLKE316", "831CLKE621", "831CLKE833", "900CLKE012"
  )
  expect_identical(.mc_default_models(codes), expected)
  expect_identical(default_taper_models$spcd, as.integer(default_taper_models$spcd))
  for (i in seq_len(nrow(default_taper_models))) {
    expect_true(has_taper_model(default_taper_models$model[i]))
    model <- get_taper_model(default_taper_models$model[i])
    expect_identical(model$spcd, default_taper_models$spcd[i])
    expect_length(model$inputs$required, 0L)
  }
  for (i in seq_along(codes)) {
    expect_identical(
      dib(20, 100, 20, codes[i]),
      dib(20, 100, 20, codes[i], model = expected[i])
    )
  }
})

test_that("species without defaults return model_unresolved through every stem front door", {
  missing <- dplyr::anti_join(species_reference, default_taper_models, by = "spcd")$spcd[1]
  expect_identical(dib(20, 100, 20, missing)$status, 404L)
  expect_identical(dob(20, 100, 20, missing)$status, 404L)
  expect_identical(height_at_dib(20, 100, 6, missing)$status, 404L)
  expect_identical(height_at_dob(20, 100, 6, missing)$status, 404L)
  expect_identical(stem_volume(20, 100, missing)$status, 404L)
  expect_identical(stem_profile(1, 20, 100, missing)$status, 404L)
  expect_identical(defect_by_thirds(20, 100, missing, 0, 0, 0)$status, 404L)
  specification <- product(
    product = "saw",
    min_length = 16,
    max_length = 16,
    min_sed = 6,
    volume_unit = "cubic"
  )
  result <- merchandise(1, 20, 100, missing, specification, quiet = TRUE)
  expect_identical(result$status$status, 404L)
  expect_identical(result$status$name, "model_unresolved")
})

for (rule in c("scribner", "international", "doyle")) {
  for (rounding in c("default", "down", "nearest", "none")) {
    test_that(paste(rule, "pins scaling diameter and scale with", rounding, "rounding"), {
      specification <- product(
        product = "saw",
        min_length = 16,
        max_length = 16,
        min_sed = 6,
        max_logs = 1,
        volume_unit = rule,
        round = rounding
      )
      result <- merchandise(1, 24, 120, 202, specification, quiet = TRUE)
      diameter <- dib(24, 120, 17, 202)$value
      reported <- switch(rounding,
        down = floor(diameter),
        nearest = floor(diameter + 0.5),
        diameter
      )
      if (rule != "doyle")
        reported <- floor(reported + 0.5)
      expected_scale <- switch(rule,
        scribner = merchandiser:::.mc_scribner(floor(reported + 0.5), 16, TRUE),
        international = merchandiser:::.mc_intl14(floor(reported + 0.5), 16),
        doyle = pmax(reported - 4, 0)^2 * 16 / 16
      )
      expect_identical(result$logs$scaling_diameter, reported)
      expect_equal(result$logs$scale, expected_scale, tolerance = 1e-12)
    })
  }
}

test_that("assumptions distinguish default selection from caller selection", {
  specification <- product(
    product = "saw",
    min_length = 16,
    max_length = 16,
    min_sed = 6,
    volume_unit = "cubic"
  )
  default <- merchandise(1, 24, 120, 202, specification, quiet = TRUE)
  caller <- merchandise(1, 24, 120, 202, specification, model = "F00FW2W202")
  expect_identical(
    default$assumptions$source[default$assumptions$assumption == "species_model"],
    "species_default"
  )
  expect_identical(
    caller$assumptions$source[caller$assumptions$assumption == "species_model"],
    "caller_model"
  )
  expect_identical(default$logs, caller$logs)
  local_mocked_bindings(.mc_cut_batch = function(...) {
    list(list(
      logs = caller$logs[FALSE, ],
      residuals = caller$residuals[FALSE, ],
      status = 412L
    ))
  })
  failed <- merchandise(1, 24, 120, 202, specification, quiet = TRUE)
  expect_identical(failed$status$status, 412L)
  expect_identical(failed$status$name, "optimizer_failed")
  expect_identical(
    failed$status$description,
    "The value optimization did not complete for this tree."
  )
})

test_that("status names and county divisions retain explicit failure codes", {
  expect_identical(stem_volume(10, 0, 202)$status, 3L)
  expect_identical(.status_name(103L), "unknown_status")
  expect_identical(.status_name(-1L), "unknown_status")
  expect_false(406L %in% status_codes()$status)
  expect_identical(nsvb_division(2, 999), data.frame(value = NA_integer_, status = 8L))
  expect_identical(nsvb_division(NA_real_, 999)$status, 1L)
})

test_that("section endpoints are exclusive choices and profiles use feet", {
  expect_error(stem_volume(20, 100, 202, from = 1, from_dib = 10), "at most one")
  expect_error(stem_volume(20, 100, 202, to = 80, to_dob = 6), "at most one")
  expect_error(stem_profile(1, 20, 100, 202, from_dib = 10, from_dob = 12), "at most one")
  expect_error(stem_profile(1, 20, 100, 202, to_dib = 6, to_dob = 8), "at most one")
  profile <- stem_profile(1, 20, 100, 202)
  expect_identical(profile$h, seq(1, 100, by = 0.5))
  expect_identical(
    stem_volume(20, 100, 202),
    stem_volume(20, 100, 202, from = 1, to = 100)
  )
})

test_that("explicit section bounds reproduce the previous interface results on ten shipped trees", {
  ## Recorded from the old lower and upper interface before editing.
  expected <- list(
    section = data.frame(
      value = c(
        62.1302777935917234,
        9.3693170729258259,
        13.9600645795890088,
        2.5791634057535648,
        146.1070317088241097,
        8.8545258389886961,
        21.9264030565364791,
        20.0767437464441549,
        14.5247204695399716,
        NA
      ),
      status = c(
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        4L
      )
    ),
    dib = data.frame(
      value = c(
        167.10739054374496959,
        13.59189170496226673,
        23.15146213287168209,
        1.02987462705550592,
        421.97788355044826858,
        12.37112119565619928,
        42.86634613504055125,
        37.57498883485001073,
        24.45850899007982449,
        0.22376005494543094
      ),
      status = c(
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L
      )
    ),
    dob = data.frame(
      value = c(
        167.45002738387717045,
        14.06886070299324487,
        23.57331174244658811,
        1.91871406149793500,
        422.40909473320306233,
        12.80214171255808076,
        43.25064066482543979,
        37.95962048886682538,
        24.87743859930968426,
        0.75232067129232627
      ),
      status = c(
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L
      )
    )
  )
  trees <- head(example_trees_pnw, 10)
  section <- stem_volume(trees$dbh, trees$ht, trees$spcd, from = 10, to = 40)
  inside <- stem_volume(trees$dbh, trees$ht, trees$spcd, to_dib = 6)
  outside <- stem_volume(trees$dbh, trees$ht, trees$spcd, to_dob = 6)
  expect_equal(section, expected$section, tolerance = 1e-12)
  expect_equal(inside, expected$dib, tolerance = 1e-12)
  expect_equal(outside, expected$dob, tolerance = 1e-12)
})

test_that("vector taper fitting retains the old example coefficients", {
  ## Recorded with dput() before editing the data frame interface.
  expected <- c(
    b1 = -3.04959541906603,
    b2 = 1.53833926875488,
    b3 = -1.44261127351777,
    b4 = 25.7096391824546,
    a1 = 0.657163218696836,
    a2 = 0.0860377330533046
  )
  measurements <- example_stem_measurements
  fit <- fit_taper(
    tree_id = measurements$tree_id,
    dbh = measurements$dbh,
    ht = measurements$ht,
    h = measurements$h,
    dib = measurements$dib,
    spcd = measurements$spcd
  )
  expect_equal(fit$coefficients, expected, tolerance = 1e-8)
  expect_identical(fit$spcd, sort(unique(as.integer(measurements$spcd))))
  model <- as_taper_model(fit, "test.interface3")
  expect_identical(model$stump_ht, 0.3048)
  expect_identical(model$spcd, fit$spcd)
  expect_identical(model$form, "max_burkhart")
  expect_identical(model$class_version, "0.5")
})

test_that("explicit models and taper maps keep their precedence", {
  mapping <- data.frame(spcd = 121, model = "demo.paraboloid")
  expect_identical(.mc_default_models(121, taper_map = mapping), "demo.paraboloid")
  expect_identical(
    .mc_default_models(121, model = "831CLKE121", taper_map = mapping),
    "831CLKE121"
  )
})

test_that("fitted and coefficient models default to a one foot stump", {
  fit <- cached_taper_fit("max_burkhart")
  model <- as_taper_model(fit, "test.interface3.stump", bark_ratio = 0.9)
  supplied <- taper_model_from_coefficients(
    id = "test.interface3.coefficients",
    form = fit$form,
    coefficients = fit$coefficients
  )
  expect_identical(model$stump_ht, 0.3048)
  expect_identical(supplied$stump_ht, 0.3048)
  expect_identical(supplied$source, .published_taper_source(fit$form))
  register_taper_model(model)
  on.exit(unregister_taper_model(model$id), add = TRUE)
  default <- stem_volume(40 / 2.54, 25 / 0.3048, 202, model = model$id)
  explicit <- stem_volume(40 / 2.54, 25 / 0.3048, 202, model = model$id, from = 1)
  ground <- stem_volume(40 / 2.54, 25 / 0.3048, 202, model = model$id, from = 0)
  expect_identical(default, explicit)
  expect_lt(default$value, ground$value)
})
