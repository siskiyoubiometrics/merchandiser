test_that("Scribner reports the diameter at the source rule's final section end", {
  specification <- product(
    product = "log", min_length = 31.5, max_length = 31.5,
    min_sed = 0, trim = 0.5, max_logs = 1, volume_unit = "scribner"
  )
  result <-
    merchandise(
      tree_id = 1, dbh = 24, ht = 120, spcd = 202, products = specification,
      model = "F00FW2W202", quiet = TRUE
    )
  expect_identical(result$logs$scaling_length, 31)
  expect_identical(result$logs$scaling_diameter, 18)
  expect_identical(result$logs$scale, .mc_scribner(18, 31, TRUE))
})

test_that("sweep records retain the documented defect column order", {
  record <- defect(1, 1, 5, "sweep", percent = 20)
  expect_identical(names(record), c(
    "tree_id", "start_height", "end_height", "effect", "product",
    "percent"
  ))
  expect_identical(record$percent, 20)
  expect_true(is.na(record$product))
})

test_that("profile identifiers remain unique after input recycling", {
  expect_error(stem_profile(
    tree_id = 1,
    dbh = c(12, 24),
    ht = 80,
    spcd = 202,
    step = 40 / 12
  ), "tree_id must be unique")
})

test_that("species defaults do not depend on other trees in the call", {
  single <- dib(dbh = 12, ht = 60, h = 20, spcd = 131)
  batch <- dib(dbh = 12, ht = 60, h = 20, spcd = c(131, NA, 202))
  expect_identical(batch$value[1], single$value)
  expect_identical(batch$status, c(0L, 1L, 0L))
  expect_identical(.mc_default_models(c(131, NA, 202)), c(
    .mc_default_models(131),
    NA_character_,
    .mc_default_models(202)
  ))
})

test_that("fractional nominal lengths follow each product's scaling contract", {
  for (unit in c("scribner", "doyle", "international", "cubic")) {
    for (increment in c(1, 2)) {
      specification <- product("log",
        min_length = 31.5, max_length = 31.5,
        min_sed = 0,
        trim = 0.5,
        max_logs = 1, volume_unit = unit, length_round = increment
      )
      result <- merchandise(1, 24, 120, 202, specification,
        model = "F00FW2W202",
        quiet = TRUE
      )$logs
      scaled <- floor(31.5 / increment) * increment
      expect_identical(result$scaling_length, scaled)
      nominal_dib <- dib(24, 120, 32.5, 202, model = "F00FW2W202")$value
      expected <- switch(unit,
        scribner = .mc_scribner(
          floor(dib(24, 120, 1 +
                    scaled,
                  202,
                  model = "F00FW2W202"
                )$value +
                  0.5),
          scaled,
          TRUE
        ),
        doyle = pmax(
          nominal_dib -
            4,
          0
        )^2 *
          scaled /
          16,
        international = .mc_intl14(
          floor(
            nominal_dib +
              0.5
          ),
          scaled
        ),
        cubic = stem_volume(
          24,
          120,
          202,
          model = "F00FW2W202",
          from = 0,
          to = 32.5
        )$value -
          stem_volume(
            24,
            120,
            202,
            model = "F00FW2W202",
            from = 0,
            to = 1
          )$value
      )
      expect_equal(result$scale, expected, tolerance = 1e-12)
      if (unit == "cubic")
        expect_true(is.na(result$scaling_diameter))
    }
  }
  for (unit in c("scribner", "doyle", "international")) {
    expect_error(product("log",
                   min_length = 16, max_length = 32, min_sed = 0,
                   volume_unit = unit, length_round = 0.5
                 ), "length_round of 1 or 2")
  }
  expect_identical(.mc_nvel_segments(31, "whole_40", 1), 31)
  expect_identical(.mc_nvel_segments(41, "whole_40", 1), c(21, 20))
})

test_that("tree cubic volume measures to the tip or an inside bark top without products", {
  trees <- example_trees
  for (inside in c(TRUE, FALSE)) {
    actual <- stem_volume(
      trees$dbh,
      trees$ht,
      trees$spcd,
      model = trees$model,
      inside_bark = inside,
      stump_ht = 1,
      to_dib = 6
    )
    expected <- stem_volume(
      trees$dbh,
      trees$ht,
      trees$spcd,
      model = trees$model,
      inside_bark = inside,
      stump_ht = 1,
      to_dib = 6
    )
    expect_identical(actual, expected)
  }
  invalid <- stem_volume(c(24, NA), 120, 202)
  expect_identical(invalid$status, c(0L, 1L))
  expect_true(is.na(invalid$value[2]))
})
