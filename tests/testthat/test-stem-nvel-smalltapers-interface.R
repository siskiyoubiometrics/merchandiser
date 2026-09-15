test_that("all regional small-taper identifiers and patterns resolve", {
  metadata <- utils::read.csv(
    system.file("extdata", "smalltaper_models.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(metadata), 80L)
  expect_identical(as.integer(table(metadata$family)), unname(c(
    behre_taper = 2L, blm_taper = 34L,
    r12_taper = 4L, r1_taper = 6L, r2_taper = 16L, r5_taper = 18L
  )))
  expect_true(all(has_taper_model(metadata$id)))
  representatives <- c(
    "101JB2W108", "201CZ2W015", "201CZ3W015", "501WO2W202", "H02SN2W301",
    "B04BEHW202", "632BEHW231"
  )
  expect_true(all(has_taper_model(representatives)))
  expect_false(any(has_taper_model(c(
    "201CZ2W122", "204CZ3W122", "I16BEHW000",
    "700BEHW231"
  ))))
  expect_true(all(has_taper_model(c(
    "201CZ2W015", "628BEHW231", "616BEHW202",
    "B01BEHW122"
  ))))
  expect_identical(unname(vapply(
    representatives, function(id) get_taper_model(id)$form,
    character(1)
  )), unname(c(
    "r1_taper", "r2_taper", "r2_taper", "r5_taper", "r12_taper",
    "blm_taper", "behre_taper"
  )))
})

test_that("R6 Behre volume does not double-count an exact top section", {
  dbh <- 6.9
  ht <- 49.9
  form_class <- 80
  d17 <- form_class / 100 * dbh
  expected <- 0.00272708 * (dbh^2 + d17^2) * 17.3 + 0.00272708 * (d17^2 + 4^2) *
    16.3 + 0.00272708 *
    4^2 * 16.3
  actual <- .oracle_stem_volume(
    dbh = dbh, ht = ht, model = "616BEHW231", lower = 0, lower_type = "height",
    upper_type = "tip", form_class = form_class, spcd = .interface_spcd("616BEHW231")
  )$value
  expect_equal(actual, expected, tolerance = 1e-14)
})

test_that("R6 Behre 632 taper uses the source 32.6-foot segment", {
  dbh <- 12
  ht <- 80
  height <- 40
  form_class <- 80
  h1 <- ht - 32.6 - 1
  ratio <- (ht - height) / h1
  expected <- dbh * form_class / 100 * ratio / (0.62 * ratio + 0.38)

  expect_equal(.oracle_dib(
    dbh = dbh, ht = ht, h = height, model = "632BEHW231", form_class = form_class,
    spcd = .interface_spcd("632BEHW231")
  )$value, expected, tolerance = 1e-14)
})

test_that("BLM total volume applies the source height adjustment", {
  dbh <- 12
  ht <- 80
  profile_ht <- ht + 1.5
  form_class <- 80
  d17 <- floor(dbh * form_class / 100 + 0.5)
  coefficient <- 0.6014
  diameter <- function(height) {
    ratio <- (profile_ht - height) / (profile_ht - 17.8)
    d17 * ratio / (coefficient * ratio + 1 - coefficient)
  }
  section_heights <- seq(1, 81, by = 4)
  section_diameters <- vapply(section_heights, diameter, numeric(1))
  expected <- 3.1416 * (section_diameters[[1]] / 2)^2 / 144 + sum(0.00272708 * head(
    section_diameters,
    -1
  )^2 * 4) + sum(0.00272708 * tail(section_diameters, -1)^2 * 4) + 0.00272708 * tail(
    section_diameters,
    1
  )^2 * 0.5
  actual <- .oracle_stem_volume(
    dbh = dbh, ht = ht, model = "B00BEHW011", lower = 0, lower_type = "height",
    upper_type = "tip", form_class = form_class, spcd = .interface_spcd("B00BEHW011")
  )$value
  expect_equal(actual, expected, tolerance = 1e-14)
})

test_that("both BLM short-tree predicates use the source double-bark cylinder", {
  actual <- .oracle_stem_volume(
    dbh = 3, ht = c(15, 20), model = "B00BEHW011", lower = 0, lower_type = "height",
    upper_type = "tip", form_class = 80, spcd = .interface_spcd("B00BEHW011")
  )
  dbh_ib <- 0.86951 * 3^1.00983
  expected <- 0.00272708 * dbh_ib^2 * (c(15, 20) + 1.5)

  expect_identical(actual$status, integer(2L))
  expect_equal(actual$value, expected, tolerance = 1e-15)
})

test_that("BLM taper-equation dispatch preserves the source forest guards", {
  actual <- .oracle_stem_volume(
    dbh = 3, ht = 15, model = c("B02BEHW122", "B03BEHW015"), lower = 0,
    lower_type = "height", upper_type = "tip", form_class = 80, spcd = .interface_spcd(c(
      "B02BEHW122",
      "B03BEHW015"
    ))
  )
  expected <- rep(0.00272708 * (3 / 1.071)^2 * (15 + 1.5), 2L)

  expect_identical(actual$status, integer(2L))
  expect_equal(actual$value, expected, tolerance = 1e-15)

  missing_bark <- .oracle_stem_volume(
    dbh = 3, ht = 15, model = "B04BEHW202", lower = 0, lower_type = "height",
    upper_type = "tip", form_class = 80, spcd = .interface_spcd("B04BEHW202")
  )
  expect_identical(missing_bark$status, 314L)
  expect_true(is.na(missing_bark$value))
  expect_silent(.oracle_stem_volume(
    dbh = 3, ht = 15, model = "B04BEHW202", lower = 0, lower_type = "height",
    upper_type = "tip", form_class = 80, spcd = .interface_spcd("B04BEHW202")
  )$value)
})

test_that("dynamic Behre defaults cover all regional source tables", {
  fixture <- utils::read.csv(testthat::test_path(
    "fixtures",
    "behre_default_oracle.csv"
  ), stringsAsFactors = FALSE)
  actual <- .oracle_dib(
    dbh = fixture$DBHOB, ht = fixture$HTTOT, h = fixture$HTUP, model = fixture$VOLEQ,
    spcd = .interface_spcd(fixture$VOLEQ)
  )
  expect_setequal(unique(fixture$GROUP), c("bm", "ca", "ec", "ni", "pn", "so", "wc"))
  expect_identical(actual$status, fixture$ERRFLAG)
  expect_equal(actual$value, fixture$DIB, tolerance = 1e-13)

  expect_equal(
    .oracle_dib(
      dbh = 12, ht = 80, h = 40, model = "B00BEHW202",
      spcd = .interface_spcd("B00BEHW202")
    )$value,
    .oracle_dib(
      dbh = 12, ht = 80, h = 40, model = "B00BEHW202", form_class = 80,
      spcd = .interface_spcd("B00BEHW202")
    )$value,
    tolerance = 0
  )
})

test_that("regional auxiliary metadata follows the source entry points", {
  cz3 <- get_taper_model("200CZ3W202")
  expect_identical(cz3$inputs$required, character())
  expect_identical(cz3$inputs$optional, c("upper_ht1", "upper_d1", "bark_ratio"))
  expect_identical(cz3$inputs$pairs, list(c("upper_ht1", "upper_d1")))
  expect_identical(
    .oracle_dib(
      dbh = 12, ht = 80, h = 40, model = cz3$id, spcd = .interface_spcd(cz3$id)
    )$status,
    309L
  )
  expect_identical(.oracle_dib(
    dbh = 12, ht = 80, h = 40, model = cz3$id, upper_ht1 = 4, upper_d1 = 8,
    spcd = .interface_spcd(cz3$id)
  )$status, 310L)
  expect_true(is.finite(.oracle_dib(
    dbh = 12, ht = 80, h = 40, model = cz3$id, upper_ht1 = 30, upper_d1 = 9,
    spcd = .interface_spcd(cz3$id)
  )$value))

  blm <- get_taper_model("B00BEHW202")
  expect_identical(blm$inputs$optional, c("form_class", "bark_ratio"))
  expect_false(blm$kernel$has_dob)
  expect_identical(
    .oracle_dob(
      dbh = 12, ht = 80, h = 40, model = blm$id, spcd = .interface_spcd(blm$id)
    )$status,
    53L
  )
  expect_true(is.finite(.oracle_dob(
    dbh = 12, ht = 80, h = 40, model = blm$id,
    bark_ratio = 0.9, spcd = .interface_spcd(blm$id)
  )$value))
  expect_identical(.oracle_stem_volume(
    dbh = 12, ht = 80, model = blm$id, lower = 0, lower_type = "height",
    upper_type = "tip", spcd = .interface_spcd(blm$id)
  )$status, 302L)
})
