test_that("all fixture equations and Flewelling patterns resolve", {
  metadata <- utils::read.csv(
    system.file("extdata", "flewelling_models.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(metadata), 181L)
  expect_equal(sum(metadata$family == "flewelling_2pt"), 154L)
  expect_equal(sum(metadata$family == "flewelling_3pt"), 27L)
  expect_true(all(has_taper_model(metadata$id)))

  expect_true(has_taper_model("F09FW2W202"))
  expect_identical(get_taper_model("F09FW2W202")$form, "flewelling_2pt")
  expect_true(has_taper_model("A03F33W098"))
  expect_identical(get_taper_model("A03F33W098")$form, "flewelling_3pt")
})
test_that("three-point inputs and recommendation are frozen", {
  model <- get_taper_model("A00FW3W098")
  expect_identical(model$inputs$required, c("upper_ht1", "upper_d1"))
  expect_identical(model$inputs$optional, c(
    "upper_ht2", "upper_d2", "upper_bark",
    "bark_ratio"
  ))
  expect_match(model$notes, "two-point Flewelling form is recommended")

  missing <- .oracle_dib(
    dbh = 12, ht = 80, h = 30, model = "A00FW3W098",
    spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(missing$status, 51L)
  incomplete <- .oracle_dib(
    dbh = 12, ht = 80, h = 30, model = "A00FW3W098", upper_ht1 = 40,
    spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(incomplete$status, 51L)
  complete <- .oracle_dib(
    dbh = 12, ht = 80, h = 30, model = "A00FW3W098", upper_ht1 = 40, upper_d1 = 8,
    spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(complete$status, 0L)
  expect_true(is.finite(complete$value))

  incomplete_second <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "A00FW3W098", upper_ht1 = 30,
    upper_d1 = 9, upper_ht2 = 50, spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(incomplete_second$status, 51L)
  two_upper_points <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "A00FW3W098", upper_ht1 = 30,
    upper_d1 = 9, upper_ht2 = 50, upper_d2 = 6, upper_bark = "ib",
    spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(two_upper_points$status, 0L)
  expect_true(is.finite(two_upper_points$value))
  outside_bark_upper <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "A00FW3W098", upper_ht1 = 30,
    upper_d1 = 9, upper_bark = "ob", spcd = .interface_spcd("A00FW3W098")
  )
  expect_identical(outside_bark_upper$status, 53L)
})

test_that("geographic subregion characters select adjustment tables", {
  west_global <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "F00FW2W202",
    spcd = .interface_spcd("F00FW2W202")
  )$value
  west_coast <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "F01FW2W202",
    spcd = .interface_spcd("F01FW2W202")
  )$value
  ingy_global <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "I00FW2W122",
    spcd = .interface_spcd("I00FW2W122")
  )$value
  ingy_subregion <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = "I11FW2W122",
    spcd = .interface_spcd("I11FW2W122")
  )$value

  expect_false(isTRUE(all.equal(west_global, west_coast)))
  expect_false(isTRUE(all.equal(ingy_global, ingy_subregion)))
  expect_equal(
    .oracle_dib(
      dbh = 12, ht = 80, h = 40, model = "FOOFW2W202",
      spcd = .interface_spcd("FOOFW2W202")
    )$value,
    west_global,
    tolerance = 1e-14
  )
})

test_that("library failures become offset statuses", {
  identifiers <- sprintf("F%02dFW2W260", 0:8)
  result <- .oracle_dib(
    dbh = 12, ht = 80, h = 40, model = identifiers,
    spcd = .interface_spcd(identifiers)
  )
  expect_true(all(is.na(result$value)))
  expect_true(all(result$status == 301L))
})

test_that("Flewelling operations dispatch through the compiled registry", {
  id <- "F00FW2W202"
  at_height <-
    .oracle_dib(dbh = 12, ht = 80, h = 30, model = id, spcd = .interface_spcd(id))$value
  outside <-
    .oracle_dob(dbh = 12, ht = 80, h = 30, model = id, spcd = .interface_spcd(id))$value
  expect_true(is.finite(at_height))
  expect_gt(outside, at_height)
  expect_equal(
    .oracle_height_at_dib(
      dbh = 12, ht = 80, dib = at_height, model = id,
      spcd = .interface_spcd(id)
    )$value,
    30,
    tolerance = 1e-04
  )
  expect_equal(
    .oracle_height_at_dob(
      dbh = 12, ht = 80, dob = outside, model = id,
      spcd = .interface_spcd(id)
    )$value,
    30,
    tolerance = 1e-04
  )
  expect_true(is.finite(.oracle_stem_volume(
    dbh = 12, ht = 80, model = id,
    spcd = .interface_spcd(id)
  )$value))

  profile <- stem_profile(
    dbh = 12,
    ht = 40,
    model = id,
    step = 5,
    spcd = .interface_spcd(id),
    tree_id = seq_along(rep_len(12, max(length(12), length(40))))
  )
  expect_true(nrow(profile) > 2L)
  expect_true(all(profile$status %in% c(0L, 102L)))
})
