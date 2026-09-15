test_that("regional wildcard identifiers resolve", {
  ids <- c("A61CURW042", "A61DEMW242", "A09BRUW098", "400MATW202", "499MATW999")
  expect_true(all(has_taper_model(ids)))
  expect_identical(get_taper_model(ids[[1L]])$form, "r10_taper")
  expect_identical(get_taper_model(ids[[4L]])$form, "r4_driver")

  unsupported <- .oracle_dib(
    dbh = 12, ht = 80, h = 30, model = "499MATW999",
    spcd = .interface_spcd("499MATW999")
  )
  expect_identical(unsupported$status, 301L)
  expect_true(is.na(unsupported$value))
})

test_that("regional capability metadata and outside bark are explicit", {
  ids <- c("A16DEMW098", "400MATW202")
  capabilities <- .model_capabilities(ids)
  expect_false(any(capabilities$has_dob))
  expect_true(all(capabilities$has_inverse))
  expect_true(all(capabilities$has_integral))

  missing <- .oracle_dob(dbh = 12, ht = 80, h = 30, model = ids, spcd = .interface_spcd(ids))
  expect_identical(missing$status, c(53L, 53L))
  expect_true(all(is.na(missing$value)))
  inside <-
    .oracle_dib(dbh = 12, ht = 80, h = 30, model = ids, spcd = .interface_spcd(ids))$value
  approximated <- .oracle_dob(
    dbh = 12, ht = 80, h = 30, model = ids, bark_ratio = 0.9,
    spcd = .interface_spcd(ids)
  )$value
  expect_equal(approximated, inside / 0.9, tolerance = 1e-14)
  expect_match(get_taper_model(ids[[1L]])$notes, "constant bark_ratio")
})

test_that("regional operations use compiled kernels", {
  ids <- c("A16DEMW098", "400MATW202")
  diameter <- .oracle_dib(dbh = 12, ht = 80, h = 30, model = ids, spcd = .interface_spcd(ids))
  inverse <- .oracle_height_at_dib(
    dbh = 12, ht = 80, dib = diameter$value, model = ids,
    spcd = .interface_spcd(ids)
  )
  volume <- .oracle_stem_volume(
    dbh = 12, ht = 80, model = ids, lower = 0, lower_type = "height", upper = 0,
    upper_type = "tip", spcd = .interface_spcd(ids)
  )
  expect_true(all(diameter$status == 0L))
  expect_true(all(inverse$status %in% c(0L, 102L)))
  expect_true(all(volume$status == 0L))
  expect_true(all(is.finite(volume$value) & volume$value > 0))
})
