test_that("regional wildcard identifiers resolve", {
  ids <- c(
    "A61CURW042", "A61DEMW242", "A09BRUW098",
    "400MATW202", "499MATW999"
  )
  expect_true(all(has_taper_model(ids)))
  expect_identical(get_taper_model(ids[[1L]])$family, "r10_taper")
  expect_identical(get_taper_model(ids[[4L]])$family, "r4_driver")

  unsupported <- dib(12, 80, 30, "499MATW999", status = TRUE)
  expect_identical(unsupported$status, 301L)
  expect_true(is.na(unsupported$value))
})

test_that("regional capability metadata and outside bark are explicit", {
  ids <- c("A16DEMW098", "400MATW202")
  capabilities <- model_capabilities(ids)
  expect_false(any(capabilities$has_dob))
  expect_true(all(capabilities$has_inverse))
  expect_true(all(capabilities$has_integral))

  missing <- dob(12, 80, 30, ids, status = TRUE)
  expect_identical(missing$status, c(53L, 53L))
  expect_true(all(is.na(missing$value)))
  inside <- dib(12, 80, 30, ids)
  approximated <- dob(12, 80, 30, ids, bark_ratio = 0.9)
  expect_equal(approximated, inside / 0.9, tolerance = 1e-14)
  expect_match(get_taper_model(ids[[1L]])$notes, "constant bark_ratio")
})

test_that("regional operations use compiled kernels", {
  ids <- c("A16DEMW098", "400MATW202")
  diameter <- dib(12, 80, 30, ids, status = TRUE)
  inverse <- height_at_dib(
    12, 80, diameter$value, ids, status = TRUE
  )
  volume <- stem_volume(
    12, 80, ids, lower = 0, lower_type = "height",
    upper = 0, upper_type = "tip", status = TRUE
  )
  expect_true(all(diameter$status == 0L))
  expect_true(all(inverse$status %in% c(0L, 102L)))
  expect_true(all(volume$status == 0L))
  expect_true(all(is.finite(volume$value) & volume$value > 0))
})
