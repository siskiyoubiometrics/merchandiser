test_that("merchandise rejects a character auxiliary input instead of aborting", {
  specification <- products(saw = product("saw", min_length = 16, max_length = 32,
                                          min_sed = 6, volume_unit = "cubic"))
  expect_error(
    merchandise(1, 20, 100, 202, specification, quiet = TRUE, units = "metric"),
    "auxiliary input must be numeric: units"
  )
  expect_error(
    merchandise(1, 20, 100, 202, specification, quiet = TRUE, form_class = "80"),
    "auxiliary input must be numeric: form_class"
  )
})

test_that("defect accepts only the four effects", {
  expect_error(defect(1, 0, 8, "deduct", percent = 10), "cull, restrict, end, or sweep")
  expect_error(defect(1, 0, 8, NA_character_), "cull, restrict, end, or sweep")
  expect_s3_class(defect(1, 0, 8, "cull"), "merch_defects")
})

test_that("green_weight takes inside_bark as a logical", {
  inside <- green_weight(volume = 1, spcd = 202, inside_bark = TRUE)$value
  outside <- green_weight(volume = 1, spcd = 202, inside_bark = FALSE)$value
  expect_lt(outside, inside)
  expect_error(green_weight(volume = 1, spcd = 202, inside_bark = "outside"), "logical")
})
