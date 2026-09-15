test_that("default equation lookup rejects character species codes", {
  for (spcd in list("Douglas-fir", "PSME", "202", character(), NA_character_)) {
    expect_error(
      nvel_default_equation(
        region = 6,
        forest = 12,
        district = 0,
        spcd = spcd
      ),
      "spcd must contain numeric species codes",
      fixed = TRUE
    )
  }
  expect_identical(
    nvel_default_equation(
      region = 6,
      forest = 12,
      district = 0,
      spcd = c(202, NA_real_)
    ),
    c("F00FW2W202", NA_character_)
  )
  expect_identical(
    nvel_default_equation(
      region = 6,
      forest = 12,
      district = 0,
      spcd = numeric()
    ),
    character()
  )
})
