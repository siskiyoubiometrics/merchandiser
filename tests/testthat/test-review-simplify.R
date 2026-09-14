test_that("measurement migration rejects invalid and competing bark bases", {
  for (measurement in list(list(volume_unit = "cubic"), list(sold_by = "cubic_ft_ob"))) {
    fields <- c(list(product = "saw", priority = 1, lengths = 16, min_sed = 4), measurement)
    for (basis in list("invalid", NA_character_, 1)) {
      expect_error(do.call(product, c(fields, list(diameter_basis = basis))),
                   "diameter_basis must contain ib or ob")
    }
    expect_error(do.call(product, c(fields, list(diameter_basis = "ib", inside_bark = FALSE))),
                 "Supply only inside_bark")
    p <- suppressMessages(do.call(product, c(fields, list(diameter_basis = factor("ob")))))
    expect_identical(p$inside_bark, FALSE)
  }
})

test_that("measurement flags accept only the documented logical and numeric values", {
  for (inside in c(0, 1)) for (split in c(0, 1)) {
    p <- product("saw", 1, lengths = 16, min_sed = 4, volume_unit = "scribner",
                 inside_bark = inside, split_scale = split)
    expect_identical(p$inside_bark, as.logical(inside))
    expect_identical(p$split_scale, as.logical(split))
    expect_identical(validate_products(p), p)
  }
  for (field in c("inside_bark", "split_scale")) {
    for (bad in list(NA, NA_real_, 2, -1, "TRUE")) {
      fields <- c(list(product = "saw", priority = 1, lengths = 16, min_sed = 4,
                       volume_unit = "scribner"), stats::setNames(list(bad), field))
      expect_error(do.call(product, fields), paste0(field, " must contain"))
    }
  }
})

test_that("authored legacy rounding cannot publish missing or discarded settings", {
  fields <- list(product = "saw", priority = 1, lengths = 16, min_sed = 4,
                 scale_rule = "doyle_formula", measurement_quantity = "board_foot",
                 scale_unit = "board_foot", scale_bark_basis = "ib", diameter_basis = "ib")
  for (override in list(list(diameter_round = "truncate_1cm"),
                        list(length_round = "truncate_1ft"),
                        list(volume_round = "nearest_10_board_feet_half_up"))) {
    expect_error(do.call(product, c(fields, override)),
                 "the product fields cannot express it.*report_also")
  }
  p <- do.call(product, c(fields, list(diameter_round = "truncate_1in")))
  expect_identical(p$round, "down")
  expect_identical(validate_products(p), p)
})
