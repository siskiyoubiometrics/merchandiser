test_that("boundary lengths reject negative and nonfinite values before normalization", {
  specification <- function(boundary) {
    product("review", 1, lengths = 16, min_sed = 6, diameter_basis = "ib",
            sold_by = "cubic_ft_ib", min_boundary_length = boundary)
  }
  for (boundary in c(-1, -Inf, Inf)) {
    expect_error(specification(boundary), "min_boundary_length must be finite")
    existing <- specification(8)
    existing$min_boundary_length <- boundary
    expect_error(validate_products(existing), "min_boundary_length must be finite")
  }
  expect_error(specification(0), "min_boundary_length may not normalize to zero")
  expect_true(is.na(specification(NA_real_)$min_boundary_length))
  expect_equal(specification(8)$min_boundary_length, 8)
})
