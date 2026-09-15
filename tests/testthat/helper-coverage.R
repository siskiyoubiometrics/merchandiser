# Synthetic input choices below define the test scenarios.
# Across coverage tests, a zero row count means an empty status table and a
# positive log count proves a nonempty cut. These expectations follow the public
# result contract in R/merchandise.R. Numeric status codes come from status_codes().
coverage_paraboloid <- function(id = "coverage.paraboloid") {
  model <- new_taper_model(
    id = id, form = "paraboloid",
    dib = function(dbh, ht, h, aux) {
      0.9 * dbh * sqrt((ht - h) / (ht - 4.5))
    },
    volume = function(dbh, ht, lower, upper, aux) {
      # Area = pi * (0.9 * dbh / 24)^2 * (ht - h) / (ht - 4.5).
      # Integrating ht - h gives ht * h - h^2 / 2, in cubic feet.
      pi * (0.9 * dbh / 24)^2 / (ht - 4.5) *
        (ht * (upper - lower) - (upper^2 - lower^2) / 2)
    }
  )
  register_taper_model(model)
  id
}

coverage_product <- function(...) {
  args <- utils::modifyList(list(
    product = "saw", min_length = 8, max_length = 16, min_sed = 0,
    volume_unit = "cubic"
  ), list(...))
  do.call(product, args)
}
