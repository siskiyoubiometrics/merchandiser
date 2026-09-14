acceptance_model_id <- "merch.acceptance.paraboloid"

register_acceptance_model <- function() {
  if (merchandiser::has_taper_model(acceptance_model_id)) {
    merchandiser::unregister_taper_model(acceptance_model_id)
  }
  inside <- function(dbh, ht, h, aux) {
    as.double(0.9 * dbh * sqrt((ht - h) / (ht - 4.5)))
  }
  outside <- function(dbh, ht, h, aux) {
    as.double(dbh * sqrt((ht - h) / (ht - 4.5)))
  }
  inverse <- function(dbh, ht, dib, aux) {
    as.double(ht - (dib / (0.9 * dbh))^2 * (ht - 4.5))
  }
  volume <- function(dbh, ht, lower, upper, aux) {
    as.double(
      pi * dbh^2 * 0.9^2 * ((ht - lower)^2 - (ht - upper)^2) /
        (1152 * (ht - 4.5))
    )
  }
  model <- merchandiser::new_taper_model(
    acceptance_model_id, "acceptance", inside, dob = outside,
    height_at_dib = inverse, volume = volume, units = "imperial",
    species = as.integer(c(131, 202)), stump_ht = 1, bark_ratio = 0.9,
    source = "SPEC_MERCHANDISER_INTERFACE.md-v1.1"
  )
  merchandiser::register_taper_model(model)
  invisible(acceptance_model_id)
}

unregister_acceptance_model <- function() {
  gc()
  if (merchandiser::has_taper_model(acceptance_model_id)) {
    merchandiser::unregister_taper_model(acceptance_model_id)
  }
  invisible(NULL)
}

acceptance_product <- function(product, priority, ...) {
  .mc_legacy_product(
    product, priority, ..., max_sweep = NA_character_,
    max_crook = NA_character_, max_defect_pct = NA_real_,
    scale_rule = "cubic", measurement_quantity = "cubic",
    scale_unit = "ft3", scale_bark_basis = "ib"
  )
}

acceptance_products_a <- function() {
  products(
    acceptance_product(
      "export", 1L, min_dbh = 12, lengths = as.double(c(32, 40)),
      trim = 1, min_sed = 12, diameter_basis = "ib",
      allow_lower_products = TRUE
    ),
    acceptance_product(
      "domestic_saw", 2L, fallback = TRUE,
      lengths = as.double(c(16, 20, 24, 32, 40)), min_boundary_length = 16,
      trim = 1, min_sed = 6, diameter_basis = "ib",
      allow_lower_products = TRUE
    ),
    acceptance_product(
      "pulp", 3L, fallback = TRUE, pulp_product = TRUE,
      min_length = 8, max_length = 40, length_step = 1,
      min_boundary_length = 8, trim = 0.5, min_sed = 3,
      diameter_basis = "ib", allow_lower_products = FALSE
    )
  )
}

acceptance_products_b <- function() {
  products(
    acceptance_product(
      "sawtimber", 1L, min_dbh = 12, min_length = 16,
      max_length = NA_real_, length_step = 1, min_boundary_length = 16,
      trim = 0.5, min_sed = 8, diameter_basis = "ob",
      max_logs_per_segment = 1L, allow_lower_products = TRUE
    ),
    acceptance_product(
      "chip_n_saw", 2L, min_dbh = 8, max_dbh = 12, fallback = FALSE,
      min_length = 16, max_length = NA_real_, length_step = 1,
      min_boundary_length = 16, trim = 0.5, min_sed = 6,
      diameter_basis = "ob", max_logs_per_segment = 1L,
      allow_lower_products = TRUE
    ),
    acceptance_product(
      "pulpwood", 3L, min_dbh = 5, max_dbh = 8, fallback = TRUE,
      pulp_product = TRUE, min_length = 8, max_length = NA_real_,
      length_step = 1, min_boundary_length = 8, trim = 0.5, min_sed = 3,
      diameter_basis = "ob", max_logs_per_segment = 1L,
      allow_lower_products = FALSE
    )
  )
}
