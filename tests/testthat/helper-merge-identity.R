merge_identity_calls <- function() {
  model <- "F00FW2W202"
  saw <- .mc_legacy_product(
    "saw", 1, lengths = c(16, 20), trim = 0.5,
    min_sed = 6, diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib"
  )
  pulp <- .mc_legacy_product(
    "pulp", 2, lengths = c(8, 12), min_sed = 3, diameter_basis = "ib",
    scale_rule = "cubic", measurement_quantity = "cubic", scale_unit = "ft3",
    scale_bark_basis = "ib", pulp_product = TRUE
  )
  logs <- function(result) {
    result$logs[c("start_height", "end_height", "nominal_length",
                  "log_net_cubic_ib", "gross_scale", "net_scale")]
  }
  scale <- function(rule, quantity = "board_foot", unit = "board_foot") {
    specification <- saw
    specification$scale_rule <- rule
    specification$measurement_quantity <- quantity
    specification$scale_unit <- unit
    logs(merchandise(16, 90, model, specification, spcd = 202, status = TRUE))
  }
  list(
    volume_inside = stem_volume(12, 80, model),
    volume_outside = stem_volume(16, 90, model, bark = "outside"),
    volume_bounds = stem_volume(16, 90, model, lower = 10, lower_type = "height",
                                upper = 50, upper_type = "height"),
    volume_metric = stem_volume(30, 25, model, units = "metric"),
    profile = stem_profile(12, 80, model, step = 10, status = TRUE),
    inside_diameters = dib(c(12, 16), c(80, 90), c(20, 40), model),
    outside_diameters = dob(c(12, 16), c(80, 90), c(20, 40), model),
    inside_heights = height_at_dib(c(12, 16), c(80, 90), c(6, 8), model),
    outside_heights = height_at_dob(c(12, 16), c(80, 90), c(6, 8), model),
    biomass = biomass(12, 80, 202, 242, status = TRUE),
    carbon = co2e(12, 80, 202, 242, status = TRUE),
    green_weight = green_weight(c(10, 20), 202, status = TRUE),
    dry_weight = green_weight(c(10, 20), 202, moisture = "dry", status = TRUE),
    cascade = logs(merchandise(c(12, 16), c(80, 90), model,
                               products(saw, pulp), spcd = 202, status = TRUE)),
    optimizer = logs(optimize_bucking(16, 90, model, saw, spcd = 202,
                                      objective = "net_cubic_ib", status = TRUE)),
    doyle = scale("doyle_formula"),
    scribner = scale("scribner_decimal_c_split_20"),
    international = scale("international_1_4_4ft"),
    smalian = scale("smalian", "cubic", "ft3"),
    huber = scale("huber", "cubic", "ft3")
  )
}
