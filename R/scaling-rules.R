#' Scaling rules
#'
#' Products carry `volume_unit`, `inside_bark`, `split_scale`, and `round`. These fields select
#' a measurement procedure without changing log acceptance lengths or creating physical cuts.
#' Additional measurements belong in `report_also` in [merchandise()] and [optimize_bucking()].
#' @name scaling_rules
#' @details `volume_unit` is required.
#'
#' Use `'scribner'`, `'international'`, `'doyle'`, `'cubic'`, `'green_ton'`, or `'cord'`. Cubic
#' measurements use cubic feet for imperial calls and cubic meters for metric calls. Green
#' weight uses short tons for imperial calls and metric tons for metric calls.
#'
#' `inside_bark` defaults to `TRUE`. It sets the bark basis for physical small and large end
#' diameter limits and integrated cubic volume. Board foot rules always measure inside bark.
#'
#' Green weight always includes wood and attached bark. The field has no effect on those
#' measurements, but still controls their diameter acceptance limits. Cord uses integrated
#' volume on the chosen bark basis and requires `cord_solid_fraction` strictly between zero and
#' one.
#'
#' `split_scale` defaults to `FALSE`. For Scribner, false selects whole-log scaling and true
#' selects the shorter scaling sections with taper evaluated at their ends. Sections are
#' measurements within a log, not additional cuts.
#'
#' International always uses its prescribed short sections. Doyle measures the whole nominal
#' log. This flag has no effect for those or for integrated volume, green weight, and cord.
#'
#' `round` defaults to `'default'`, retaining the procedure's prescribed rounding. `'down'`
#' truncates modeled scaling diameter to the whole inch. `'nearest'` rounds to the nearest
#' whole inch with halves upward.
#'
#' `'none'` passes the modeled diameter to the procedure. Metric diameters are converted to
#' inches for this preprocessing. Scribner and International then apply their internal
#' rounding.
#'
#' An already whole diameter stays whole. Integrated volume, green weight, and cord have no
#' single scaling diameter, so this field has no effect on their measurement.
#'
#' Scaling uses the nominal body and excludes trim. Acceptance tests the physical
#' endpoints, including trim. Doyle retains its existing nominal-end diameter.
#'
#' Located rot deductions use the physical inside-bark volume fraction. Later
#' percentages compound on the current net quantities. Net board feet can be
#' fractional after deductions even when the gross scale is rounded.
#' @section Measurement mapping:
#' Product validation uses the combinations in `.mc_measurement_map()`. Each row
#' accepts every `round` choice listed in the corresponding section. The preprocessing choice is
#' applied before the listed procedure.
#'
#' `inside_bark` always retains its
#' acceptance meaning even when the measurement basis is fixed.
#' @evalRd local({
#' mapping <- .mc_measurement_map()
#' mapping <- unique(mapping[c("volume_unit", "inside_bark", "split_scale",
#'                              "scale_rule", "scale_bark_basis")])
#' labels <- c(scribner_decimal_c_whole_40 = "Scribner whole logs, up to 40 feet",
#'             scribner_decimal_c_split_20 = "Scribner sections, up to 20 feet",
#'             international_1_4_4ft = "International quarter-inch, 4-foot sections",
#'             doyle_formula = "Doyle whole log", cubic = "Integrated nominal profile")
#' mapping$scale_rule <- unname(labels[mapping$scale_rule])
#' mapping$scale_rule[mapping$volume_unit == "green_ton"] <- "Integrated wood and bark weight"
#' mapping$scale_rule[mapping$volume_unit == "cord"] <- "Integrated profile converted to cords"
#' mapping$scale_bark_basis[mapping$volume_unit == "green_ton"] <- "wood_bark"
#' mapping$scale_bark_basis <- ifelse(mapping$scale_bark_basis == "ib", "Inside", "Outside")
#' mapping$scale_bark_basis[mapping$volume_unit == "green_ton"] <- "Wood and bark"
#' rows <- apply(mapping, 1, paste, collapse = " \\tab ")
#' paste0("\\section{Measurement combinations}{\n\\tabular{lllll}{\n",
#'        "Volume unit \\tab Inside bark \\tab Split scale \\tab Procedure",
#'        " \\tab Measurement basis \\cr\n", paste(rows, collapse = " \\cr\n"), "\n}}")
#' })
#' @section Additional reporting:
#' Supply `report_also` to measure the selected cuts by another procedure. Character values include
#' `'smalian'`, `'huber'`, `'international'`, and
#' `'doyle'`. Both evaluate the nominal log body.
#'
#' Cubic reporting follows the call units. Scribner
#' factor approximations are additional reporting procedures only.
#'
#' Named definitions use `name`, `scale_rule`, and `scale_bark_basis`, with
#' optional `measurement_quantity`, `scale_unit`, and `cord_solid_fraction`. Board foot quantities
#' use `'board_foot'`. Cubic quantities use `'cubic'`
#' with `'ft3'` or `'m3'`.
#'
#' Green weight uses `'green_weight'` with
#' `'green_short_ton'` or `'green_metric_ton'`. Cord uses `'cord'`. The additional definition can
#' carry `diameter_round`, `length_round`, and
#' `volume_round` settings.
#'
#' These fields are not product columns. Additional factor procedures are
#' `'scribner_factor_whole_40'` and
#' `'scribner_factor_split_20'`. They use the corresponding Scribner sections
#' and source diameter rounding, but omit Decimal C exception corrections.
#'
#' The factor result rounds to whole board feet with halves upward.
#'
#' Advanced `diameter_round` values are `'rule_default'`, `'none'`,
#' `'truncate_1in'`, `'nearest_1in_half_up'`, `'nearest_0.5in_half_up'`,
#' `'truncate_1cm'`, and `'nearest_1cm_half_up'`. Advanced `length_round`
#' values are `'rule_default'`, `'none'`, `'truncate_1ft'`,
#' `'nearest_1ft_half_up'`, `'truncate_0.1m'`, and `'nearest_0.1m_half_up'`. Advanced
#' `volume_round` values are `'rule_default'`, `'none'`,
#' `'truncate_board_foot'`, `'nearest_board_foot_half_up'`, and
#' `'nearest_10_board_feet_half_up'`.
#'
#' A truncation rounds down to its named
#' increment. Integrated volume rejects dimension rounding. Cubic procedures
#' reject board foot output rounding.
#'
#' Scribner and International retain their
#' prescribed length and output rounding.
#' @section Migration:
#' The previous `sold_by` field is accepted with a deprecation message naming
#' `volume_unit`, `inside_bark`, and `split_scale`. `diameter_basis` is replaced
#' by `inside_bark`. Extra measurement procedures belong in `report_also`.
#' @section Status and missing values:
#' Invalid measurement fields stop product validation. Missing species or
#' unavailable weight properties leave weight missing. Inspect [status_codes()]
#' before interpreting missing quantities.
#'
#' Missing measurements are not zeros.
#' @return Product measurements are `gross_scale` and `net_scale` in the log
#' table, accompanied by `volume_unit`. Additional measurements are rows of
#' `scales`, with `is_product` distinguishing the assigned product measurement.
#' @seealso [product_schema] for acceptance fields, [result_tables] for returned
#' quantities, [green_weight()] for species properties and weight assumptions.
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Measure the same cuts by additional procedures
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       species = example_trees$species,
#'                       report_also = 'huber',
#'                       status = TRUE)
#'
#' ## Inspect additional cubic measurements
#' result$scales %>%
#'   filter(!is_product) %>%
#'   slice_head(n = 3)
NULL
