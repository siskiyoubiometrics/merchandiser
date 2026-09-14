.merge_deprecations <- new.env(parent = emptyenv())

.merge_deprecated <- function(old, replacement) {
  if (!exists(old, envir = .merge_deprecations, inherits = FALSE)) {
    assign(old, TRUE, envir = .merge_deprecations)
    message(old, " is deprecated in merchandiser 0.2.0. Use ", replacement, ".")
  }
  invisible(NULL)
}

#' Deprecated names and measurement migration
#'
#' These names remain available for compatibility with earlier releases. Each emits a
#' message once per session naming its replacement. Update scripts to use
#' their replacements. Result tables and product fields use the current
#' names even when a deprecated function is called.
#'
#' @section Replacements:
#' \describe{
#'   \item{`merch_product`}{Use [product].}
#'   \item{`merch_products`}{Use [products].}
#'   \item{`preset`}{Use [example_products()].}
#'   \item{`list_presets`}{Use [example_product_names()].}
#'   \item{`merch_defect`}{Use [defect].}
#'   \item{`merch_summary`}{Use [product_summary].}
#'   \item{`merch_summary_by_product`}{Use [product_summary_by_tree].}
#'   \item{`merch_volume`}{Use [tree_cubic_volume].}
#'   \item{`merch_piece_count`}{Use [tree_log_count].}
#'   \item{`mc_status_codes`}{Use [status_codes].}
#'   \item{`tv_status_codes`}{Use [status_codes].}
#'   \item{`tv_models`}{Use [taper_models].}
#'   \item{`taper_model`}{Use [get_taper_model].}
#'   \item{`has_model`}{Use [has_taper_model].}
#'   \item{`check_model_ids`}{Use [check_taper_models].}
#'   \item{`taper_model_spec`}{Use [new_taper_model].}
#'   \item{`register_model`}{Use [register_taper_model].}
#'   \item{`unregister_model`}{Use [unregister_taper_model].}
#'   \item{`registry_manifest`}{Use [taper_manifest].}
#'   \item{`check_registry_manifest`}{Use [check_taper_manifest].}
#'   \item{`tv_species`}{Use [species_reference].}
#'   \item{`tv_threads`}{Use [threads].}
#' }
#'
#' @param ... Arguments passed to the replacement function.
#' @return The replacement function's return value. The species data alias
#'   returns [species_reference].
#' @name deprecated-names
#' @keywords internal
#' @section Renamed result tables and fields:
#' Results use `logs` and `trees`, replacing `pieces` and `stems`. The old
#' `table` argument labels are translated by [product_summary()] with a
#' message. Product construction translates `min_top_length` to
#' `min_boundary_length`, `continue_to_top` to `allow_lower_products`, and
#' `max_pieces` to `max_logs_per_segment`.
#'
#' Supplying both old and new names
#' is an error. `scale_rule = 'exact_profile'` becomes `'cubic'` in
#' [product()]. These translations do not rename columns in an arbitrary
#' imported data frame.
#'
#' Use current field names when importing tables.
#' @section Measurement migration:
#' Representable legacy `sold_by` values and expanded measurement fields
#' migrate with a deprecation message. The following table also applies to
#' `scale_rule`, `measurement_quantity`, `scale_unit`, and `scale_bark_basis`
#' combinations with the same meaning.
#'
#' | Legacy measurement | New fields or disposition |
#' |---|---|
#' | `scribner_decimal_c_whole_40`, `scribner_whole` | `scribner`, `split_scale = FALSE` |
#' | `scribner_decimal_c_split_20`, `scribner_split` | `scribner`, `split_scale = TRUE` |
#' | `international_1_4_4ft`, `international` | `volume_unit = 'international'` |
#' | `doyle_formula`, `doyle` | `volume_unit = 'doyle'` |
#' | `cubic_ft_ib`, `cubic_ft_ob`, `cubic_m_ib`, `cubic_m_ob` | `cubic`, matching bark and units |
#' | `green_short_ton`, `green_metric_ton` | `volume_unit = 'green_ton'`, matching call units |
#' | `cord_ib`, `cord_ob`, `cord` | `volume_unit = 'cord'`, matching bark |
#' | `cubic`, `green_ton` aliases | Same `volume_unit`, units follow the call |
#' | Smalian, Huber, factor Scribner, retired allocation | Stop, cannot express the procedure |
#' | Independent acceptance and cubic or cord bark bases | Stop, cannot express the combination |
#' | Custom rounding without an exact field equivalent | Stop, cannot express the override |
#' | Cubic or weight units opposite to the call units | Stop, cannot express the fixed unit |
#'
#' `diameter_basis = 'ib'` becomes `inside_bark = TRUE`, and `'ob'` becomes `FALSE`. Board-foot
#' and green-weight measurements use inside bark. Cubic and cord measurements must match
#' acceptance bark.
#'
#' Fixed legacy units survive construction, validation, and combination and are checked against
#' the merchandising call. The other defaults are `split_scale = FALSE` and `round =
#' 'default'`. Expanded board-foot diameter rounding `rule_default`, `truncate_1in`,
#' `nearest_1in_half_up`, and `none` maps to `default`, `down`, `nearest`, and `none`.
#'
#' Other diameter operators and custom length or volume rounding are rejected.
#'
#' Rejection names the setting, says the product fields cannot express it, and
#' points to the advanced reporting argument `report_also` for measurement forms
#' still available there. Named reporting definitions retain Smalian, Huber,
#' factor Scribner, fixed units, independent bark bases, and supported rounding
#' operators. The retired allocation is unavailable there. Reporting measures
#' existing cuts and does not change product acceptance or log selection.
#'
#' @section Status and missing values:
#' The function aliases use the replacement function's argument validation,
#' missing-value behavior, and result codes. Messages occur once per old name
#' per session. The `tv_species` data alias is an active data binding, so its
#' message appears when that name is first accessed.
#'
#' It returns the current
#' species reference, including missing properties.
#' @seealso [product()] for current product fields, [merchandise()] for current
#'   result tables, [species_reference] for the species data.
#' @usage
#'
#' ## Call signatures
#' merch_product(...)
#'
#' merch_summary(...)
#'
#' taper_model(...)
#'
#' tv_species
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the example product measurements
#' example_products(name = 'pnw') %>%
#'   select(product, priority, volume_unit, inside_bark)
NULL

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_product <- function(...) {
  .merge_deprecated("merch_product()", "product()")
  product(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_products <- function(...) {
  .merge_deprecated("merch_products()", "products()")
  products(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
preset <- function(...) {
  .merge_deprecated("preset()", "example_products()")
  example_products(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
list_presets <- function(...) {
  .merge_deprecated("list_presets()", "example_product_names()")
  product_presets(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_defect <- function(...) {
  .merge_deprecated("merch_defect()", "defect()")
  defect(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_summary <- function(...) {
  .merge_deprecated("merch_summary()", "product_summary()")
  product_summary(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_summary_by_product <- function(...) {
  .merge_deprecated("merch_summary_by_product()", "product_summary_by_tree()")
  product_summary_by_tree(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_volume <- function(...) {
  .merge_deprecated("merch_volume()", "tree_cubic_volume()")
  tree_cubic_volume(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
merch_piece_count <- function(...) {
  .merge_deprecated("merch_piece_count()", "tree_log_count()")
  tree_log_count(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
mc_status_codes <- function(...) {
  .merge_deprecated("mc_status_codes()", "status_codes()")
  status_codes(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
tv_status_codes <- function(...) {
  .merge_deprecated("tv_status_codes()", "status_codes()")
  status_codes(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
tv_models <- function(...) {
  .merge_deprecated("tv_models()", "taper_models()")
  taper_models(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
taper_model <- function(...) {
  .merge_deprecated("taper_model()", "get_taper_model()")
  get_taper_model(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
has_model <- function(...) {
  .merge_deprecated("has_model()", "has_taper_model()")
  has_taper_model(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
check_model_ids <- function(...) {
  .merge_deprecated("check_model_ids()", "check_taper_models()")
  check_taper_models(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
taper_model_spec <- function(...) {
  .merge_deprecated("taper_model_spec()", "new_taper_model()")
  new_taper_model(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
register_model <- function(...) {
  .merge_deprecated("register_model()", "register_taper_model()")
  register_taper_model(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
unregister_model <- function(...) {
  .merge_deprecated("unregister_model()", "unregister_taper_model()")
  unregister_taper_model(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
registry_manifest <- function(...) {
  .merge_deprecated("registry_manifest()", "taper_manifest()")
  taper_manifest(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
check_registry_manifest <- function(...) {
  .merge_deprecated("check_registry_manifest()", "check_taper_manifest()")
  check_taper_manifest(...)
}

#' @rdname deprecated-names
#' @export
#' @usage NULL
tv_threads <- function(...) {
  .merge_deprecated("tv_threads()", "threads()")
  threads(...)
}

#' @rdname deprecated-names
#' @name tv_species
#' @export tv_species
#' @usage NULL
NULL
