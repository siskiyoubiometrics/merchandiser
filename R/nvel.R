#' Turn a source rule record into product specifications
#'
#' Translate a complete source-rule record into product specifications, stump height, a
#' utilization cap, and model auxiliaries.
#'
#' @param x Required `treevolume_nvel_rules` list from [nvel_rules()], with
#'   exactly one value per field listed in the corresponding section. No default. Omission,
#'   incomplete required merchant values, or more than one record is an error.
#'
#' Supply imperial source lengths and diameters, because product construction
#'   initially uses inches and feet.
#'
#' @return A list containing `products`, `stump_ht`, `utilization_height`, and
#'   `model_aux`.
#' @export
#' @usage
#'
#' ## Call signatures
#' products_from_nvel_rules(x)
#' @examples
#' ## Record complete example source rules
#' rules <- nvel_rules(even_or_odd = 2,
#'                     option = 14,
#'                     maximum_length = 40,
#'                     minimum_length = 8,
#'                     minimum_top_length = 8,
#'                     merchantable_length = 80,
#'                     primary_top = 6,
#'                     secondary_top = 4,
#'                     stump = 1,
#'                     trim = 0.5,
#'                     minimum_board_foot_dbh = 1)
#'
#' ## Translate the source record
#' translated <- products_from_nvel_rules(x = rules)
#'
#' ## Inspect the translated product specifications
#' translated$products
#' @section Source rule fields:
#' Each field has exactly one value in this translation.
#' \describe{
#' \item{`even_or_odd`}{National Volume Estimator Library `EVOD` segment-length code
#'   (unitless): `1` permits
#'   odd nominal lengths and `2` permits only even nominal lengths. Options 11
#'   through 14 define their own fixed-length behavior. `NA` requests the
#'   library default.
#'
#' Numeric nonempty vector. Accepted finite values are 1 or 2. Default `NA` applies when omitted.
#'
#' Numeric `NA` leaves a source
#'   default unset. Infinite values are errors. }
#' \item{`option`}{source library `OPT` segmentation code (unitless). Codes 11, 12, 13,
#'   and 14 use the library's 16-, 20-, 32-, and 40-foot scale conventions. Codes 21 through 24
#' select the four nominal-log top-segment rules.
#'
#' `NA`
#'   requests the library default. Numeric nonempty vector. Accepted finite values are 11 through
#' 14 or 21 through 24.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`maximum_length`}{source library `MAXLEN`, the maximum nominal segment length, in
#'   the consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`minimum_length`}{source library `MINLEN`, the minimum nominal segment length, in
#'   the consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`minimum_top_length`}{source library `MINLENT`, the minimum accepted top-log
#'   length, in the consuming call's height units. Numeric nonempty vector. Accepted finite values
#' are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`merchantable_length`}{source library `MERCHL`, the minimum length of primary
#'   product required for the tree to be merchantable, in the consuming call's
#'   height units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`primary_top`}{source library `MTOPP`, the primary-product top diameter inside
#'   bark used for board-foot, cubic, and cord volume, in the consuming call's
#'   diameter units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`secondary_top`}{source library `MTOPS`, the secondary-product top diameter inside
#'   bark used for cubic and cord volume, in the consuming call's diameter
#'   units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`stump`}{source library `STUMP`, the groundline-to-stump height, in the consuming
#'   call's height units. Numeric nonempty vector. Accepted finite values are at least zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`trim`}{source library `TRIM`, the trim allowance added to each segment, in the
#'   consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`bark_ratio`}{The source library bark-ratio auxiliary represented by `merchandiser`
#'   as the unitless constant diameter-inside-bark to diameter-outside-bark
#'   ratio in `(0, 1]`. `NA` leaves bark to the selected model or library
#'   default. Numeric nonempty vector.
#'
#' Accepted finite values are greater than zero and at most one. Default `NA` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors. }
#' \item{`minimum_board_foot_dbh`}{source library `MINBFD`, the minimum outside-bark diameter
#'   at breast height
#'   for board-foot eligibility, in the consuming call's diameter units. Numeric nonempty vector.
#' Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`scribner`}{source library `COR` Scribner-volume mode (unitless): `'table'` uses
#'   the Scribner table, `'factor'` uses factors, and `'regional'` requests
#'   the regional library default.
#'   Nonempty character vector, default `'regional'` when omitted.
#'   Missing elements and unknown labels are errors. Example: `scribner = 'regional'`.}
#' \item{`prod`}{source library two-character product code (unitless), such as `'01'` for
#'   sawtimber or `'08'` for a nonsaw product.
#'   Nonempty character vector, default `'01'` when omitted.
#'   Missing elements and unknown labels are errors. }
#' \item{`ht_type`}{source library `HTTYPE` code (unitless): `'F'` denotes a physical
#'   height, `'L'` denotes a height expressed in logs, and `''` leaves the
#'   field unspecified for the consuming call.
#'   Nonempty character vector, default `''` when omitted.
#'   Missing elements and unknown labels are errors. Example: `ht_type = ''`.}
#' \item{`live`}{source library live/dead status code (unitless): `'L'` for live or `'D'`
#'   for dead.
#'   Nonempty character vector, default `'L'` when omitted.
#'   Missing elements and unknown labels are errors. Example: `live = 'L'`.}
#' \item{`ctype`}{Source call-origin code, unitless. `'C'` selects cruise, `'I'` selects Forest
#' Inventory and Analysis, and `'F'` selects Forest Vegetation Simulator. `'B'` selects the
#' alternate National Scale Volume and Biomass route.
#'
#' Nonempty character vector, default `'C'` when omitted. Missing elements and unknown labels are
#' errors. Example: `ctype = 'C'`.}
#' \item{`cull`}{source library `CULL`, the whole-tree cull deduction as a percentage from
#'   0 through 100. Numeric nonempty vector. Accepted finite values are zero through 100.
#'
#' Default `0` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#'
#' }
#' \item{`forest`}{source library two-digit national-forest code represented as an integer
#'   from 0 through 99 (unitless). Zero means no forest-specific selection. Numeric nonempty
#' vector.
#'
#' Accepted finite values are whole numbers from zero through 99. Default `0` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors. }
#' \item{`district`}{source library two-digit ranger-district code represented as an
#'   integer from 0 through 99 (unitless). Zero means no district-specific
#'   selection. Numeric nonempty vector.
#'
#' Accepted finite values are whole numbers from zero through 99. Default `0` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors. }
#' }
#' @section Translation rules:
#' The returned product labels are `nvel_primary` and `nvel_secondary` with
#' priorities 1 and 2. Both use inside-bark diameter limits and board-foot
#' scaled quantity. The secondary product is marked as the pulp restriction's
#' allowed product. That label does not change its board-foot measurement unit.
#'
#' Option 14 selects the Pacific Northwest scaling convention, options 21 and 22 the allocated-
#' section convention, and the other supported options the short-section convention. `scribner
#' = 'factor'` selects factors. Other values, including `'regional'`, select the table in this
#' translator.
#'
#' The option remains in `segmentation_policy`. Minimum and maximum lengths and trim are
#' copied. A one-unit length increment is assigned, and `even_or_odd = 2` selects even lengths.
#'
#' The primary tree diameter minimum and both small-end limits come from their matching fields.
#'
#' `prod`, `ht_type`, `live`, `ctype`, `cull`, `forest`, and `district` are
#' copied to `meta_nvel_` columns. They retain notes and do not apply live-tree,
#' cull, or geographic behavior to the merchandising calculation. There is
#' no separate percentage deduction for the source `cull` field here.
#'
#' @section Returned settings:
#' `products` is the product table described in [product_schema]. Numeric `stump_ht` is the
#' record's stump height. Numeric `utilization_height` is `stump` plus `merchantable_length`,
#' used as an upper height cap.
#'
#' The translator uses this field as a height cap, despite its source definition as a minimum
#' primary-product length. `model_aux` is a list containing numeric
#' `bark_ratio`, which can be missing. Forward its value only when appropriate for the selected
#' equation.
#'
#' Lengths use the supplied height unit and the ratio is unitless.
#' @section Status and missing values:
#' No tree codes are returned. Required merchant defaults must be complete. A source-
#' version mismatch stops translation.
#'
#' Missing bark ratio is retained and does not request a model default if explicitly passed
#' onward as missing. A translated table is not evidence of volume. `no_feasible_log` identifies a
#' valid result with no logs in later calculations.
#' @seealso [nvel_rules()] to create
#' the source record, [product()] to enter product specifications, [merchandise()] to calculate the
#' translated products.
products_from_nvel_rules <- function(x) {
  if (!inherits(x, "treevolume_nvel_rules") || !is.list(x)) {
    stop(
      "x must be a treevolume_nvel_rules object. ",
      "Pass one record returned by nvel_rules().", call. = FALSE
    )
  }
  sizes <- vapply(x, length, integer(1L))
  if (any(sizes != 1L)) {
    stop(
      "x must contain exactly one NVEL rule record. ",
      "Select one record and try again.", call. = FALSE
    )
  }
  required <- c(
    "even_or_odd", "option", "maximum_length", "minimum_length",
    "minimum_top_length", "merchantable_length", "primary_top",
    "secondary_top", "stump", "trim", "minimum_board_foot_dbh"
  )
  if (any(vapply(x[required], function(value) is.na(value[[1L]]), logical(1L)))) {
    stop(
      "The NVEL rule record must have complete merchant defaults. ",
      "Fill the missing NVEL rule values and try again.", call. = FALSE
    )
  }
  expected_revision <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
  if (!identical(as.character(nvel_source_revision()), expected_revision)) {
    stop(
      "The installed NVEL revision does not match merchandiser. ",
      "Install matching package versions and try again.", call. = FALSE
    )
  }
  parity <- if (x$even_or_odd == 2L) "even" else "any"
  policy <- paste0("nvel_opt_", x$option)
  convention <- if (x$option == 14L) "whole_40" else
    if (x$option %in% c(21L, 22L)) "allocated_20" else "split_20"
  mode <- if (x$scribner == "factor") "scribner_factor_" else
    "scribner_decimal_c_"
  scale_rule <- paste0(mode, convention)
  source <- paste0("NVEL:", expected_revision)
  provenance <- list(
    meta_nvel_prod = x$prod, meta_nvel_ht_type = x$ht_type,
    meta_nvel_live = x$live, meta_nvel_ctype = x$ctype,
    meta_nvel_cull = x$cull, meta_nvel_forest = x$forest,
    meta_nvel_district = x$district
  )
  common <- c(list(
    species = "all", min_length = as.double(x$minimum_length),
    max_length = as.double(x$maximum_length), length_step = 1,
    length_parity = parity, min_boundary_length = as.double(x$minimum_top_length),
    trim = as.double(x$trim), diameter_basis = "ib",
    segmentation_policy = policy, scale_rule = scale_rule,
    measurement_quantity = "board_foot", scale_unit = "board_foot",
    scale_bark_basis = "ib", source_id = source
  ), provenance)
  primary <- do.call(.mc_legacy_product, c(list(
    product = "nvel_primary", priority = 1L,
    min_dbh = as.double(x$minimum_board_foot_dbh),
    min_sed = as.double(x$primary_top), allow_lower_products = TRUE
  ), common))
  secondary <- do.call(.mc_legacy_product, c(list(
    product = "nvel_secondary", priority = 2L,
    min_sed = as.double(x$secondary_top), fallback = TRUE,
    pulp_product = TRUE, allow_lower_products = FALSE
  ), common))
  list(
    products = products(primary, secondary),
    stump_ht = as.double(x$stump),
    utilization_height = as.double(x$stump + x$merchantable_length),
    model_aux = list(bark_ratio = as.double(x$bark_ratio))
  )
}
