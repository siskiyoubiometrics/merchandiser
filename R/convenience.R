.mc_convenience_result <- function(dbh, ht, species, taper_map, dots,
                                   function_name) {
  warn <- !"status" %in% names(dots)
  if (warn) dots$status <- TRUE
  result <- do.call(
    merchandise,
    c(list(
      dbh = dbh, ht = ht, species = species, taper_map = taper_map
    ), dots)
  )
  if (warn) {
    .mc_status_warning(function_name, result$trees$status, result$trees$spcd)
  }
  result
}

#' Select logs and summarize volume by product
#'
#' Select logs and return grouped product totals in one call. Expansion weights apply to
#' quantities, while contributing-tree counts remain unweighted.
#'
#' @inheritParams merchandise
#' @inheritParams product_summary
#' @param ... Named arguments from [merchandise()], individually listed in the corresponding
#' section. Omission uses their stated defaults.
#'
#' @return A grouped data frame with the totals, labels, and unweighted
#'   contributing-tree counts listed in the corresponding section. Cubic totals use cubic feet or
#'   cubic meters, and scaled totals retain their product measurement unit.
#' @export
#' @usage
#'
#' ## Call signatures
#' product_summary_by_tree(dbh, ht, species, taper_map = NULL, ..., group = NULL, expansion = 1,
#'   basis = 'net', na_action = 'exclude')
#' @examples
#' ## Select logs and sum their quantities by product
#' product_summary_by_tree(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         species = example_trees$species,
#'                         model = example_trees$model,
#'                         products = example_products(name = 'pnw'),
#'                         status = TRUE)
#' @inheritSection merchandise Input length and equation selection
#' @inheritSection merchandise Equation auxiliaries
#' @inheritSection merchandise Status and missing values
#' @section Arguments passed through dots:
#' \describe{
#' \item{`model`}{Character or factor vector of registered taper equation
#'   identifiers, for example `'F00FW2W202'`. Default `NULL` uses
#'   `taper_map`, or the geographic defaults if no map is supplied. An explicit model takes
#' precedence over `taper_map`.
#'
#' Unknown or missing
#'   identifiers do not request a replacement equation. Unitless.}
#' \item{`products`}{A product data frame, usually made with [product()]
#'   and [products()]. The fields, units, defaults, and accepted
#' measurement rules are described in [Product specification schema][product_schema]. Default
#'   `NULL` loads the
#'   selected illustrative preset.
#'
#' An explicitly supplied table takes
#'   precedence. Invalid or incomplete specifications stop the call. Example: `products =
#' example_products(name = 'pnw')`.}
#' \item{`id`}{Atomic vector of unique, nonmissing tree identifiers, such as
#'   `c('A-1', 'A-2')`. Default `NULL` assigns sequential row numbers after
#'   input lengths are resolved. Required when a nonempty defect table is
#'   supplied.
#'
#' Use the same identifier type in that table. Unitless.}
#' \item{`spcd`}{Species identifiers, supplied as positive whole-number
#'   inventory species codes, recognized symbols, common names, or
#'   scientific names. Default `NULL`. Supply species
#'   for automatic equation selection, species-restricted products, or
#'   green-weight calculations.
#'
#' Missing required species can leave a tree
#'   unresolved or a scaled quantity unavailable. Unitless.}
#' \item{`age`}{Numeric tree ages, default `NULL`.
#'   Use years and the same total-age or breast-height-age convention as
#'   the product table. Required only when a relevant product limits age.
#'   Missing required age is a missing product attribute, not age zero.}
#' \item{`pruned`}{Logical or numeric `0`/`1` pruning indicators, default
#'   `NULL`. Example: `pruned = TRUE`. Required only when a relevant
#'   product requires a pruning state.
#'
#' Missing required values are missing
#'   product attributes. Unitless. No pruning height is inferred.}
#' \item{`defects`}{A defect data frame made with [defect()] or
#'   [defects_from_stoppers()]. Default `NULL` means no recorded defects
#'   are applied. An empty table has the same effect.
#'
#' Required fields are
#'   `id`, `from`, `to`, and `effect`, with conditional fields listed in the corresponding section.
#' Heights use the call's height units. Missing fields or invalid
#'   record structure may stop the call.
#'
#' }
#' \item{`stump_ht`}{Numeric stump heights above ground, default `NULL`. Omission uses 1 foot for
#' imperial calls or 0.3 meter for metric calls. These are the package's two defaults, not exact
#' conversions of each
#'   other.
#'
#' Supplied values must satisfy
#'   `0 <= stump_ht < ht`. An explicit `NA` does not request the default. Supply a finite height
#' for each tree being calculated.}
#' \item{`utilization_top`}{Numeric minimum utilization diameters, default
#'   `NULL`. Units are inches or
#'   centimeters. When omitted, each tree uses the minimum small-end
#'   diameter and bark basis of its last reachable product before defects
#'   are applied.
#'
#' A supplied zero requests the tree tip. A missing value
#'   or a diameter without a usable crossing gives `utilization_top_invalid`.}
#' \item{`utilization_top_basis`}{One character value, `'ib'` for inside
#'   bark or `'ob'` for outside bark. Default `'ib'`. Used for an
#'   explicitly supplied `utilization_top`.
#'
#' The omitted-top calculation
#'   instead uses the selected product's bark basis. Missing is invalid. Example:
#' `utilization_top_basis = 'ib'`.
#'
#' Unitless.}
#' \item{`utilization_height`}{Numeric upper height limits above ground,
#'   default `NULL` for no additional height cap. Values use feet or
#'   meters and must be nonnegative and finite, or `NA` for no cap on that
#'   tree.  The effective top is the
#'   lowest of this cap, the diameter limit, total height, and the earliest
#'   recorded break.}
#' \item{`curvature_scale`}{A list with names `order` and `ratio_upper`, in
#'   that order. Default `NULL`. Required when a defect supplies a sweep
#'   or crook category or a product limits one.
#'
#' `order` is a unique,
#'   nonmissing character vector from least to greatest severity. `ratio_upper` is a numeric vector
#' of equal length, strictly increasing
#'   from zero to a final `Inf`, with finite intervening values. Both components are unitless.
#'
#' Current
#'   eligibility uses category order. The bounds are validated metadata,
#'   not an equation that converts measured sweep into a scale deduction. Invalid lists or unknown
#' product maximum categories stop the call.
#'
#' A defect category absent from the scale gives tree `unknown_curvature_category`.}
#' \item{`report_also`}{Character procedure names or a data frame of named
#' measurements, default `NULL` for none. Units follow each definition.
#' Missing definitions are invalid.
#' See [Scaling rules][scaling_rules] for all fields and procedures.}
#' \item{`scaling`}{Deprecated alternative to `report_also`, default `NULL`.
#' Supply only one. Use `report_also = 'huber_ft3_ib'` in new code.}
#' \item{`currency`}{One nonempty character label for the currency of all
#'   supplied prices, default `NULL`. Required if any product has a price. Example: `currency =
#' 'USD'` for United States dollars.
#'
#' Missing is
#'   invalid when prices are present. This labels values without converting
#'   currencies or establishing whether prices are delivered or stumpage.}
#' \item{`units`}{One character value, `'imperial'` or `'metric'`. Default `'imperial'`. Imperial
#' inputs use inches for diameters and
#'   feet for lengths and heights, with physical volume in cubic feet.
#'
#' Metric inputs use centimeters and meters, with physical volume in
#'   cubic meters. Measurement units are specified separately in `products`. Missing or other
#' spellings are invalid.
#'
#' Example: `units = 'metric'`.}
#' \item{`status`}{One logical or `0`/`1` value. When omitted, diagnoses are retained
#'   and nonzero tree codes warn. `TRUE` adds status columns to tree and log tables and a
#' `diagnostics`
#'   table, while suppressing tree-status warnings.
#'
#' `FALSE` reports
#'   nonzero tree statuses in warnings. Structural errors still stop either
#'   call. Missing is invalid.
#'
#' Example: `status = TRUE`. Unitless.}
#' \item{`preset`}{One name returned by [example_product_names()], default `NULL`. Example: `preset
#' = 'pnw'`. Selects an illustrative product
#'   table if `products` is omitted, and provides geographic defaults if
#'   needed.
#'
#' Other names include `'us_south'` and `'douglas_fir'`. Missing or unknown
#'   names are invalid. These are examples to review, not verified product
#'   specifications.
#'
#' Unitless.}
#' \item{`quiet`}{One logical or `0`/`1` value, default `FALSE`. `TRUE` suppresses the
#' informational defaults message. It does not
#'   suppress tree-status warnings or errors.
#'
#' Missing is invalid. Example: `quiet = TRUE`. Unitless.}
#' \item{`region`}{One numeric Forest Service region code or a preset name,
#'   default `NULL`. Used for source-library
#'   equation defaults when explicit equations are absent. Valid library
#'   region codes are 1 through 10.
#'
#' Omission uses the selected preset,
#'   otherwise the southern example for an entirely loblolly pine species
#'   vector and the Pacific Northwest example for other inputs. This is not an
#'   inference of geographic location. Missing is invalid.
#'
#' Unitless.}
#' \item{`forest`}{One numeric whole-number Forest Service forest code from
#'   zero through 99, default `NULL`. Omission uses the selected geographic settings. The Pacific
#' Northwest example
#'   uses 12 and the southern example uses 8.
#'
#' This is an agency code, not
#'   the name or identifier of the user's stand. Missing is invalid. Unitless.}
#' \item{`district`}{One numeric whole-number Forest Service district code
#'   from zero through 99, default `NULL`. Omission uses the selected settings, currently zero in
#' both example
#'   presets. Other numeric regions begin with forest and district zero
#'   unless overridden.
#'
#' Missing is invalid. Unitless. }
#'
#' }
#' @inheritSection product_summary Grouping and returned totals
#' @seealso [product_summary()] to summarize a saved result, [stand_table()]
#'   to retain the complete inventory, including trees with no logs.
#' @param species Required species identifiers, numeric inventory codes or
#'   character symbols or names, length one or one per tree. Unitless. Example:
#' `example_trees$spcd`.
#'
#' Omission is an error. Missing species
#'   cannot select an automatic equation or satisfy a species-limited product.
#' @section Convenience-call status behavior:
#' Omitting `status` retains diagnoses and warns for nonzero tree
#' statuses. Explicit `status = TRUE` retains diagnoses and suppresses those
#' warnings. Explicit `FALSE` discards the diagnoses, so the numeric helpers
#' cannot replace failed-tree quantities with missing values.
#'
#' `no_feasible_log` is
#' a valid result with no logs and remains zero in the numeric helpers.
#' @section Shared fields:
#' See [Product specification schema][product_schema] for product fields, [defect()] for recorded
#'   conditions,
#' and [Result tables][result_tables] for the complete volume tables.
product_summary_by_tree <- function(dbh, ht, species, taper_map = NULL, ...,
                                    group = NULL,
                                    expansion = 1, basis = "net",
                                    na_action = "exclude") {
  basis <- .mc_scalar_choice(basis, "basis", c("net", "gross"))
  na_action <- .mc_scalar_choice(
    na_action, "na_action", c("exclude", "propagate")
  )
  result <- .mc_convenience_result(
    dbh, ht, species, taper_map, list(...), "product_summary_by_tree"
  )
  product_summary(
    result, table = "logs", group = group, expansion = expansion,
    basis = basis, na_action = na_action
  )
}

#' Calculate log volume or log count for each tree
#'
#' Return physical inside-bark log volume or log count for each input tree. Failed-tree
#' quantities remain missing when calculation statuses are retained.
#'
#' @inheritParams merchandise
#' @param basis One character value, `'net'` (default) or `'gross'`. Selects physical inside-bark
#'   log volume after or before deductions. Missing or unknown choices are errors.
#'
#' Unitless. Example: `basis = 'gross'`.
#' @param ... Named arguments from [merchandise()], individually listed in the corresponding
#' section. Omission uses their stated defaults.
#'
#' @return `tree_cubic_volume()` returns numeric physical inside-bark log
#'   volume including trim in cubic feet or cubic meters. `tree_log_count()`
#'   returns integer unitless log counts. Both have one element per tree.
#' @name merch_tree_helpers
#' @usage
#'
#' ## Call signatures
#' tree_cubic_volume(dbh, ht, species, taper_map = NULL, ..., basis = 'net')
#'
#' tree_log_count(dbh, ht, species, taper_map = NULL, ...)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Add selected log volume to the first example trees
#' example_trees %>%
#'   slice_head(n = 3) %>%
#'   transmute(tree,
#'             cubic = tree_cubic_volume(dbh = dbh,
#'                                       ht = ht,
#'                                       species = species,
#'                                       model = model,
#'                                       products = example_products(name = 'pnw')))
#' @inheritSection merchandise Input length and equation selection
#' @inheritSection merchandise Equation auxiliaries
#' @inheritSection merchandise Status and missing values
#' @section Arguments passed through dots:
#' \describe{
#' \item{`model`}{Character or factor vector of registered taper equation
#'   identifiers, for example `'F00FW2W202'`. Default `NULL` uses
#'   `taper_map`, or the geographic defaults if no map is supplied. An explicit model takes
#' precedence over `taper_map`.
#'
#' Unknown or missing
#'   identifiers do not request a replacement equation. Unitless.}
#' \item{`products`}{A product data frame, usually made with [product()]
#'   and [products()]. The fields, units, defaults, and accepted
#' measurement rules are described in [Product specification schema][product_schema]. Default
#'   `NULL` loads the
#'   selected illustrative preset.
#'
#' An explicitly supplied table takes
#'   precedence. Invalid or incomplete specifications stop the call. Example: `products =
#' example_products(name = 'pnw')`.}
#' \item{`id`}{Atomic vector of unique, nonmissing tree identifiers, such as
#'   `c('A-1', 'A-2')`. Default `NULL` assigns sequential row numbers after
#'   input lengths are resolved. Required when a nonempty defect table is
#'   supplied.
#'
#' Use the same identifier type in that table. Unitless.}
#' \item{`spcd`}{Species identifiers, supplied as positive whole-number
#'   inventory species codes, recognized symbols, common names, or
#'   scientific names. Default `NULL`. Supply species
#'   for automatic equation selection, species-restricted products, or
#'   green-weight calculations.
#'
#' Missing required species can leave a tree
#'   unresolved or a scaled quantity unavailable. Unitless.}
#' \item{`age`}{Numeric tree ages, default `NULL`.
#'   Use years and the same total-age or breast-height-age convention as
#'   the product table. Required only when a relevant product limits age.
#'   Missing required age is a missing product attribute, not age zero.}
#' \item{`pruned`}{Logical or numeric `0`/`1` pruning indicators, default
#'   `NULL`. Example: `pruned = TRUE`. Required only when a relevant
#'   product requires a pruning state.
#'
#' Missing required values are missing
#'   product attributes. Unitless. No pruning height is inferred.}
#' \item{`defects`}{A defect data frame made with [defect()] or
#'   [defects_from_stoppers()]. Default `NULL` means no recorded defects
#'   are applied. An empty table has the same effect.
#'
#' Required fields are
#'   `id`, `from`, `to`, and `effect`, with conditional fields listed in the corresponding section.
#' Heights use the call's height units. Missing fields or invalid
#'   record structure may stop the call.
#'
#' }
#' \item{`stump_ht`}{Numeric stump heights above ground, default `NULL`. Omission uses 1 foot for
#' imperial calls or 0.3 meter for metric calls. These are the package's two defaults, not exact
#' conversions of each
#'   other.
#'
#' Supplied values must satisfy
#'   `0 <= stump_ht < ht`. An explicit `NA` does not request the default. Supply a finite height
#' for each tree being calculated.}
#' \item{`utilization_top`}{Numeric minimum utilization diameters, default
#'   `NULL`. Units are inches or
#'   centimeters. When omitted, each tree uses the minimum small-end
#'   diameter and bark basis of its last reachable product before defects
#'   are applied.
#'
#' A supplied zero requests the tree tip. A missing value
#'   or a diameter without a usable crossing gives `utilization_top_invalid`.}
#' \item{`utilization_top_basis`}{One character value, `'ib'` for inside
#'   bark or `'ob'` for outside bark. Default `'ib'`. Used for an
#'   explicitly supplied `utilization_top`.
#'
#' The omitted-top calculation
#'   instead uses the selected product's bark basis. Missing is invalid. Example:
#' `utilization_top_basis = 'ib'`.
#'
#' Unitless.}
#' \item{`utilization_height`}{Numeric upper height limits above ground,
#'   default `NULL` for no additional height cap. Values use feet or
#'   meters and must be nonnegative and finite, or `NA` for no cap on that
#'   tree.  The effective top is the
#'   lowest of this cap, the diameter limit, total height, and the earliest
#'   recorded break.}
#' \item{`curvature_scale`}{A list with names `order` and `ratio_upper`, in
#'   that order. Default `NULL`. Required when a defect supplies a sweep
#'   or crook category or a product limits one.
#'
#' `order` is a unique,
#'   nonmissing character vector from least to greatest severity. `ratio_upper` is a numeric vector
#' of equal length, strictly increasing
#'   from zero to a final `Inf`, with finite intervening values. Both components are unitless.
#'
#' Current
#'   eligibility uses category order. The bounds are validated metadata,
#'   not an equation that converts measured sweep into a scale deduction. Invalid lists or unknown
#' product maximum categories stop the call.
#'
#' A defect category absent from the scale gives tree `unknown_curvature_category`.}
#' \item{`report_also`}{Character procedure names or a data frame of named
#' measurements, default `NULL` for none. Units follow each definition.
#' Missing definitions are invalid.
#' See [Scaling rules][scaling_rules] for all fields and procedures.}
#' \item{`scaling`}{Deprecated alternative to `report_also`, default `NULL`.
#' Supply only one. Use `report_also = 'huber_ft3_ib'` in new code.}
#' \item{`currency`}{One nonempty character label for the currency of all
#'   supplied prices, default `NULL`. Required if any product has a price. Example: `currency =
#' 'USD'` for United States dollars.
#'
#' Missing is
#'   invalid when prices are present. This labels values without converting
#'   currencies or establishing whether prices are delivered or stumpage.}
#' \item{`units`}{One character value, `'imperial'` or `'metric'`. Default `'imperial'`. Imperial
#' inputs use inches for diameters and
#'   feet for lengths and heights, with physical volume in cubic feet.
#'
#' Metric inputs use centimeters and meters, with physical volume in
#'   cubic meters. Measurement units are specified separately in `products`. Missing or other
#' spellings are invalid.
#'
#' Example: `units = 'metric'`.}
#' \item{`status`}{One logical or `0`/`1` value. When omitted, diagnoses are retained
#'   and nonzero tree codes warn. `TRUE` adds status columns to tree and log tables and a
#' `diagnostics`
#'   table, while suppressing tree-status warnings.
#'
#' `FALSE` reports
#'   nonzero tree statuses in warnings. Structural errors still stop either
#'   call. Missing is invalid.
#'
#' Example: `status = TRUE`. Unitless.}
#' \item{`preset`}{One name returned by [example_product_names()], default `NULL`. Example: `preset
#' = 'pnw'`. Selects an illustrative product
#'   table if `products` is omitted, and provides geographic defaults if
#'   needed.
#'
#' Other names include `'us_south'` and `'douglas_fir'`. Missing or unknown
#'   names are invalid. These are examples to review, not verified product
#'   specifications.
#'
#' Unitless.}
#' \item{`quiet`}{One logical or `0`/`1` value, default `FALSE`. `TRUE` suppresses the
#' informational defaults message. It does not
#'   suppress tree-status warnings or errors.
#'
#' Missing is invalid. Example: `quiet = TRUE`. Unitless.}
#' \item{`region`}{One numeric Forest Service region code or a preset name,
#'   default `NULL`. Used for source-library
#'   equation defaults when explicit equations are absent. Valid library
#'   region codes are 1 through 10.
#'
#' Omission uses the selected preset,
#'   otherwise the southern example for an entirely loblolly pine species
#'   vector and the Pacific Northwest example for other inputs. This is not an
#'   inference of geographic location. Missing is invalid.
#'
#' Unitless.}
#' \item{`forest`}{One numeric whole-number Forest Service forest code from
#'   zero through 99, default `NULL`. Omission uses the selected geographic settings. The Pacific
#' Northwest example
#'   uses 12 and the southern example uses 8.
#'
#' This is an agency code, not
#'   the name or identifier of the user's stand. Missing is invalid. Unitless.}
#' \item{`district`}{One numeric whole-number Forest Service district code
#'   from zero through 99, default `NULL`. Omission uses the selected settings, currently zero in
#' both example
#'   presets. Other numeric regions begin with forest and district zero
#'   unless overridden.
#'
#' Missing is invalid. Unitless. }
#'
#' }
#' @section Interpreting one value per tree:
#' `tree_cubic_volume()` returns physical inside-bark log volume including trim,
#' in cubic feet or cubic meters according to `units`. `basis = 'net'`
#' (default) includes deductions and `'gross'` precedes them. Missing or
#' unknown basis labels are errors.
#'
#' `tree_log_count()` returns an integer
#' count, which is unitless. Neither output is expanded to a stand total. Failed trees return
#' missing values when statuses are retained, as they
#' are by default inside these functions.
#'
#' Explicit `status = FALSE` removes
#' that failure mask. Keep the default when failed trees must remain missing.
#' @seealso [merchandise()] for the full log and tree tables, [stand_table()]
#'   for expanded totals, [product_summary_by_tree()] for product totals.
#' @param species Required species identifiers, numeric inventory codes or
#'   character symbols or names, length one or one per tree. Unitless. Example:
#' `example_trees$spcd`.
#'
#' Omission is an error. Missing species
#'   cannot select an automatic equation or satisfy a species-limited product.
#' @section Convenience-call status behavior:
#' Omitting `status` retains diagnoses and warns for nonzero tree
#' statuses. Explicit `status = TRUE` retains diagnoses and suppresses those
#' warnings. Explicit `FALSE` discards the diagnoses, so the numeric helpers
#' cannot replace failed-tree quantities with missing values.
#'
#' `no_feasible_log` is
#' a valid result with no logs and remains zero in the numeric helpers.
#' @section Shared fields:
#' See [Product specification schema][product_schema] for product fields, [defect()] for recorded
#'   conditions,
#' and [Result tables][result_tables] for the complete volume tables.
NULL

#' @rdname merch_tree_helpers
#' @export
#' @usage NULL
tree_cubic_volume <- function(dbh, ht, species, taper_map = NULL, ...,
                              basis = "net") {
  basis <- .mc_scalar_choice(basis, "basis", c("net", "gross"))
  result <- .mc_convenience_result(
    dbh, ht, species, taper_map, list(...), "tree_cubic_volume"
  )
  if (basis == "net") {
    value <- result$trees$log_net_cubic_ib
  } else {
    tree <- match(result$logs$id, result$trees$id)
    value <- .mc_sum_by_index(
      result$logs$log_gross_cubic_ib, tree, nrow(result$trees)
    )
  }
  if ("status" %in% names(result$trees)) {
    value[!result$trees$status %in% c(0L, 410L)] <- NA_real_
  }
  value
}

#' @rdname merch_tree_helpers
#' @export
#' @usage NULL
tree_log_count <- function(dbh, ht, species, taper_map = NULL, ...) {
  result <- .mc_convenience_result(
    dbh, ht, species, taper_map, list(...), "tree_log_count"
  )
  value <- result$trees$log_count
  if ("status" %in% names(result$trees)) {
    value[!result$trees$status %in% c(0L, 410L)] <- NA_integer_
  }
  value
}
