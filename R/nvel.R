#' Convert explicit source rules to products
#'
#' Translate a complete source rule record into explicit product rows. Use the result when the
#'   source rule fields have already been resolved for an equation.
#'
#' @param x Validated list returned by [nvel_rules()] with explicit finite length, diameter,
#'   stump, and trim fields. Required, without a default. Missing source defaults cannot be
#'   translated without an equation-specific lookup.
#' @return A list with `products` (two [product()] rows named `nvel_primary` and
#'   `nvel_secondary`, scaled in Scribner board feet), `stump_ht` (feet), and `model_aux` (list
#'   containing `bark_ratio`, an inside-to-outside diameter ratio). Product columns and units are
#'   those of [product()]. Continuous product bounds do not reproduce every source top-segment
#'   rule.
#' @usage
#' products_from_nvel_rules(
#'   x
#' )
#' @export
#' @examples
#' ## Translate complete source rules and inspect the stump height in feet
#' products_from_nvel_rules(x = nvel_rules(even_or_odd = 1,
#'                                         option = 11,
#'                                         maximum_length = 32,
#'                                         minimum_length = 16,
#'                                         primary_top = 6,
#'                                         secondary_top = 4,
#'                                         stump = 1,
#'                                         trim = 0.5,
#'                                         minimum_board_foot_dbh = 1))$stump_ht
products_from_nvel_rules <- function(x) {
  if (!inherits(x, "treevolume_nvel_rules") || any(lengths(x) != 1)) {
    stop("x must contain one record from nvel_rules().", call. = FALSE)
  }
  required <- c(
    "even_or_odd", "option", "maximum_length", "minimum_length",
    "primary_top", "secondary_top", "stump",
    "trim", "minimum_board_foot_dbh"
  )
  if (anyNA(unlist(x[required])))
    stop("Fill the source rule's length and diameter limits.", call. = FALSE)
  if (identical(x$scribner, "factor")) {
    stop("Regional factor scaling has no product volume unit.", call. = FALSE)
  }
  primary <- product("nvel_primary",
    min_dbh = x$minimum_board_foot_dbh, min_length = x$minimum_length,
    max_length = x$maximum_length, length_round = if (x$even_or_odd == 2)
      2 else 1, trim = x$trim, min_sed = x$primary_top, volume_unit = "scribner",
    split_scale = x$option !=
      14
  )
  secondary <- product("nvel_secondary",
    min_length = x$minimum_length, max_length = x$maximum_length,
    length_round = if (x$even_or_odd == 2)
      2 else 1, trim = x$trim, min_sed = x$secondary_top, volume_unit = "scribner",
    split_scale = x$option !=
      14
  )
  list(
    products = products(primary, secondary), stump_ht = x$stump,
    model_aux = list(bark_ratio = x$bark_ratio)
  )
}
