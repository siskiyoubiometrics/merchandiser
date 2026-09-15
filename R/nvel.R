#' Translate a source rule record to product specifications
#'
#' Continuous product length bounds replace source top segment policies.
#' Factor scaling cannot be expressed by the product interface.
#' @param x The source rules supply a product specification. A list returned by nvel_rules().
#'   Required, with no default.
#' @return A list with products (the product table, with columns documented in product()),
#'   stump_ht (feet), and model_aux (named model measurements including bark_ratio when supplied).
#' @usage
#' products_from_nvel_rules(
#'   x
#' )
#' @export
#' @examples
#' ## Translate explicit source limits and inspect the result structure
#' names(products_from_nvel_rules(x = nvel_rules(even_or_odd = 1,
#'                                               option = 14,
#'                                               maximum_length = 32,
#'                                               minimum_length = 16,
#'                                               primary_top = 6,
#'                                               secondary_top = 3,
#'                                               stump = 1,
#'                                               trim = 0.5,
#'                                               minimum_board_foot_dbh = 6)))
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
