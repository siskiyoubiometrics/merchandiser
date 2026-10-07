#' Inspect assumptions recorded with a merchandising result
#'
#' Extract the selected equation, stump height, and any supplementary bark ratio recorded during
#'   calculation. Use these records to compare inputs behind merchandising results.
#'
#' @param x Result returned by [merchandise()]. Required, without a default. The stored
#'   assumptions are returned without recalculating the tree.
#' @return A data frame with `tree_id` (input identifier), `assumption` (record type), `spcd`
#'   (numeric species code), `model` (equation identifier), `value` (numeric value), `unit`
#'   (value unit), `basis` (measurement basis), `product` (associated product or missing), and
#'   `source` (provenance). Stump values use feet and bark ratios are inside diameter divided by
#'   outside diameter. Equation selections have identifiers rather than numeric values.
#' @usage
#' assumptions(
#'   x
#' )
#' @export
#' @examples
#' ## Define an unpriced cubic-foot product
#' saw <- product(product = 'saw',  ## product label
#'                min_length = 16,  ## feet
#'                max_length = 32,  ## feet
#'                min_sed = 6,  ## inches inside bark
#'                volume_unit = 'cubic')  ## cubic feet
#'
#' ## Select logs from the shipped trees
#' result <- merchandise(tree_id = example_trees$tree_id,
#'                       dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       spcd = example_trees$spcd,
#'                       products = saw,
#'                       model = example_trees$model)
#'
#' ## Inspect the recorded equation and stump inputs
#' head(assumptions(x = result))
assumptions <- function(x) {
  if (!inherits(x, "merch_result")) {
    stop("x must be a merchandise result.", call. = FALSE)
  }
  x$assumptions
}
