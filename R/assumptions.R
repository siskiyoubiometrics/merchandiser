#' Review the choices recorded with a result
#'
#' @param x The result holds the assumptions to inspect. A merch_result object. Required, with no
#'   default.
#' @return The assumptions data frame, with columns:
#'   * `tree_id`: tree identifier.
#'   * `assumption`: assumption name.
#'   * `spcd`: species code.
#'   * `model`: model identifier.
#'   * `value`, `unit`: numeric assumption and its measurement unit.
#'   * `basis`: measurement basis.
#'   * `product`: product name.
#'   * `source`: provenance.
#' @usage
#' assumptions(
#'   x
#' )
#' @export
#' @examples
#' ## Inspect assumption names for the first example tree
#' assumptions(x = merchandise(tree_id = example_trees$tree_id[1],
#'                             dbh = example_trees$dbh[1],
#'                             ht = example_trees$ht[1],
#'                             spcd = example_trees$spcd[1],
#'                             products = product(product = 'saw',  ## name, unitless
#'                                                min_length = 16,  ## feet
#'                                                max_length = 40,  ## feet
#'                                                min_sed = 6,  ## inches
#'                                                volume_unit = 'cubic')))$assumption  ## cubic feet
assumptions <- function(x) {
  if (!inherits(x, "merch_result")) {
    stop("x must be a merchandise result.", call. = FALSE)
  }
  x$assumptions
}
