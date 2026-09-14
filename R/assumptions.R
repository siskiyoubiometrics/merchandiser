#' Review the choices recorded with a result
#'
#' Return the choices recorded in a merchandising result without repeating its calculations.
#'
#' @param x Required `merch_result` from [merchandise()] or
#'   [optimize_bucking()], The function returns `run_metadata$assumptions` without validating its
#' contents. Other classes are errors. Missing recorded assumptions return `NULL`.
#' @return A data frame with one row per distinct recorded choice. All fields
#'   except `value` are unitless identifiers or descriptions. Character
#'   `assumption` names the choice.
#'
#' Integer `spcd` and character `species`
#'   identify its species, and character `model` names the equation. Numeric `value` records a
#' magnitude in the character `units` field. Character `basis` identifies the bark or measurement
#' basis.
#'
#' Character `product`, `preset`, and `source` identify the specification,
#'   example table, and source. Integer `region`, `forest`, and `district`
#'   record geographic selection codes. Missing fields mean not recorded or
#'   not applicable to that choice.
#'
#'  For example, a stump choice can have `value = 1` and
#'   `units = 'imperial'`, meaning feet for stump height.
#' @details The table records equation mappings, automatically selected
#'   example products, species-reference bark ratios, stump defaults, unit
#'   system, and default utilization limits when applicable. It does not
#'   contain local equation coefficients. Preserve those models separately.
#' @section Status and missing values:
#' No new calculation code is produced. Missing fields do not mean a zero
#' assumption or a failed tree. Inspect `x$trees$status` separately.
#'
#' `no_feasible_log` there
#' denotes a valid result with no logs under the selected specifications.
#' @seealso [merchandise()] to make a result, [taper_manifest()] to record
#'   available equations, [status_codes()] to interpret failed trees.
#' @export
#' @usage
#'
#' ## Call signatures
#' assumptions(x)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Calculate logs for the shipped tree list
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       model = example_trees$model,
#'                       species = example_trees$species,
#'                       products = example_products(name = 'pnw'),
#'                       status = TRUE)
#'
#' ## Inspect the result
#' assumptions(x = result) %>%
#'   slice_head(n = 3)
assumptions <- function(x) {
  if (!inherits(x, "merch_result")) {
    stop(
      "x must be a merch_result. Pass the object returned by merchandise() ",
      "or optimize_bucking().", call. = FALSE
    )
  }
  x$run_metadata$assumptions
}
