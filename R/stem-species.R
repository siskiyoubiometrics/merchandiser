#' Species reference for joining tree lists
#'
#' Species codes, names, and bark ratios are available for user-managed joins.
#' Function arguments accept numeric codes only.
#' @format A data frame with these columns:
#'   * `spcd`: numeric species code.
#'   * `common`, `scientific`, `genus`: species and genus names.
#'   * `symbol`: plant symbol.
#'   * `bark_ratio`: inside to outside diameter ratio, unitless.
#'   * `softwood_hardwood`: wood group.
#'   * `wood_density`: oven-dry wood weight, pounds per cubic foot.
#'   * `sources`: provenance.
#' @export
#' @examples
#' ## Inspect the species names and codes available for joins
#' head(species_reference, n = 3)
"species_reference"
