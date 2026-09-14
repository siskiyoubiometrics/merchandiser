#' Inspect species names and physical properties
#'
#' Provide species names, classification, bark ratios, and wood density from the pinned source
#' tables.
#'
#' @details Physical properties and biomass classifications follow the source sequence
#' Forest Inventory and Analysis database `REF_SPECIES` to National Biomass Estimator Library
#' `BM_REF_SPECIES` to National Volume Estimator Library `wdbkwtdata.inc`. The package pins source
#' commit
#' `38548071d5aa652bb90c7f111f86b427f798a1c9`. Names and symbols were joined from the local
#' Forest Inventory and Analysis reference table, DataMart export.
#'
#' Unmatched codes retain missing names. The join changes no physical properties or numerical
#' coefficients.
#'
#' @format A data frame with these columns:
#' \describe{
#'   \item{spcd}{Forest Inventory and Analysis species code.}
#'   \item{symbol}{species symbol.}
#'   \item{common}{Common name.}
#'   \item{scientific}{Scientific name.}
#'   \item{genus}{Genus.}
#'   \item{softwood_hardwood}{Wood type from the National Scale Volume and Biomass equations
#'   reference.}
#'   \item{bark_ratio}{Diameter ratio derived from bark-to-wood volume percent.}
#'   \item{wood_density}{Oven-dry wood weight in pounds per cubic foot.}
#'   \item{sources}{Source and pinned revision.}
#' }
#' @source tree inventory database `REF_SPECIES`, biomass source library `BM_REF_SPECIES`,
#'   and source library
#'   `wdbkwtdata.inc`. Names use the local species reference.
#' @usage
#'
#' ## Call signatures
#' species_reference
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect species represented in the example inventory
#' species_reference %>%
#'   filter(spcd %in% example_trees$spcd)
#' @export
#' @section Field types, units, and missing values:
#' `spcd` is a numeric whole-number species code. `symbol`, `common`, `scientific`, `genus`,
#' `softwood_hardwood`, and `sources` are character. They record the inventory symbol, common
#' name, scientific name, genus, wood group, and source description, respectively.
#'
#' These are unitless. `bark_ratio` is numeric inside-bark diameter divided by outside-bark
#' diameter, derived from the source bark-to-wood volume percentage. `wood_density` is numeric
#' oven-dry wood pounds per cubic foot.
#'
#' Data units are fixed and are not changed by a consuming function's unit argument.
#'
#' Missing names or properties remain missing and are not defaults to replace with zero. Data
#' fields have no omitted-argument behavior because they are shipped columns. There are no list
#' columns.
#' @section Status and missing values:
#' The table contains no calculation status codes.
#'
#' Missing names do not necessarily mean missing physical properties. Check the required field
#' for the intended calculation, using [species_lookup()] for matched rows.
#' @seealso
#' [species_lookup()] to match identifiers, [green_weight()] to convert volume,
#' [taper_models()] to inspect equation scope.
"species_reference"

#' Match tree species codes, symbols, or names
#'
#' Match species codes, symbols, common names, or scientific names. Return reference records or
#' one selected field in input order.
#'
#' @param x Required numeric vector for `from = 'spcd'`, or character vector
#'   for name and symbol matching. Any length is accepted. Codes must be
#'   positive whole numbers within R's integer range to match.
#'
#' Missing and
#'   unmatched inputs return missing fields. Omission or wrong types are errors. Unitless.
#'
#' Example: `example_trees$spcd`.
#' @param from One character label, `'spcd'` (default), `'symbol'`, `'common'`,
#'   or `'scientific'`. Missing or unknown choices are errors. Unitless.
#'
#' Example: `from = 'common'`.
#' @param to One character label, default `'record'` for all fields. Any column
#'   of `species_reference` may be selected. Missing or unknown choices are
#'   errors.
#'
#' Unitless. Example: `to = 'wood_density'`.
#'
#' @return A data frame for `to = 'record'`. Otherwise, a vector with the same
#'   size as `x`. Unknown identifiers are `NA`.
#' @export
#' @usage
#'
#' ## Call signatures
#' species_lookup(x, from = c('spcd', 'symbol', 'common', 'scientific'), to = 'record')
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Match the example species to reference records
#' example_trees %>%
#'   distinct(spcd) %>%
#'   mutate(common = species_lookup(x = spcd, to = 'common'))
#' @inherit species_reference format
#' @inheritSection species_reference Field types, units, and missing values
#' @section Status and missing values:
#' No calculation codes are returned. Missing or unmatched identifiers return missing rows or
#' values. An ambiguous name stops the call and lists matching codes.
#'
#' Select the intended code and try again. Missing properties are not observed zeros.
#' @seealso
#' [species_reference] to inspect all fields, [taper_models()] to inspect equation scope,
#' [green_weight()] to estimate weight from volume.
species_lookup <- function(x, from = c("spcd", "symbol", "common", "scientific"),
                           to = "record") {
  from <- match.arg(from)
  to <- .scalar_character(to, "to", c("record", names(species_reference)))
  if (identical(from, "spcd")) {
    if (!is.numeric(x)) {
      stop("x must be numeric when from is spcd.", call. = FALSE)
    }
    valid <- is.finite(x) & x == floor(x) & x > 0 & x <= .Machine$integer.max
    matched <- rep(NA_integer_, length(x))
    matched[valid] <- match(as.integer(x[valid]), species_reference$spcd)
  } else {
    if (!is.character(x)) {
      stop("x must be character for name and symbol lookups.", call. = FALSE)
    }
    normalize <- function(value) tolower(trimws(value))
    reference <- normalize(species_reference[[from]])
    matched <- rep(NA_integer_, length(x))
    for (index in seq_along(x)) {
      if (is.na(x[[index]])) {
        next
      }
      candidates <- which(!is.na(reference) & reference == normalize(x[[index]]))
      if (length(candidates) > 1L) {
        labels <- paste0(
          species_reference$spcd[candidates], " (", species_reference$symbol[candidates], ", ",
          species_reference$scientific[candidates], ")"
        )
        stop(
          "ambiguous ", from, " name '", x[[index]], "'. Candidates: ",
          paste(labels, collapse = ", "), call. = FALSE
        )
      }
      if (length(candidates) == 1L) {
        matched[[index]] <- candidates
      }
    }
  }
  if (identical(to, "record")) {
    result <- species_reference[rep(NA_integer_, length(x)), , drop = FALSE]
    known <- !is.na(matched)
    result[known, ] <- species_reference[matched[known], , drop = FALSE]
    rownames(result) <- NULL
    return(result)
  }
  species_reference[[to]][matched]
}
