#' Understand how stem calculations support log selection
#'
#' Stem equations provide diameter, inverse-height, and volume operations used during log
#' selection.
#'
#' @details `dib()`, `height_at_dib()`, and `stem_volume()` supply these operations.
#' `model_capabilities()` reports direct equation capabilities.
#' @section Status and missing values:
#' Use [status_codes()] to interpret tree diagnoses. An unavailable equation
#' or bark calculation cannot be treated as observed zero volume.
#' @seealso [dib()] for stem diameter, [stem_volume()] for volume,
#'   [merchandise()] for log selection.
#' @name provider_api
#' @keywords internal
#' @examples
#' ## Inspect the source revision used for inventory equations
#' nvel_source_revision()
NULL

.nvel_source_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
.nvel_upstream_url <- "https://github.com/FMSC-Measurements/VolumeLibrary"
.nvel_fixtures_release_tag <- "v0.1.0"

#' Record the source version used for the shipped equations
#'
#' Return the pinned source revision used to generate the shipped equations. Attributes
#' identify the source repository and fixture release.
#'
#' @return A single character source revision, unitless. Character attributes
#'   `upstream_url` and `fixtures_release_tag` record the source repository
#'   address and saved reference-test release label. Neither is a tree input.
#' @details This identifies the source used to prepare the shipped equations,
#'   not the current upstream release. It does not include local coefficients.
#' @section Status and missing values:
#' No status codes or missing values are produced. A matching revision
#' identifies a common source version, not agreement for every possible tree.
#' @seealso [taper_manifest()] for registered equations, [assumptions()] for
#'   result choices, [products_from_nvel_rules()] for source-rule translation.
#' @export
#' @usage
#'
#' ## Call signatures
#' nvel_source_revision()
#' @examples
#' ## Inspect the source revision used for inventory equations
#' nvel_source_revision()
nvel_source_revision <- function() {
  structure(
    .nvel_source_commit,
    upstream_url = .nvel_upstream_url,
    fixtures_release_tag = .nvel_fixtures_release_tag
  )
}
