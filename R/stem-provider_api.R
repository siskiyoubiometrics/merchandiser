NULL

.nvel_source_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
.nvel_upstream_url <- "https://github.com/FMSC-Measurements/VolumeLibrary"
.nvel_fixtures_release_tag <- "v0.1.0"

#' Inspect the pinned source library revision
#'
#' Return the source revision associated with shipped equations and fixtures. Use it to identify
#'   the implementation behind a source comparison.
#'
#' @return A character scalar containing the pinned source commit, with `upstream_url` and
#'   `fixtures_release_tag` attributes.
#' @usage
#' nvel_source_revision()
#' @export
#' @examples
#' ## Inspect the revision behind the shipped example equations
#' nvel_source_revision()
nvel_source_revision <- function() {
  structure(.nvel_source_commit,
    upstream_url = .nvel_upstream_url,
    fixtures_release_tag = .nvel_fixtures_release_tag
  )
}
