NULL

.nvel_source_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
.nvel_upstream_url <- "https://github.com/FMSC-Measurements/VolumeLibrary"
.nvel_fixtures_release_tag <- "v0.1.0"

#' Record the source version used for the shipped equations
#'
#' @return A single character source revision, unitless. Character attributes   `upstream_url` and
#'   `fixtures_release_tag` record the source repository   address and saved reference-test
#'   release label. Neither is a tree input.
#' @usage
#' nvel_source_revision()
#' @export
#' @examples
#' ## Record the source revision used by this package.
#' nvel_source_revision()
nvel_source_revision <- function() {
  structure(.nvel_source_commit,
    upstream_url = .nvel_upstream_url,
    fixtures_release_tag = .nvel_fixtures_release_tag
  )
}
