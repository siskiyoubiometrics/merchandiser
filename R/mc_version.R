#' Report the installed merchandiser package version
#'
#' Return the installed package version as a character value.
#'
#' @section Status and missing values:
#' No tree codes are returned.
#' @seealso [nvel_source_revision()] for source version, [assumptions()] for
#'   choices retained in a result.
#' @return A character scalar giving the installed package version, as
#'   reported by [utils::packageVersion()].
#' @keywords internal
#' @usage
#'
#' ## Call signatures
#' mc_version()
#' @examples
#' ## Query the installed package version
#' as.character(x = packageVersion(pkg = 'merchandiser'))
mc_version <- function() {
  as.character(utils::packageVersion("merchandiser"))
}
