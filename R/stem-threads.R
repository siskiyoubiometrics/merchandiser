.tv_runtime <- local({
  runtime <- new.env(parent = emptyenv())
  runtime$threads <- 1L
  runtime
})

#' Choose the number of calculation workers
#'
#' @param n The worker count controls how many threads may evaluate trees. Numeric scalar, number
#'   of threads, a whole number of at least one. Counts above the detected core count
#'   are clamped with a warning. Default: \code{NULL}.
#' @return The current worker count, or the previous worker count when `n` is supplied.
#' @usage
#' threads(
#'   n = NULL
#' )
#' @export
#' @examples
#' ## Read the current worker setting before changing it.
#' previous <- threads()
#'
#' ## Use one worker for a local calculation.
#' threads(n = 1)
#'
#' ## Restore the original setting.
#' threads(n = previous)
threads <- function(n = NULL) {
  if (is.null(n)) {
    return(.tv_runtime$threads)
  }
  n <- .validate_thread_count(n, "n")
  old <- .tv_runtime$threads
  .tv_runtime$threads <- n
  old
}

.validate_thread_count <- function(n, name) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != floor(n)) {
    stop(name, " must be one integer of at least one.", call. = FALSE)
  }
  cores <- parallel::detectCores()
  if (is.na(cores))
    cores <- 1L
  if (n > cores) {
    warning(name, " exceeds the core count and was clamped to ", cores, ".", call. = FALSE)
    n <- cores
  }
  as.integer(n)
}

#' Evaluate code with a temporary merchandiser thread count
#'
#' @param n The worker count controls how many threads may evaluate trees. Numeric scalar, number
#'   of threads, a whole number of at least one. Counts above the detected core count
#'   are clamped with a warning. Required, with no default.
#' @param code The expression runs with the requested worker count and then restores the previous
#'   count. Unevaluated R expression. Required, with no default.
#' @return The value of code, with the previous worker count restored afterward.
#' @usage
#' with_threads(
#'   n,
#'   code
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure an example stem with temporary workers
#' with_threads(n = 4,
#'              code = dib(dbh = example_trees$dbh[1],
#'                         ht = example_trees$ht[1],
#'                         h = 20,
#'                         spcd = example_trees$spcd[1],
#'                         model = example_trees$model[1])) %>%
#'   rename(`diameter inside bark (inches)` = value)
with_threads <- function(n, code) {
  if (is.null(n))
    stop("n must be one integer of at least one.", call. = FALSE)
  old <- threads(n)
  on.exit(threads(old), add = TRUE)
  force(code)
}
