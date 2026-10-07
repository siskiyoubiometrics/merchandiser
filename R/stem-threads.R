.tv_runtime <- local({
  runtime <- new.env(parent = emptyenv())
  runtime$threads <- 1L
  runtime
})

#' Inspect or set the calculation thread limit
#'
#' Control the worker limit used by parallel tree calculations. Initialization reads
#'   MERCHANDISER_THREADS, then TREEVOLUME_THREADS, and otherwise starts with one worker.
#'
#' @param n Requested thread count as one finite whole number of at least one. Defaults to `NULL`
#'   to report the current count. Larger requests are limited to the available cores, and builds
#'   without parallel support use one.
#' @return With `n = NULL`, the current integer thread limit. With a count supplied, the previous
#'   limit after setting the new one. Counts are limited to available cores, and nonparallel
#'   builds use one worker.
#' @usage
#' threads(
#'   n = NULL
#' )
#' @export
#' @examples
#' ## Inspect the active calculation limit
#' threads()
#'
#' ## Measure the example trees under a temporary single-worker limit
#' with_threads(n = 1,
#'              code = dib(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         h = 20,
#'                         spcd = example_trees$spcd))
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

#' Evaluate a calculation with a temporary thread limit
#'
#' Set a worker limit for one expression and restore the previous setting afterward, including
#'   when evaluation fails. Use this wrapper to limit workers within a larger script.
#'
#' @param n Requested thread count as one finite whole number of at least one. Required, without
#'   a default. Larger requests are limited to the available cores, and builds without parallel
#'   support use one.
#' @param code Expression to evaluate under the temporary limit. Required, without a default.
#'   Evaluation occurs in the calling environment.
#' @return The value returned by `code`, with its visibility preserved. The prior thread setting
#'   is restored.
#' @usage
#' with_threads(
#'   n,
#'   code
#' )
#' @export
#' @examples
#' ## Measure the example trees under a temporary single-worker limit
#' with_threads(n = 1,
#'              code = dib(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         h = 20,
#'                         spcd = example_trees$spcd))
with_threads <- function(n, code) {
  if (is.null(n))
    stop("n must be one integer of at least one.", call. = FALSE)
  old <- threads(n)
  on.exit(threads(old), add = TRUE)
  force(code)
}
