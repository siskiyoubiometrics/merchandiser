.tv_runtime <- local({
  runtime <- new.env(parent = emptyenv())
  runtime$threads <- 1L
  runtime
})

#' Choose the number of calculation workers
#'
#' Query or set the worker count for compiled calculations. Setting the count returns the
#' previous value.
#'
#' @param n One positive whole-number numeric count, no greater than R's
#'   integer limit, or default `NULL` to query without changing the count.
#'   Missing, infinite, fractional, or zero values are errors. Unitless.
#'
#' @details The initial setting is one unless `MERCHANDISER_THREADS` supplies
#'   a valid count when the package loads. The older `TREEVOLUME_THREADS`
#'   setting is used only when the current setting is unset. Changing the
#'   worker count does not change product specifications or measurement rules.
#' @return One integer count, unitless. Setting returns the previous count,
#'   and querying returns the current count.
#' @section Status and missing values:
#' No tree codes are returned. Invalid counts stop the call.
#' @seealso [with_threads()] for a temporary setting, [dib()] for diameter.
#' @export
#' @usage
#'
#' ## Call signatures
#' threads(n = NULL)
#' @examples
#' ## Inspect the worker count used for inventory calculations
#' threads()
threads <- function(n = NULL) {
  if (is.null(n)) {
    return(.tv_runtime$threads)
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
        n < 1 || n > .Machine$integer.max || n != floor(n)) {
    stop("n must be one integer of at least one.", call. = FALSE)
  }
  old <- .tv_runtime$threads
  .tv_runtime$threads <- as.integer(n)
  old
}

#' Evaluate code with a temporary merchandiser thread count
#'
#' Evaluate an expression with a temporary worker count and restore the previous count on exit.
#'
#' @param n Required positive whole-number worker count of length one.
#'   Missing is an error. Unitless.
#' @param code Required R expression evaluated with `n` workers. Its return
#'   type, length, units, and missing values depend on the expression.
#'   Example: `sum(example_trees$dbh)`. Omission is an error.
#'
#' @return The value of `code`.
#' @export
#' @usage
#'
#' ## Call signatures
#' with_threads(n, code)
#' @examples
#' ## Evaluate inventory diameters with a temporary worker count
#' with_threads(n = 1,
#'              code = dib(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         h = 20,
#'                         model = example_trees$model))
#' @section Worker count and missing values:
#' The count is one positive whole number no greater than R's integer limit, unitless. Missing,
#' infinite, fractional, or zero values are errors. Built-in calculations can use this many
#' workers.
#'
#' Equations written in R run on the main R process. A worker count does not change product
#' rules.
#' @section Status and missing values:
#' These functions produce no tree status codes.
#' Invalid counts stop the call.
#'
#' `with_threads()` restores the previous count even when the evaluated code fails. Missing
#' values returned by that code are passed through unchanged.
#' @seealso [threads()] to inspect
#' or set the session count, [with_threads()] for a temporary count, [merchandise()] for tree
#' calculations.
with_threads <- function(n, code) {
  old <- threads(n)
  on.exit(threads(old), add = TRUE)
  force(code)
}
