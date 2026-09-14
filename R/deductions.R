#' Deduct a percentage from current net volume
#'
#' Apply percentage deductions to a numeric vector or merchandising result. Cuts stay fixed,
#' and each deduction compounds on current net quantities.
#'
#' @param x Required numeric vector or complete `merch_result` from
#'   [merchandise()]. For a vector, units are unchanged, and missing quantities
#'   remain missing. For a result, the percentage follows log rows.
#'
#' Missing objects and other
#' types are errors.
#' @param pct Required finite numeric percentage from zero through 100.
#'   Supply one value to repeat or exactly one per numeric input or result log.
#'   Missing values and incompatible lengths are errors.
#' @param kind Required nonempty character label of length one, such as
#'   `'hidden_defect'`. Missing values are errors. Unitless. With a numeric
#'   vector it is validated but no record is attached.
#'
#' @details Each call multiplies current net quantities by `1 - pct / 100`. Repeated calls
#' compound, so two deductions do not add their percentages. Gross quantities, located deductions,
#' and cut positions remain unchanged.
#'
#' Net physical volume, scaled quantity, and value are updated for each log
#'   and summarized by tree. [Result tables][result_tables] describes the
#'   retained deduction record. Keep a supplied vector of varying percentages
#'   because that record cannot store it as one scalar percentage.
#' @return The numeric vector in its original units, or an updated
#' `merch_result` with the result tables described in [Result tables][result_tables]. No new
#'   status is
#'   assigned and existing status columns are retained.
#' @section Status and missing values:
#' Missing quantities remain missing. An invalid percentage stops the call. A 100 percent deduction
#' gives zero for an available quantity.
#'
#' This is a
#' complete deduction of selected volume, not `no_feasible_log`, which means no
#' acceptable log was selected. Review result statuses before summing.
#' @seealso [defect()] for located restrictions, [defect_pct_from_thirds()]
#'   for volume-weighted percentages, [product_summary()] for totals.
#' @export
#' @usage
#'
#' ## Call signatures
#' apply_defect_pct(x, pct, kind)
#' @examples
#' ## Calculate logs for the shipped inventory
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       model = example_trees$model,
#'                       species = example_trees$species,
#'                       products = example_products(name = 'pnw'),
#'                       status = TRUE)
#'
#' ## Apply a later percentage deduction
#' reduced <- apply_defect_pct(x = result, pct = 5, kind = 'unlocated')
#'
#' ## Inspect the updated totals
#' product_summary(x = reduced, table = 'trees')
apply_defect_pct <- function(x, pct, kind) {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) || !nzchar(kind)) {
    stop(
      "kind must be one nonempty character value. ",
      "Supply a short audit label and try again.",
      call. = FALSE
    )
  }
  if (is.numeric(x) && !inherits(x, "merch_result")) {
    pct <- .mc_as_double(pct, "pct")
    pct <- .mc_recycle(pct, length(x), "pct")
    if (anyNA(pct) || any(!is.finite(pct)) || any(pct < 0 | pct > 100)) {
      stop(
        "pct must be from zero through 100. ",
        "Correct the percentage and try again.", call. = FALSE
      )
    }
    return(x * (1 - pct / 100))
  }
  if (!inherits(x, "merch_result")) {
    stop(
      "x must be a merch_result. Pass the object returned by ",
      "merchandise() or optimize_bucking().", call. = FALSE
    )
  }
  pct <- .mc_as_double(pct, "pct")
  pct <- .mc_recycle(pct, nrow(x$logs), "pct")
  if (anyNA(pct) || any(!is.finite(pct)) || any(pct < 0 | pct > 100)) {
    stop("pct must be from zero through 100. Correct the percentage and try again.", call. = FALSE)
  }
  factor <- 1 - pct / 100
  previous_piece <- x$logs$log_net_cubic_ib
  previous_piece_ob <- x$logs$log_net_cubic_ob
  previous_sale <- x$logs$net_scale
  x$logs$log_net_cubic_ib <- previous_piece * factor
  x$logs$posthoc_deduction_cubic_ib <-
    x$logs$posthoc_deduction_cubic_ib + previous_piece - x$logs$log_net_cubic_ib
  x$logs$log_net_cubic_ob <- previous_piece_ob * factor
  x$logs$posthoc_deduction_cubic_ob <-
    x$logs$posthoc_deduction_cubic_ob + previous_piece_ob -
    x$logs$log_net_cubic_ob
  x$logs$net_scale <- previous_sale * factor
  log_key <- paste(x$logs$id, x$logs$log, sep = "\034")
  if (nrow(x$scales)) {
    scale_key <- paste(x$scales$id, x$scales$log, sep = "\034")
    scale_factor <- factor[match(scale_key, log_key)]
    previous <- x$scales$net
    x$scales$net <- previous * scale_factor
    x$scales$posthoc_deduction <-
      x$scales$posthoc_deduction + previous - x$scales$net
  }
  if (nrow(x$values)) {
    value_key <- paste(x$values$id, x$values$log, sep = "\034")
    value_factor <- factor[match(value_key, log_key)]
    previous <- x$values$net_value
    x$values$net_value <- previous * value_factor
    x$values$posthoc_deduction_value <-
      x$values$posthoc_deduction_value + previous - x$values$net_value
    if ("net_value" %in% names(x$logs)) {
      x$logs$net_value <- x$values$net_value[
        match(log_key, value_key)
      ]
      x$logs$posthoc_deduction_value <-
        x$values$posthoc_deduction_value[match(log_key, value_key)]
    }
  }
  stem_index <- match(x$logs$id, x$trees$id)
  stem_count <- nrow(x$trees)
  x$trees$log_net_cubic_ib <- .mc_sum_by_index(
    x$logs$log_net_cubic_ib, stem_index, stem_count
  )
  x$trees$posthoc_deduction_cubic_ib <- .mc_sum_by_index(
    x$logs$posthoc_deduction_cubic_ib, stem_index, stem_count
  )
  x$trees$log_net_cubic_ob <- .mc_sum_by_index(
    x$logs$log_net_cubic_ob, stem_index, stem_count,
    require_defined = TRUE
  )
  x$trees$posthoc_deduction_cubic_ob <- .mc_sum_by_index(
    x$logs$posthoc_deduction_cubic_ob, stem_index, stem_count,
    require_defined = TRUE
  )
  if ("net_value" %in% names(x$trees)) {
    value_index <- match(x$values$id, x$trees$id)
    x$trees$net_value <- .mc_sum_by_index(
      x$values$net_value, value_index, stem_count, require_defined = TRUE
    )
    x$trees$posthoc_deduction_value <- .mc_sum_by_index(
      x$values$posthoc_deduction_value, value_index, stem_count,
      require_defined = TRUE
    )
  }
  x$defect_accounting <- data.frame(
    id = x$logs$id, log = x$logs$log,
    located_deduction_cubic_ib = x$logs$located_deduction_cubic_ib,
    posthoc_deduction_cubic_ib = x$logs$posthoc_deduction_cubic_ib,
    located_deduction_cubic_ob = x$logs$located_deduction_cubic_ob,
    posthoc_deduction_cubic_ob = x$logs$posthoc_deduction_cubic_ob,
    stringsAsFactors = FALSE
  )
  calls <- x$run_metadata$posthoc_calls
  call_seq <- if (nrow(calls)) max(calls$call_seq) + 1L else 1L
  audit_pct <- if (length(unique(pct)) == 1L) unique(pct) else NA_real_
  x$run_metadata$posthoc_calls <- rbind(
    calls,
    data.frame(call_seq = call_seq, kind = kind, percent = audit_pct,
               stringsAsFactors = FALSE)
  )
  x
}
