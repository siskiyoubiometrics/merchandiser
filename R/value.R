.mc_validate_price_scenarios <- function(price_scenarios, products, currency) {
  if (!is.data.frame(price_scenarios)) {
    stop(
      "price_scenarios must be a data frame. ",
      "Supply one row per scenario and product.", call. = FALSE
    )
  }
  required <- c("scenario", "product", "price")
  missing <- setdiff(required, names(price_scenarios))
  if (length(missing)) {
    stop("Missing price scenario column: ", missing[[1L]],
         ". Add the column and try again.", call. = FALSE)
  }
  allowed <- c(required, "price_quantity", "currency")
  unknown <- setdiff(names(price_scenarios), allowed)
  if (length(unknown)) {
    stop("Unknown price scenario column: ", unknown[[1L]],
         ". Remove it or rename it to a documented scenario field.",
         call. = FALSE)
  }
  for (name in intersect(
    c("scenario", "product", "currency"), names(price_scenarios)
  )) {
    if (is.factor(price_scenarios[[name]])) {
      price_scenarios[[name]] <- as.character(price_scenarios[[name]])
    }
  }
  if (!is.character(price_scenarios$scenario) ||
        !is.character(price_scenarios$product) ||
        !.mc_numeric(price_scenarios$price)) {
    stop(
      "Scenario and product must be text and price must be numeric. ",
      "Correct those columns and try again.", call. = FALSE
    )
  }
  price_scenarios$price <- as.double(price_scenarios$price)
  if (anyNA(price_scenarios[c("scenario", "product", "price")]) ||
        any(!nzchar(price_scenarios$scenario)) ||
        any(!nzchar(price_scenarios$product)) ||
        any(!is.finite(price_scenarios$price)) ||
        any(price_scenarios$price < 0)) {
    stop(
      "Price scenarios contain a missing or invalid value. ",
      "Fill every required field with a valid value and try again.",
      call. = FALSE
    )
  }
  if (!"price_quantity" %in% names(price_scenarios)) {
    price_scenarios$price_quantity <- products$price_quantity[
      match(price_scenarios$product, products$product)
    ]
  }
  if (.mc_numeric(price_scenarios$price_quantity)) {
    price_scenarios$price_quantity <- as.double(price_scenarios$price_quantity)
  }
  if (!is.double(price_scenarios$price_quantity) ||
        anyNA(price_scenarios$price_quantity) ||
        any(!is.finite(price_scenarios$price_quantity)) ||
        any(price_scenarios$price_quantity <= 0)) {
    stop(
      "price_quantity must contain finite positive numbers. ",
      "Correct the quantities and try again.", call. = FALSE
    )
  }
  if (!"currency" %in% names(price_scenarios)) {
    if (is.null(currency)) {
      stop(
        "currency is required in the scenario table or base run. ",
        "Supply one currency label and try again.", call. = FALSE
      )
    }
    price_scenarios$currency <- currency
  }
  if (!is.character(price_scenarios$currency) ||
        anyNA(price_scenarios$currency) ||
        any(!nzchar(price_scenarios$currency))) {
    stop(
      "currency must contain nonempty text values. ",
      "Supply one currency label per scenario row.", call. = FALSE
    )
  }
  key <- paste(price_scenarios$scenario, price_scenarios$product, sep = "\034")
  if (anyDuplicated(key)) {
    stop(
      "Each scenario and product pair must be unique. ",
      "Remove duplicate pairs and try again.", call. = FALSE
    )
  }
  scenarios <- unique(price_scenarios$scenario)
  if (!length(scenarios)) {
    stop(
      "price_scenarios must contain at least one scenario. ",
      "Add scenario rows and try again.", call. = FALSE
    )
  }
  for (scenario in scenarios) {
    rows <- price_scenarios$scenario == scenario
    if (!setequal(price_scenarios$product[rows], products$product)) {
      stop(
        "Every scenario must price every product exactly once. ",
        "Add or remove rows so each scenario is complete.", call. = FALSE
      )
    }
    if (length(unique(price_scenarios$currency[rows])) != 1L) {
      stop(
        "Each scenario must use one currency. ",
        "Make its currency values agree and try again.", call. = FALSE
      )
    }
  }
  price_scenarios
}

#' Compare log values as cut and with trees bucked again
#'
#' Reprice existing cuts and optimize the trees again for each price scenario. Return both
#' values and their difference, with the new results attached.
#'
#' @param x Required complete `merch_result` from [merchandise()] or
#'   [optimize_bucking()], with fields described in [Result tables][result_tables]. No default.
#' Missing or other objects are errors.
#'
#' Use a saved merchandising result.
#' @param price_scenarios Required data frame with one row per product and scenario,
#'   with `scenario`, `product`, and `price`. No default. Missing is an error.
#'
#'  Optional columns are `price_quantity` and
#' `currency`. Every scenario must
#'   contain every product exactly once.
#'
#' @return A data frame with the value of the logs as cut, the value with
#'   trees bucked again, and the difference for each price list.
#' @export
#' @usage
#'
#' ## Call signatures
#' compare_bucking_prices(x, price_scenarios)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Compare cut values under an example price scenario
#' compare_bucking_prices(x = merchandise(dbh = example_trees$dbh,
#'                                        ht = example_trees$ht,
#'                                        species = example_trees$species,
#'                                        status = TRUE),
#'                        price_scenarios = example_products(name = 'pnw') %>%
#'                          transmute(scenario = 'example',
#'                                    product,
#'                                    price = 10,
#'                                    currency = 'USD'))
#' @details Compare scenarios before making later percentage deductions.
#' Local equations used in the saved result must remain registered.
#' @section Price scenario fields and ordering:
#' Supply one row for every product in each scenario. `scenario` and
#' `product` are required nonempty character labels, unitless, for example `'pulp_high'` and
#' `'pulp'`. Factor labels are accepted.
#'
#' `price` is required finite nonnegative numeric currency per `price_quantity` measurement
#' units, for example `8`. Missing required values and duplicate pairs are errors.
#'
#' Optional numeric `price_quantity` must be finite and positive. If omitted, it uses the
#' product table's quantity. Missing values are errors.
#'
#' Example: `1000` for a price per thousand of the stated measurement unit. Optional character
#' `currency` uses the original call's label when omitted, for example `'USD'` for United
#' States dollars. It must be present in either place, nonempty, nonmissing, and the same
#' throughout a scenario.
#'
#' The logs as cut are valued first. The trees are then bucked again with those prices and a
#' value objective. This requires local models to remain registered.
#'
#' Comparison must precede later percentage deductions.
#' @section Returned comparison fields:
#' Character `scenario` and `currency` retain the supplied labels. Numeric `repriced_value` totals
#' the existing cuts, and `rebucked_value` totals the newly selected cuts. `rebuck_gain` is their
#' difference.
#'
#' Values use each scenario's currency. No currency conversion
#' or inventory expansion is performed.
#'
#' The named `rebucked_results` attribute retains a complete result for each scenario, with
#' tables described in [Result tables][result_tables].
#' @section Status and missing values:
#' Invalid scenarios stop the call. Missing scaled quantities propagate to value sums. An empty
#' log table sums to zero, which alone does not prove a valid result with no logs.
#'
#' Review tree statuses in the original and each saved result, including `no_feasible_log` for a
#' valid
#' result with no logs and other codes for failed trees. The comparison adds no status column
#' of its own.
#' @seealso [optimize_bucking()] for one price table, [product_summary()] for
#' grouping an existing result, [apply_defect_pct()] for later deductions.
compare_bucking_prices <- function(x, price_scenarios) {
  if (!inherits(x, "merch_result")) {
    stop(
      "x must be a merch_result. Pass the object returned by ",
      "merchandise() or optimize_bucking().", call. = FALSE
    )
  }
  replay <- x$run_metadata$call
  if (is.null(replay) || is.null(replay$products)) {
    stop(
      "x does not retain the inputs needed for trees to be bucked again. ",
      "Rerun merchandise() and compare that result.", call. = FALSE
    )
  }
  if (nrow(x$run_metadata$posthoc_calls)) {
    stop(
      "Price comparison must happen before post-hoc deductions. ",
      "Compare the original merchandise result instead.", call. = FALSE
    )
  }
  price_scenarios <- .mc_validate_price_scenarios(
    price_scenarios, replay$products, replay$currency
  )
  scenarios <- unique(price_scenarios$scenario)
  rebucked <- stats::setNames(vector("list", length(scenarios)), scenarios)
  rows <- vector("list", length(scenarios))
  for (at in seq_along(scenarios)) {
    scenario <- scenarios[[at]]
    table <- price_scenarios[
      price_scenarios$scenario == scenario, , drop = FALSE
    ]
    product_at <- match(replay$products$product, table$product)
    scenario_products <- replay$products
    scenario_products$price <- table$price[product_at]
    scenario_products$price_quantity <- table$price_quantity[product_at]
    log_at <- match(x$logs$product, table$product)
    repriced <- sum(
      x$logs$net_scale * table$price[log_at] /
        table$price_quantity[log_at]
    )
    arguments <- replay
    arguments$products <- scenario_products
    arguments$currency <- unique(table$currency)
    arguments$objective <- "value"
    arguments$unpriced <- "error"
    rebucked[[at]] <- do.call(optimize_bucking, arguments)
    rebucked_value <- sum(rebucked[[at]]$values$net_value)
    rows[[at]] <- data.frame(
      scenario = scenario, currency = unique(table$currency),
      repriced_value = repriced, rebucked_value = rebucked_value,
      rebuck_gain = rebucked_value - repriced,
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "rebucked_results") <- rebucked
  class(result) <- c("bucking_price_comparison", "data.frame")
  result
}
