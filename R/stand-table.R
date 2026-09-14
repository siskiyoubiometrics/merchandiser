.mc_stand_status <- function(x) {
  if (!is.null(x$trees$status)) return(x$trees$status)
  call <- x$run_metadata$call
  call$status <- TRUE
  if (identical(x$run_metadata$algorithm, "cascade")) {
    fun <- merchandise
  } else {
    fun <- optimize_bucking
    call$objective <- x$run_metadata$objective
    call$unpriced <- x$run_metadata$unpriced
  }
  do.call(fun, call)$trees$status
}

#' Expand an inventory into a stand table
#'
#' Expand tree counts, basal area, log counts, and product quantities by inventory group.
#' Retain failures and inventory trees absent from the result.
#'
#' @param x One result from [merchandise()] or [optimize_bucking()]. Required. Missing is an error.
#'
#' `trees$id` and `logs$id` identify trees. Log fields
#'   `product` (unitless label), `net_scale` (net quantity), and `scale_unit`
#'   (quantity unit) provide volume. Other result fields are unchanged.
#'
#'  Use `status = TRUE` when creating the result.
#' @param trees Required data frame with one row per caller tree. `id` must
#'   contain unique, nonmissing identifiers matching every result tree. Additional caller trees are
#' retained as `not_in_result`.
#'
#' `dbh` is numeric
#'   diameter at breast height outside bark, in inches for an imperial
#'   result or centimeters for a metric result. Missing, nonfinite, or
#'   nonpositive diameters retain tree counts but make their group's basal
#'   area missing. `species` supplies grouping labels, or `spcd` supplies
#'   species codes if `species` is absent.
#'
#' These are unitless. Other columns
#'   requested in `by` must be atomic grouping labels. Missing group labels
#'   form their own group.
#'
#' Example: `example_trees`. Omission is an error.
#' @param expansion Required numeric vector of finite, nonnegative trees per
#'   area represented by each record. Length one is recycled, otherwise
#'   length must equal `nrow(trees)` in caller row order, not result order. Missing values are
#' errors.
#'
#' Example: `expansion = 5` for trees per acre
#'   when `area_unit = 'acre'`. No expansion is inferred from the inventory.
#' @param by Character vector of unique grouping column names. Default
#'   `c('product', 'species', 'dbh_class')`. `product` comes from the logs,
#'   `species` from the caller inventory, and `dbh_class` is calculated.
#'
#' Other names refer to caller columns. `character()` gives totals by
#'   volume status. Missing, unknown, or output column names are errors.
#'
#' Example: `by = c('species', 'dbh_class')`.
#' @param class_width One finite positive numeric diameter class width. Default `2`, in inches for
#' imperial results or centimeters for metric
#'   results. Classes are left-closed intervals with lower boundary
#'   `floor(dbh / class_width) * class_width`, reported in `dbh_class`.
#'
#' Missing values are errors.
#' @param area_unit One character label, `'acre'` (default) or `'hectare'`. Labels the supplied
#' expansion, it does not convert expansion factors. Missing or unknown values are errors.
#'
#' Example: `area_unit = 'hectare'`.
#' @param na_action One character value, `'warn'` (default) or `'propagate'`. Both propagate
#'   missing quantities. `'warn'` emits one consolidated
#'   warning for entirely missing quantities.
#'
#' Explicit `'propagate'` stays
#'   silent. Missing or unknown values are errors. Unitless.
#'
#' Example: `na_action = 'propagate'`.
#'
#' @details The function returns weighted sums without variance estimates. Expansion factors are
#' used as supplied.
#'
#' A tree contributing several logs to a product is counted once in that product. A tree
#' contributing to several products occurs in each product group. Do not add product tree
#' counts or basal areas to obtain stand totals.
#'
#' Omit `product` from `by` to count each tree once within its group. Scaled quantities with
#' different units have separate columns. Quantities sharing a unit are added, so use
#' comparable product scale and bark rules.
#'
#' @section Status and missing values:
#' `merchandising_status` always separates `valid`, `failed`, and `not_in_result`. `ok` is
#' valid volume. `no_feasible_log`, `no_feasible_log`, is valid zero volume.
#'
#' Other statuses are failures. Inspect `attr(output, 'failed_trees')` and [status_codes()] to
#' repair those trees before interpreting the scaled total. When result statuses were omitted,
#' the saved calculation is rerun with statuses, requiring its models to remain registered.
#'
#' Trees without logs have a missing `product` group label. Valid zero volume contributes zero
#' logs and quantity. Failed or absent trees contribute missing volume.
#'
#' Any missing quantity in an applicable group propagates, so incomplete volume is never
#' reported as zero. When every contributing value for a quantity is missing, one consolidated
#' warning names each affected quantity and group, including failed and absent volume groups.
#' Explicit `na_action = 'propagate'` stays silent.
#'
#' Empty sums for valid trees without applicable logs remain zero.
#'
#' @return A data frame with the requested grouping columns and
#'   `merchandising_status`, plus numeric weighted sums. `trees_per_acre` and
#'   `failed_trees_per_acre` count all and failed trees respectively. `basal_area_ft2_per_acre`
#' uses square feet for imperial input,
#'   `basal_area_m2_per_acre` uses square meters for metric input.
#'
#' `logs_per_acre` counts logs. `net_scale_<unit>_per_acre` holds net scale
#'   quantities in each result measurement unit, for example `net_scale_ft3_per_acre`. With
#' `area_unit = 'hectare'`, every suffix is `_per_hectare` instead.
#'
#' Units are sanitized as column names. If no logs exist, units come from
#'   the saved product table. Counts include expansion, not raw record counts.
#'
#' The `failed_trees` attribute is a data frame with `id`, integer `status`
#'   (missing for absent result trees), and `expansion` in trees per area.
#'
#' The same attributes and
#' columns are returned by [stock_table()].
#' @seealso [stock_table()] for product quantities by diameter class,
#'   [product_summary()] for totals from a selected result table,
#'   [merchandise()] to calculate the logs first.
#' @export
#' @usage
#'
#' ## Call signatures
#' stand_table(x, trees, expansion, by = c('product', 'species', 'dbh_class'), class_width = 2,
#'   area_unit = 'acre', na_action = 'warn')
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Compile the example inventory with supplied expansion
#' stand_table(x = merchandise(dbh = example_trees$dbh,
#'                             ht = example_trees$ht,
#'                             species = example_trees$species,
#'                             products = example_products(name = 'pnw'),
#'                             status = TRUE),
#'             trees = example_trees %>%
#'             mutate(id = tree),
#'             expansion = 5,
#'             by = 'species')
stand_table <- function(x, trees, expansion,
                        by = c("product", "species", "dbh_class"),
                        class_width = 2, area_unit = "acre", na_action = "warn") {
  na_action <- .mc_scalar_choice(na_action, "na_action", c("warn", "propagate"))
  if (!inherits(x, "merch_result")) stop("x must be a merchandise result.", call. = FALSE)
  if (!is.data.frame(trees) || !all(c("id", "dbh") %in% names(trees)) ||
        anyNA(trees$id) || anyDuplicated(trees$id) || anyDuplicated(x$trees$id) ||
        any(!x$trees$id %in% trees$id) || !is.numeric(trees$dbh)) {
    stop("trees must have unique id and numeric dbh columns matching the result.", call. = FALSE)
  }
  if (missing(expansion)) stop("Supply expansion in trees per area.", call. = FALSE)
  expansion <- .mc_recycle(expansion, nrow(trees), "expansion")
  if (!is.numeric(expansion) || any(!is.finite(expansion)) || any(expansion < 0)) {
    stop("expansion must contain finite nonnegative numbers.", call. = FALSE)
  }
  area_unit <- .mc_scalar_choice(area_unit, "area_unit", c("acre", "hectare"))
  if (!is.numeric(class_width) || length(class_width) != 1 ||
        !is.finite(class_width) || class_width <= 0) {
    stop("class_width must be one finite positive number.", call. = FALSE)
  }
  if (!"species" %in% names(trees)) trees$species <- trees$spcd
  diameter <- trees$dbh
  diameter[!is.finite(diameter) | diameter <= 0] <- NA_real_
  trees$dbh_class <- floor(diameter / class_width) * class_width
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) ||
        any(!by %in% c("product", names(trees))) || "merchandising_status" %in% by) {
    stop("by must name distinct tree columns, product, species, or dbh_class.", call. = FALSE)
  }
  status <- .mc_stand_status(x)[match(trees$id, x$trees$id)]
  valid <- status %in% c(0, 410)
  volume <- ifelse(is.na(status), "not_in_result", ifelse(valid, "valid", "failed"))
  imperial <- identical(x$run_metadata$units, "imperial")
  basal <- pi * (diameter / if (imperial) 24 else 200)^2
  logs <- x$logs
  units <- unique(c(as.character(logs$scale_unit),
                    as.character(x$run_metadata$call$products$scale_unit)))
  units <- units[!is.na(units)]
  sale_names <- paste0("net_scale_", make.names(units), "_per_", area_unit)
  if (anyDuplicated(sale_names)) {
    stop("Measurement units produce duplicate column names.", call. = FALSE)
  }
  log_row <- match(logs$id, trees$id)
  no_logs <- which(!seq_len(nrow(trees)) %in% log_row)
  index <- c(log_row, no_logs)
  joined <- trees[index, setdiff(by, "product"), drop = FALSE]
  if ("product" %in% by) joined$product <- c(as.character(logs$product), rep(NA, length(no_logs)))
  joined$merchandising_status <- volume[index]
  group_names <- c(by, "merchandising_status")
  keys <- lapply(joined[group_names], function(value) match(value, unique(value)))
  key <- do.call(paste, c(keys, sep = "\r"))
  count_names <- paste0(c("trees", "failed_trees", if (imperial) "basal_area_ft2" else
                            "basal_area_m2", "logs"), "_per_", area_unit)
  if (any(by %in% c(count_names, sale_names))) {
    stop("by must not reuse a summary output column name.", call. = FALSE)
  }
  missing <- .mc_missing_summary()
  rows <- lapply(split(seq_along(index), key), function(selected) {
    tree_rows <- unique(index[selected])
    log_rows <- selected[selected <= nrow(logs)]
    all_valid <- all(valid[tree_rows])
    labels <- joined[selected[1], group_names, drop = FALSE]
    missing$record(basal[tree_rows] * expansion[tree_rows], count_names[3], labels)
    if (!all_valid) missing$record(NA_real_, count_names[4], labels)
    counts <- c(sum(expansion[tree_rows]), sum(expansion[tree_rows[!valid[tree_rows]]]),
                sum(basal[tree_rows] * expansion[tree_rows]),
                if (all_valid) sum(expansion[log_row[log_rows]]) else NA_real_)
    quantities <- vapply(units, function(unit) {
      name <- sale_names[match(unit, units)]
      if (!all_valid) {
        missing$record(NA_real_, name, labels)
        return(NA_real_)
      }
      at <- log_rows[logs$scale_unit[log_rows] == unit]
      value <- logs$net_scale[at] * expansion[log_row[at]]
      missing$record(value, name, labels)
      sum(value)
    }, numeric(1))
    values <- as.data.frame(as.list(stats::setNames(c(counts, quantities),
                                                    c(count_names, sale_names))))
    cbind(joined[selected[1], group_names, drop = FALSE], values)
  })
  if (length(rows)) {
    result <- do.call(rbind, rows)
    rownames(result) <- NULL
  } else {
    result <- joined[FALSE, group_names, drop = FALSE]
    for (name in c(count_names, sale_names)) result[[name]] <- numeric()
  }
  attr(result, "failed_trees") <- data.frame(
    id = trees$id[!valid], status = status[!valid], expansion = expansion[!valid]
  )
  missing$warn(na_action)
  result
}

#' Report product stock by diameter class
#'
#' Expand an inventory with product and diameter-class grouping by default. Return the same
#' fields and status groups as [stand_table()].
#'
#' @inheritParams stand_table
#' @param by Character vector of distinct grouping names, default
#'   `c('product', 'dbh_class')`. Unitless. Example: `by = 'product'`.
#'
#' Missing, unknown, or output names are errors. See [stand_table()] for grouping
#'   definitions and the behavior of `character()`.
#' @return The [stand_table()] data frame contains weighted tree counts, basal area, log counts,
#' and net scaled quantities. It separates volume statuses and retains a `failed_trees` attribute.
#' @section Status and missing values:
#'   Failures are separate from valid volume. Trees without logs remain in
#'   counts. Failed and absent trees have missing volume quantities.
#'
#' See
#'   [stand_table()] for the complete field and status definitions. One consolidated warning names
#' each entirely missing quantity and its
#'   group. Explicit `na_action = 'propagate'` suppresses that warning.
#'
#' Partially missing quantities still propagate under either choice.
#' @seealso [stand_table()] for tree density and basal area,
#'   [product_summary()] for totals without the complete-inventory join.
#' @export
#' @usage
#'
#' ## Call signatures
#' stock_table(x, trees, expansion, by = c('product', 'dbh_class'), class_width = 2, area_unit =
#'   'acre', na_action = 'warn')
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Compile the example inventory with supplied expansion
#' stock_table(x = merchandise(dbh = example_trees$dbh,
#'                             ht = example_trees$ht,
#'                             species = example_trees$species,
#'                             products = example_products(name = 'pnw'),
#'                             status = TRUE),
#'             trees = example_trees %>%
#'             mutate(id = tree),
#'             expansion = 5,
#'             by = 'species')
stock_table <- function(x, trees, expansion, by = c("product", "dbh_class"),
                        class_width = 2, area_unit = "acre", na_action = "warn") {
  stand_table(x, trees, expansion, by, class_width, area_unit, na_action)
}
