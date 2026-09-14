.mc_missing_summary <- function() {
  missing <- character()
  list(
    record = function(value, quantity, labels) {
      if (length(value) && all(is.na(value))) {
        label_values <- vapply(labels, function(value) as.character(value)[1], character(1))
        group <- paste(paste(names(labels), label_values, sep = "="),
                       collapse = ", ")
        missing <<- c(missing, paste0(quantity, " [", group, "]"))
      }
    },
    warn = function(na_action) {
      if (na_action != "propagate" && length(missing)) {
        warning("All contributing values are missing for: ",
                paste(unique(missing), collapse = ", "), call. = FALSE)
      }
    }
  )
}

.mc_summary_groups <- function(x, group) {
  trees <- x$trees
  if (is.null(group)) return(data.frame(.all = rep("all", nrow(trees))))
  if (is.character(group)) {
    if (any(!group %in% names(trees))) {
      stop(
        "Every group name must be a tree column. ",
        "Choose names from x$trees and try again.", call. = FALSE
      )
    }
    return(trees[group])
  }
  if (!is.list(group) || is.null(names(group)) ||
        any(!nzchar(names(group))) || anyDuplicated(names(group))) {
    stop(
      "group must be NULL, tree-column names, or a uniquely named list. ",
      "Name each custom grouping vector and try again.", call. = FALSE
    )
  }
  size <- .mc_common_size(group)
  if (!size %in% c(1L, nrow(trees))) {
    stop(
      "Named group vectors must align to trees. ",
      "Use one value or one value per tree.", call. = FALSE
    )
  }
  as.data.frame(lapply(group, .mc_recycle, size = nrow(trees), name = "group"),
                stringsAsFactors = FALSE)
}

.mc_summary_quantities <- function(table, mode, basis) {
  if (mode == "trees") {
    names(table)[grepl("_cubic_(ib|ob)$|_value$|^log_count$", names(table))]
  } else if (mode == "logs") {
    suffix <- if (basis == "net") {
      c("log_net_cubic_ib", "log_net_cubic_ob", "net_scale")
    } else {
      c("log_gross_cubic_ib", "log_gross_cubic_ob", "gross_scale")
    }
    intersect(suffix, names(table))
  } else if (mode == "scales") {
    if (basis == "net") "net" else "gross"
  } else {
    if (basis == "net") "net_value" else "gross_value"
  }
}

#' Sum volume by product or tree group
#'
#' Sum quantities from a saved result by product or caller-supplied groups. Measurement units
#' and procedures remain explicit in the grouping fields.
#'
#' @param x Required complete `merch_result`, such as `result` below. Missing or
#' other objects are errors. Its tables and fields are described in [Result tables][result_tables]
#'   .
#' @param table One character name, `'trees'` (default), `'logs'`, `'scales'`, or
#'   `'values'`. Missing or unknown names are errors. Unitless.
#'
#' Example: `'logs'`.
#' @param group Default `NULL` gives no additional grouping. Otherwise supply a
#'   character vector of tree-column names or a uniquely named list of atomic
#'   vectors, each length one or one per result tree. Group labels are
#'   unitless.
#'
#' Example: `list(stand = 'A')`. Missing grouping labels are
#'   excluded by the grouping calculation, so resolve them before reporting.
#' @param expansion Finite nonnegative numeric weights, length one or exactly one
#'   per result tree in tree-table order. Default `1` sums the supplied records. Missing values are
#' errors.
#'
#' Example: `5` represents five trees per record
#'   in the analyst's stated population or area basis. No plot design is inferred.
#' @param basis One character value, `'net'` (default) or `'gross'`. It chooses log,
#'   scale, or value quantities. For `table = 'trees'`, all recognized cubic,
#'   value, and log-count fields are summed regardless of this choice.
#'
#' Missing choices are errors. Unitless. Example: `basis = 'gross'`.
#' @param na_action One character value, default `'exclude'`, excludes
#'   missing quantities when at least one quantity is observed. An entirely
#'   missing quantity returns `NA`, including under the default. `'propagate'` returns `NA` if any
#' quantity is missing.
#'
#' Missing or
#'   unknown choices are errors. Unitless. Example: `na_action = 'propagate'`.
#'
#' @section Status and missing values:
#'   An entirely missing requested quantity returns `NA`. One consolidated
#'   warning names each affected quantity and group. Explicit
#'   `na_action = 'propagate'` suppresses this warning and also propagates
#'   partially missing quantities. Counts describe tree calculation status,
#'   not the availability of individual quantities.
#'
#' @return A grouped data frame of weighted quantity totals and unweighted
#'   tree counts. `input_trees` counts distinct contributing tree identifiers,
#'   `valid_trees` counts completed and valid zero-log trees, `failed_trees` counts other
#'   statuses, and `valid_zero_log_trees` counts `no_feasible_log`.
#'
#' These count
#'   columns are record counts, not expanded trees per area.
#'
#' Use
#'   [stand_table()] to retain the complete inventory and expanded counts.
#' @export
#' @usage
#'
#' ## Call signatures
#' product_summary(x, table = 'trees', group = NULL, expansion = 1, basis = 'net', na_action =
#'   'exclude')
#' @examples
#' ## Calculate logs for the shipped tree list
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       model = example_trees$model,
#'                       species = example_trees$species,
#'                       products = example_products(name = 'pnw'),
#'                       status = TRUE)
#'
#' ## Inspect the result
#' product_summary(x = result, table = 'logs')
#' @section Grouping and returned totals:
#' Log groups always retain `product`, `scale_rule`, `measurement_quantity`, `scale_unit`, and
#' `scale_bark_basis`. Scale groups retain `scale_rule`, `unit`, and `bark_basis`. Named
#' reporting also retains `name` and `is_product`.
#'
#' Value groups retain `product` and `currency`. Requested group columns precede these labels.
#' Labels keep the meanings and units in [Result tables][result_tables].
#'
#' No rounding is applied to totals.
#'
#' For logs, net totals are `log_net_cubic_ib`, `log_net_cubic_ob`, and `net_scale`. Gross
#' totals use the corresponding `log_gross_cubic_ib`, `log_gross_cubic_ob`, and `gross_scale`.
#' Scales return `net` or `gross`, and values return `net_value` or `gross_value`.
#'
#' Tree summaries include every cubic, value, and `log_count` column present. Multiply the
#' source unit by the supplied expansion basis to interpret each total.
#'
#' The integer, unitless counts `input_trees`, `valid_trees`, `failed_trees`,
#' and `valid_zero_log_trees` count contributing records, not expanded trees. An empty source table
#' returns a data frame with no columns. If statuses
#' were omitted from the result, contributing trees are assumed completed.
#'
#' Create the result with `status = TRUE` when failure counts matter.
#' @seealso [stand_table()] for complete tree accounting, [product_summary_by_tree()]
#'   to calculate and sum logs in one call, [assumptions()] for saved choices.
product_summary <- function(x, table = "trees",
                            group = NULL, expansion = 1,
                            basis = "net", na_action = "exclude") {
  old_tables <- c(pieces = "logs", stems = "trees")
  if (is.character(table) && length(table) == 1L && table %in% names(old_tables)) {
    .merge_deprecated(paste0("table = ", table), paste0("table = ", old_tables[[table]]))
    table <- unname(old_tables[[table]])
  }
  if (!inherits(x, "merch_result")) {
    stop(
      "x must be a merch_result. Pass the object returned by ",
      "merchandise() or optimize_bucking().", call. = FALSE
    )
  }
  table <- .mc_scalar_choice(
    table, "table", c("trees", "logs", "scales", "values")
  )
  basis <- .mc_scalar_choice(basis, "basis", c("net", "gross"))
  na_action <- .mc_scalar_choice(
    na_action, "na_action", c("exclude", "propagate")
  )
  expansion <- .mc_recycle(expansion, nrow(x$trees), "expansion")
  if (.mc_numeric(expansion)) expansion <- as.double(expansion)
  if (!is.double(expansion) || anyNA(expansion) ||
        any(!is.finite(expansion)) || any(expansion < 0)) {
    stop(
      "expansion must contain finite nonnegative doubles or integers. ",
      "Supply one value or one value per tree and try again.", call. = FALSE
    )
  }
  stem_groups <- .mc_summary_groups(x, group)
  stem_groups$id <- x$trees$id
  stem_groups$.expansion <- expansion
  source <- x[[table]]
  if (!nrow(source)) return(data.frame())
  requested_groups <- setdiff(names(stem_groups), c("id", ".expansion"))
  collisions <- intersect(requested_groups, names(source))
  if (is.list(group) && !is.character(group) && length(collisions)) {
    stop(
      "Named group vectors may not reuse source-table column names. ",
      "Rename the custom group and try again.", call. = FALSE
    )
  }
  stem_row <- match(source$id, stem_groups$id)
  joined <- source
  for (name in setdiff(requested_groups, names(joined))) {
    joined[[name]] <- stem_groups[[name]][stem_row]
  }
  joined$.expansion <- stem_groups$.expansion[stem_row]
  mandatory <- switch(table,
    trees = character(),
    logs = c("product", "scale_rule", "measurement_quantity", "scale_unit", "scale_bark_basis"),
    scales = c("scale_rule", "unit", "bark_basis"),
    values = c("product", "currency")
  )
  if (table == "scales" && "name" %in% names(joined)) {
    mandatory <- c(mandatory, "name", "is_product")
  }
  group_names <- c(requested_groups, mandatory)
  quantities <- .mc_summary_quantities(joined, table, basis)
  for (name in quantities) joined[[name]] <- joined[[name]] * joined$.expansion
  split_key <- interaction(joined[group_names], drop = TRUE, lex.order = TRUE)
  missing <- .mc_missing_summary()
  rows <- lapply(split(seq_len(nrow(joined)), split_key), function(selected) {
    labels <- joined[selected[1L], group_names, drop = FALSE]
    totals <- lapply(quantities, function(name) {
      value <- joined[[name]][selected]
      missing$record(value, name, labels)
      if (all(is.na(value)) || (na_action == "propagate" && anyNA(value))) {
        NA_real_
      } else {
        sum(value, na.rm = TRUE)
      }
    })
    names(totals) <- quantities
    ids <- unique(joined$id[selected])
    stem_rows <- match(ids, x$trees$id)
    statuses <- if ("status" %in% names(x$trees)) x$trees$status[stem_rows] else
      rep(0L, length(stem_rows))
    counts <- data.frame(
      input_trees = length(ids), valid_trees = sum(statuses %in% c(0L, 410L)),
      failed_trees = sum(!statuses %in% c(0L, 410L)),
      valid_zero_log_trees = sum(statuses == 410L),
      stringsAsFactors = FALSE
    )
    cbind(labels, as.data.frame(totals), counts)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  if (".all" %in% names(result)) result$.all <- NULL
  missing$warn(na_action)
  result
}
