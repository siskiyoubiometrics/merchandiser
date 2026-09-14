.mc_defect_columns <- c(
  "id", "defect_id", "from", "to", "effect", "percent", "category",
  "pathology", "source_record_id"
)

.mc_effects <- c("cull", "pulp", "exclude", "rot", "sweep", "crook", "fork", "break")
.mc_effect_rank <- stats::setNames(seq_along(c(
  "break", "cull", "fork", "pulp", "exclude", "sweep", "crook", "rot"
)), c("break", "cull", "fork", "pulp", "exclude", "sweep", "crook", "rot"))

.mc_defect_frame <- function(values) {
  result <- data.frame(
    id = values$id,
    defect_id = values$defect_id,
    from = values$from,
    to = values$to,
    effect = values$effect,
    percent = values$percent,
    category = values$category,
    pathology = values$pathology,
    source_record_id = values$source_record_id,
    stringsAsFactors = FALSE
  )
  class(result) <- c("merch_defects", "data.frame")
  result
}

#' Record a stem condition at a height or along a section
#'
#' Create located defect records from tree identifiers, heights, and treatments. Use
#' [validate_defects()] to check the records against tree heights.
#'
#' @param id Required atomic identifiers, one per record or one value to repeat.
#'   Missing values are errors. Unitless.
#' @param from,to Required numeric heights, one per record or one value to repeat.
#'   No defaults. Use feet or meters consistently with the later calculation.
#'   Bounds and the special missing `to` convention are listed in the corresponding section.
#'
#' @param effect Required character treatments, one per record or one repeated
#'   value. Accepted labels and missing behavior are below. Example: `'sweep'`.
#' @param defect_id Character labels, one per record or one repeated value. Default `NULL` creates
#' missing labels for later assignment. Unitless.
#'
#' Missing labels are accepted until validation.
#' @param percent Numeric percentages, one per record or one repeated value.
#'   Default `NA_real_` means none supplied.  See fields.
#' @param category Character categories, one per record or one repeated value. Default
#' `NA_character_`. Unitless.
#'
#' Example: `'moderate'`.
#' @param pathology,source_record_id Character notes, one per record or one
#'   repeated value. Defaults are `NA_character_`. Unitless.
#'
#' @details Only length-one inputs repeat. Other nonempty inputs must have
#'   the same length. Supplying empty `id`, `from`, `to`, and `effect` returns
#'   an empty defect table.
#'
#' All required arguments must still be supplied.
#' @return A `merch_defects` data frame with the fields and no status
#'   column. Rows retain the supplied order.
#' @section Defect table fields:
#' A defect table has one row per recorded condition. Heights are measured
#' above ground in feet for imperial calculations or meters for metric
#' calculations. Construction does not convert units.
#'
#' Each field has one
#' value per row. Required columns must be present even in an empty table. \describe{
#' \item{`id`}{Required atomic tree identifier, such as `'example-1'`.
#' Missing identifiers are errors. Identifiers repeat for several records on
#' the same tree and must match the inventory. Unitless.}
#' \item{`from`}{Required numeric lower height, such as `10`. It must be
#' finite and between zero and total height when checked against a tree.
#' Missing values prevent a reliable interval check.}
#' \item{`to`}{Required numeric upper height, such as `20`. For intervals,
#' it must exceed `from` and not exceed total height. For `'fork'` or
#' `'break'`, supply `NA_real_` or the same height as `from`. Validation
#' retains these as single-height records.}
#' \item{`effect`}{Required character treatment, with no default. Choose
#' `'cull'`, `'pulp'`, `'exclude'`, `'rot'`, `'sweep'`, `'crook'`,
#' `'fork'`, or `'break'`, for example `'sweep'`. Missing or unknown
#' values receive `unknown_defect_effect` when validated. Unitless.}
#' \item{`defect_id`}{Character record label, unitless. Omission or `NA`
#' requests an identifier during validation, such as `'auto:000001'`.
#' Supplied identifiers must be unique within each tree. Exact duplicate
#' records with missing identifiers are errors.}
#' \item{`percent`}{Numeric percentage from zero through 100, such as `15`.
#' Default `NA_real_`. Required for rot and must be missing for other effects.
#' Invalid percentages receive `defect_percent_out_of_range` during validation.}
#' \item{`category`}{Character sweep or crook category, such as `'moderate'`. Default
#' `NA_character_`. It must be nonempty for sweep or crook and
#' missing for other effects.
#'
#' The later merchandising call checks it against
#' its ordered curvature categories. Unitless.}
#' \item{`pathology`}{Character field note, such as `'conk'`. Default
#' `NA_character_`, meaning no note. It changes no treatment. Unitless.}
#' \item{`source_record_id`}{Character field-record reference, such as
#' `'sheet-1'`. Default `NA_character_`, meaning no reference. Unitless.}
#' }
#' There are no list columns.
#'
#' Factor labels are accepted for text fields. Numeric measurements may be integers or doubles.
#' @section How restrictions and deductions work:
#' An interval includes its lower height and stops just before its upper
#' height. A log ending at the lower height does not overlap the interval.
#' A log starting at that height does. Tests use physical log length,
#' including trim.
#'
#' A cull section excludes volume and restarts product selection above it. A pulp restriction
#' permits only the product marked `accepts_pulp_restriction = TRUE`. An exclude restriction
#' rejects overlapping logs without providing that restart.
#'
#' Rot reduces volume in the affected section, with overlapping percentages combined by taking
#' the greatest percentage. The log deduction is weighted by modeled inside-bark volume. Sweep
#' and crook restrict product eligibility without assigning a percentage deduction.
#'
#' A fork restricts the stem above it to pulp within the utilization limit. A break ends log
#' selection at its height.
#' @section Status and missing values:
#' Construction
#' checks field types and lengths but does not produce status codes.
#'
#' Use validation before interpreting a record as a usable restriction. Validation returns
#' integer `status` for each record. `ok` passes the checks that were possible.
#'
#' `defect_height_out_of_range` identifies heights outside the tree, and `defect_interval_invalid`
#' identifies invalid
#' interval or point
#' bounds. `defect_percent_out_of_range` identifies invalid or misplaced percentages.
#' `unknown_defect_effect` identifies an unknown
#' effect, and `unknown_curvature_category` identifies a missing, empty, or misplaced curvature
#' category.
#'
#' Correct nonzero records before using them. The later merchandising call also checks
#' categories against its ordered scale.
#'
#' Validation with a missing or nonfinite tree height cannot establish that record heights are
#' inside the tree. Supply a usable tree height and validate again. Some such rows can retain
#' `ok`, so this is a known limitation of the validator.
#'
#' Missing quantities are not zero volume. `no_feasible_log` in a later merchandising result
#' denotes a
#' valid result with no logs.
#' @seealso [validate_defects()] to check locations,
#' [defects_from_stoppers()] to translate stopping heights, [merchandise()] to apply the
#' restrictions.
#' @export
#' @usage
#'
#' ## Call signatures
#' defect(id, from, to, effect, defect_id = NULL, percent = NA_real_, category = NA_character_,
#'   pathology = NA_character_, source_record_id = NA_character_)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Record a cull interval on the first shipped tree
#' defect(id = first(x = example_trees$tree), from = 10, to = 20, effect = 'cull')
defect <- function(id, from, to, effect, defect_id = NULL,
                   percent = NA_real_, category = NA_character_,
                   pathology = NA_character_,
                   source_record_id = NA_character_) {
  if (!length(id) && !length(from) && !length(to) && !length(effect)) {
    return(.mc_empty_defects(id))
  }
  if (is.null(defect_id)) defect_id <- NA_character_
  values <- list(
    id = id, defect_id = defect_id, from = from, to = to, effect = effect,
    percent = percent, category = category, pathology = pathology,
    source_record_id = source_record_id
  )
  size <- .mc_common_size(values)
  values <- Map(.mc_recycle, values, MoreArgs = list(size = size, name = "defect field"))
  .mc_require_atomic_id(values$id)
  for (name in c("defect_id", "effect", "category", "pathology", "source_record_id")) {
    if (is.factor(values[[name]])) values[[name]] <- as.character(values[[name]])
  }
  for (name in c("from", "to", "percent")) {
    value <- values[[name]]
    if (is.logical(value) && all(is.na(value))) value <- rep(NA_real_, length(value))
    if (.mc_numeric(value)) value <- as.double(value)
    values[[name]] <- value
  }
  if (!is.character(values$defect_id) || !is.double(values$from) ||
        !is.double(values$to) || !is.character(values$effect) ||
        !is.double(values$percent) || !is.character(values$category) ||
        !is.character(values$pathology) ||
        !is.character(values$source_record_id)) {
    stop(
      "Defect columns have unsupported types. Use numeric heights and ",
      "percentages plus text labels and try again.", call. = FALSE
    )
  }
  .mc_defect_frame(values)
}

.mc_defect_structure <- function(x, id) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data frame. Build defect rows with defect() ",
      "and try again.", call. = FALSE
    )
  }
  allowed <- .mc_defect_columns
  if (inherits(x, "merch_defects")) allowed <- c(allowed, "status")
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) {
    stop("Unknown defect column: ", unknown[[1L]],
         ". Remove it or rename it to a documented defect field.", call. = FALSE)
  }
  required <- c("id", "from", "to", "effect")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing required defect column: ", missing[[1L]],
         ". Add that field or build the row with defect().", call. = FALSE)
  }
  n <- nrow(x)
  defaults <- list(
    defect_id = rep(NA_character_, n), percent = rep(NA_real_, n),
    category = rep(NA_character_, n), pathology = rep(NA_character_, n),
    source_record_id = rep(NA_character_, n)
  )
  for (name in setdiff(names(defaults), names(x))) x[[name]] <- defaults[[name]]
  x <- x[.mc_defect_columns]
  .mc_require_atomic_id(x$id, "Defect id")
  matched_id <- match(x$id, id)
  if (!anyNA(matched_id)) {
    x$id <- id[matched_id]
  } else if (!identical(typeof(x$id), typeof(id))) {
    stop(
      "Defect id values do not match the tree ids. ",
      "Use ids from the tree input and try again.", call. = FALSE
    )
  }
  .mc_require_atomic_id(x$id)
  for (name in c("defect_id", "effect", "category", "pathology", "source_record_id")) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  }
  for (name in c("from", "to", "percent")) {
    if (is.logical(x[[name]]) && all(is.na(x[[name]]))) {
      x[[name]] <- rep(NA_real_, nrow(x))
    } else if (.mc_numeric(x[[name]])) {
      x[[name]] <- as.double(x[[name]])
    }
  }
  expected <- c(
    defect_id = "character", from = "double", to = "double", effect = "character",
    percent = "double", category = "character", pathology = "character",
    source_record_id = "character"
  )
  for (name in names(expected)) {
    if (typeof(x[[name]]) != expected[[name]]) {
      stop(
        name, " has an unsupported type. Convert it to the documented ",
        "defect field type and try again.", call. = FALSE
      )
    }
  }
  x
}

.mc_duplicate_missing_defects <- function(x) {
  missing <- is.na(x$defect_id)
  if (!any(missing)) return(FALSE)
  key_names <- setdiff(.mc_defect_columns, "defect_id")
  values <- lapply(x[missing, key_names, drop = FALSE], function(value) {
    ifelse(is.na(value), "<NA>", enc2utf8(as.character(value)))
  })
  anyDuplicated(do.call(paste, c(values, sep = "\034"))) > 0L
}

.mc_order_missing_last <- function(x) {
  ifelse(is.na(x), "\U0010ffff", enc2utf8(as.character(x)))
}

.mc_canonicalize_defects <- function(x, id) {
  if (.mc_duplicate_missing_defects(x)) {
    stop(
      "Exact duplicate defect rows with missing defect_id are not allowed. ",
      "Remove the duplicate or give each row a unique defect_id.",
      call. = FALSE
    )
  }
  id_order <- match(x$id, unique(id))
  rank <- unname(.mc_effect_rank[x$effect])
  rank[is.na(rank)] <- 999L
  initial <- order(
    id_order, x$from, x$to, rank,
    is.na(x$percent), x$percent,
    is.na(x$category), .mc_order_missing_last(x$category),
    is.na(x$pathology), .mc_order_missing_last(x$pathology),
    is.na(x$source_record_id), .mc_order_missing_last(x$source_record_id),
    is.na(x$defect_id), .mc_order_missing_last(x$defect_id),
    na.last = TRUE, method = "radix"
  )
  x <- x[initial, , drop = FALSE]
  stem_rank <- stats::ave(seq_len(nrow(x)), x$id, FUN = seq_along)
  missing <- is.na(x$defect_id)
  x$defect_id[missing] <- sprintf("auto:%06d", stem_rank[missing])
  duplicate <- duplicated(x[c("id", "defect_id")])
  if (any(duplicate)) {
    stop(
      "defect_id must be unique within id. ",
      "Give each defect on a tree a different defect_id.", call. = FALSE
    )
  }
  rank <- unname(.mc_effect_rank[x$effect])
  rank[is.na(rank)] <- 999L
  final <- order(
    match(x$id, unique(id)), x$from, x$to, rank, x$defect_id,
    na.last = TRUE, method = "radix"
  )
  x <- x[final, , drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Check defect records against tree heights
#'
#' Validate defect bounds, fields, and identifiers against the supplied trees. Return
#' normalized records with a status for each row.
#'
#' @param x Required data frame with the defect fields. Zero rows are
#'   accepted. Missing objects and unknown columns are errors.
#' @param products Required product data frame described in
#'   [Product specification schema][product_schema]. It is checked in its recorded
#'   unit system, or imperial units if that attribute is absent. Example:
#'   `example_products('pnw')`.
#'
#' Missing objects are errors.
#' @param ht Required numeric total heights, exactly one per `id`. Units must
#'   match the defect heights. Example: `example_trees$ht`.
#'
#' Missing and
#'   nonfinite heights weaken the location checks as listed in the corresponding section.
#' @param id Required unique nonmissing atomic tree identifiers, exactly one
#'   per tree. Unitless. Example: `example_trees$tree`. Unknown defect tree
#'   identifiers and duplicate tree identifiers are errors.
#'
#' @details Records are sorted by input tree order, lower height, upper height,
#'   effect order, and record identifier. Effect order is break, cull, fork,
#'   pulp, exclude, sweep, crook, then rot. Missing identifiers are assigned
#'   after sorting record contents.
#'
#' Products are validated, but this function
#'   does not check tree eligibility for a pulp product or curvature order.
#' @return A `merch_defects` table with the fields plus integer,
#'   unitless `status`. `ok` means no problem was detected for that record.
#' @section Defect table fields:
#' A defect table has one row per recorded condition. Heights are measured
#' above ground in feet for imperial calculations or meters for metric
#' calculations. Construction does not convert units.
#'
#' Each field has one
#' value per row. Required columns must be present even in an empty table. \describe{
#' \item{`id`}{Required atomic tree identifier, such as `'example-1'`.
#' Missing identifiers are errors. Identifiers repeat for several records on
#' the same tree and must match the inventory. Unitless.}
#' \item{`from`}{Required numeric lower height, such as `10`. It must be
#' finite and between zero and total height when checked against a tree.
#' Missing values prevent a reliable interval check.}
#' \item{`to`}{Required numeric upper height, such as `20`. For intervals,
#' it must exceed `from` and not exceed total height. For `'fork'` or
#' `'break'`, supply `NA_real_` or the same height as `from`. Validation
#' retains these as single-height records.}
#' \item{`effect`}{Required character treatment, with no default. Choose
#' `'cull'`, `'pulp'`, `'exclude'`, `'rot'`, `'sweep'`, `'crook'`,
#' `'fork'`, or `'break'`, for example `'sweep'`. Missing or unknown
#' values receive `unknown_defect_effect` when validated. Unitless.}
#' \item{`defect_id`}{Character record label, unitless. Omission or `NA`
#' requests an identifier during validation, such as `'auto:000001'`.
#' Supplied identifiers must be unique within each tree. Exact duplicate
#' records with missing identifiers are errors.}
#' \item{`percent`}{Numeric percentage from zero through 100, such as `15`.
#' Default `NA_real_`. Required for rot and must be missing for other effects.
#' Invalid percentages receive `defect_percent_out_of_range` during validation.}
#' \item{`category`}{Character sweep or crook category, such as `'moderate'`. Default
#' `NA_character_`. It must be nonempty for sweep or crook and
#' missing for other effects.
#'
#' The later merchandising call checks it against
#' its ordered curvature categories. Unitless.}
#' \item{`pathology`}{Character field note, such as `'conk'`. Default
#' `NA_character_`, meaning no note. It changes no treatment. Unitless.}
#' \item{`source_record_id`}{Character field-record reference, such as
#' `'sheet-1'`. Default `NA_character_`, meaning no reference. Unitless.}
#' }
#' There are no list columns.
#'
#' Factor labels are accepted for text fields. Numeric measurements may be integers or doubles.
#' @section How restrictions and deductions work:
#' An interval includes its lower height and stops just before its upper
#' height. A log ending at the lower height does not overlap the interval.
#' A log starting at that height does. Tests use physical log length,
#' including trim.
#'
#' A cull section excludes volume and restarts product selection above it. A pulp restriction
#' permits only the product marked `accepts_pulp_restriction = TRUE`. An exclude restriction
#' rejects overlapping logs without providing that restart.
#'
#' Rot reduces volume in the affected section, with overlapping percentages combined by taking
#' the greatest percentage. The log deduction is weighted by modeled inside-bark volume. Sweep
#' and crook restrict product eligibility without assigning a percentage deduction.
#'
#' A fork restricts the stem above it to pulp within the utilization limit. A break ends log
#' selection at its height.
#' @section Status and missing values:
#' Construction
#' checks field types and lengths but does not produce status codes.
#'
#' Use validation before interpreting a record as a usable restriction. Validation returns
#' integer `status` for each record. `ok` passes the checks that were possible.
#'
#' `defect_height_out_of_range` identifies heights outside the tree, and `defect_interval_invalid`
#' identifies invalid
#' interval or point
#' bounds. `defect_percent_out_of_range` identifies invalid or misplaced percentages.
#' `unknown_defect_effect` identifies an unknown
#' effect, and `unknown_curvature_category` identifies a missing, empty, or misplaced curvature
#' category.
#'
#' Correct nonzero records before using them. The later merchandising call also checks
#' categories against its ordered scale.
#'
#' Validation with a missing or nonfinite tree height cannot establish that record heights are
#' inside the tree. Supply a usable tree height and validate again. Some such rows can retain
#' `ok`, so this is a known limitation of the validator.
#'
#' Missing quantities are not zero volume. `no_feasible_log` in a later merchandising result
#' denotes a
#' valid result with no logs.
#' @seealso [defect()] to enter a record, [merchandise()] to check
#' whether logs meet these restrictions.
#' @export
#' @usage
#'
#' ## Call signatures
#' validate_defects(x, products, ht, id)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Record a cull interval on the first shipped tree
#' record <- defect(id = first(x = example_trees$tree), from = 10, to = 20, effect = 'cull')
#'
#' ## Check bounds against its total height
#' validate_defects(x = record,
#'                  products = example_products(name = 'pnw'),
#'                  ht = first(x = example_trees$ht),
#'                  id = first(x = example_trees$tree))
validate_defects <- function(x, products, ht, id) {
  product_units <- attr(products, "units")
  if (is.null(product_units)) product_units <- "imperial"
  products <- .validate_products_units(products, product_units)
  .mc_require_atomic_id(id)
  if (anyDuplicated(id)) {
    stop("id must be unique. Give each tree a different id and try again.", call. = FALSE)
  }
  if (!.mc_numeric(ht) || length(ht) != length(id)) {
    stop(
      "ht must be numeric and aligned with id. ",
      "Supply one height per tree and try again.", call. = FALSE
    )
  }
  ht <- as.double(ht)
  x <- .mc_defect_structure(x, id)
  point_form <- x$effect %in% c("fork", "break") &
    (is.na(x$to) | x$to == x$from)
  point_form[is.na(point_form)] <- FALSE
  x$to[point_form] <- x$from[point_form]
  x <- .mc_canonicalize_defects(x, id)
  tree <- match(x$id, id)
  if (anyNA(tree)) {
    stop(
      "Every defect id must occur in id. ",
      "Use tree ids from this call and try again.", call. = FALSE
    )
  }
  tree_ht <- ht[tree]
  status <- integer(nrow(x))
  unknown <- is.na(x$effect) | !x$effect %in% .mc_effects
  status[unknown] <- 406L
  finite_from <- is.finite(x$from)
  outside <- status == 0L & is.finite(tree_ht) &
    (!finite_from | x$from < 0 | x$from > tree_ht |
       (!is.na(x$to) & (!is.finite(x$to) | x$to < 0 | x$to > tree_ht)))
  outside[is.na(outside)] <- FALSE
  status[outside] <- 401L
  interval <- x$effect %in% c("cull", "pulp", "exclude", "rot", "sweep", "crook")
  invalid_interval <- status == 0L & interval &
    (is.na(x$to) | x$from >= x$to)
  invalid_interval[is.na(invalid_interval)] <- FALSE
  point <- x$effect %in% c("fork", "break")
  invalid_point <- status == 0L & point & !is.na(x$to) & x$to != x$from
  invalid_point[is.na(invalid_point)] <- FALSE
  status[invalid_interval | invalid_point] <- 402L
  bad_percent <- status == 0L & (
    (x$effect == "rot" &
       (is.na(x$percent) | !is.finite(x$percent) | x$percent < 0 | x$percent > 100)) |
      (x$effect != "rot" & !is.na(x$percent))
  )
  status[bad_percent] <- 403L
  bad_category <- status == 0L & (
    (x$effect %in% c("sweep", "crook") &
       (is.na(x$category) | !nzchar(x$category))) |
      (!x$effect %in% c("sweep", "crook") & !is.na(x$category))
  )
  status[bad_category] <- 407L
  point_ok <- status == 0L & point
  x$to[point_ok] <- x$from[point_ok]
  x$status <- status
  class(x) <- c("merch_defects", "data.frame")
  attr(x, "products_hash") <- .mc_products_hash(products)
  x
}

.mc_empty_defects <- function(id) {
  result <- data.frame(
    id = .mc_empty_id(id), defect_id = character(), from = double(), to = double(),
    effect = character(), percent = double(), category = character(),
    pathology = character(), source_record_id = character(),
    stringsAsFactors = FALSE
  )
  class(result) <- c("merch_defects", "data.frame")
  result
}

#' Convert product stopping heights to located defect records
#'
#' Translate stopping heights into located cull and pulp restrictions.
#'
#' @param id Required nonmissing atomic tree identifiers, one per tree or one
#'   repeated value. Unitless.
#' @param ht Required finite positive numeric total heights, one per tree or
#'   one repeated value. Feet in imperial units or meters in metric units.
#'   Missing values are errors.
#' @param saw_stop,pulp_stop,jump_butt Numeric heights above ground, one per
#'   tree or one repeated value. Default `NULL` and explicit `NA_real_` both
#'   mean no recorded stop. Present heights must follow stump, jump butt,
#'   saw stop, then pulp stop in strictly increasing order and cannot exceed
#'   total height.
#'
#' Units match `ht`.
#' @param always_pulp Logical or numeric 0/1, one per tree or one repeated
#'   value. Default `NULL` means `FALSE`. Missing values are errors.
#'
#' `TRUE`
#'   restricts volume above stump to pulp and cannot accompany a jump-butt
#'   or saw-stop height. Unitless. Example: `always_pulp = TRUE`.
#' @param stump_ht Numeric height above ground, one per tree or one repeated
#'   value. Default `NULL` selects 1 foot or 0.3 meter. Must be finite,
#'   nonmissing, at least zero, and below `ht`.
#' @param units One character value, `'imperial'` (default) or `'metric'`.
#'   Missing or unknown values are errors. Example: `units = 'metric'`.
#'
#' @details A jump butt removes the interval from stump to its height. A saw
#'   stop restricts the interval from its height to the pulp stop, or total
#'   height when the pulp stop is absent, to pulp. A pulp stop removes the
#'   interval above it.
#'
#' Empty intervals are omitted. Only length-one inputs
#'   repeat, and other lengths must match.
#' @return A `merch_defects` table with the fields, ordered by tree and
#'   interval. No status column is added. `defect_id` records the stopping
#'   convention as `.stopper.jump_butt`, `.stopper.pulp`, or `.stopper.cull`.
#' @section Defect table fields:
#' A defect table has one row per recorded condition. Heights are measured
#' above ground in feet for imperial calculations or meters for metric
#' calculations. Construction does not convert units.
#'
#' Each field has one
#' value per row. Required columns must be present even in an empty table. \describe{
#' \item{`id`}{Required atomic tree identifier, such as `'example-1'`.
#' Missing identifiers are errors. Identifiers repeat for several records on
#' the same tree and must match the inventory. Unitless.}
#' \item{`from`}{Required numeric lower height, such as `10`. It must be
#' finite and between zero and total height when checked against a tree.
#' Missing values prevent a reliable interval check.}
#' \item{`to`}{Required numeric upper height, such as `20`. For intervals,
#' it must exceed `from` and not exceed total height. For `'fork'` or
#' `'break'`, supply `NA_real_` or the same height as `from`. Validation
#' retains these as single-height records.}
#' \item{`effect`}{Required character treatment, with no default. Choose
#' `'cull'`, `'pulp'`, `'exclude'`, `'rot'`, `'sweep'`, `'crook'`,
#' `'fork'`, or `'break'`, for example `'sweep'`. Missing or unknown
#' values receive `unknown_defect_effect` when validated. Unitless.}
#' \item{`defect_id`}{Character record label, unitless. Omission or `NA`
#' requests an identifier during validation, such as `'auto:000001'`.
#' Supplied identifiers must be unique within each tree. Exact duplicate
#' records with missing identifiers are errors.}
#' \item{`percent`}{Numeric percentage from zero through 100, such as `15`.
#' Default `NA_real_`. Required for rot and must be missing for other effects.
#' Invalid percentages receive `defect_percent_out_of_range` during validation.}
#' \item{`category`}{Character sweep or crook category, such as `'moderate'`. Default
#' `NA_character_`. It must be nonempty for sweep or crook and
#' missing for other effects.
#'
#' The later merchandising call checks it against
#' its ordered curvature categories. Unitless.}
#' \item{`pathology`}{Character field note, such as `'conk'`. Default
#' `NA_character_`, meaning no note. It changes no treatment. Unitless.}
#' \item{`source_record_id`}{Character field-record reference, such as
#' `'sheet-1'`. Default `NA_character_`, meaning no reference. Unitless.}
#' }
#' There are no list columns.
#'
#' Factor labels are accepted for text fields. Numeric measurements may be integers or doubles.
#' @section Status and missing values:
#' Invalid tree heights, unordered stops, missing always-pulp flags, or
#' incompatible lengths stop the call. Omitted stops produce no restriction. A table with no
#' records is valid.
#'
#' It does not indicate that volume is
#' zero. Later `no_feasible_log` denotes a valid result with no logs under the products.
#' @seealso [defect()] to record other conditions, [validate_defects()] to
#'   check the result, [merchandise()] to select logs.
#' @export
#' @usage
#'
#' ## Call signatures
#' defects_from_stoppers(id, ht, saw_stop = NULL, pulp_stop = NULL, jump_butt = NULL, always_pulp
#'   = NULL, stump_ht = NULL, units = 'imperial')
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Translate stopping heights in the shipped inventory
#' defects_from_stoppers(id = example_trees_south$tree,
#'                       ht = example_trees_south$ht_simulated,
#'                       saw_stop = example_trees_south$saw_stop,
#'                       pulp_stop = example_trees_south$pulp_stop,
#'                       jump_butt = example_trees_south$jump_butt) %>%
#'   select(id, from, to, effect) %>%
#'   slice_head(n = 3)
defects_from_stoppers <- function(id, ht, saw_stop = NULL, pulp_stop = NULL,
                                  jump_butt = NULL, always_pulp = NULL,
                                  stump_ht = NULL, units = "imperial") {
  units <- .mc_scalar_choice(units, "units", c("imperial", "metric"))
  inputs <- Filter(Negate(is.null), list(
    id = id, ht = ht, saw_stop = saw_stop, pulp_stop = pulp_stop,
    jump_butt = jump_butt, always_pulp = always_pulp, stump_ht = stump_ht
  ))
  size <- .mc_common_size(inputs)
  id <- .mc_recycle(id, size, "id")
  ht <- .mc_recycle(ht, size, "ht")
  .mc_require_atomic_id(id)
  if (!.mc_numeric(ht)) {
    stop(
      "ht must contain finite positive numbers. ",
      "Supply one positive height per tree and try again.", call. = FALSE
    )
  }
  ht <- as.double(ht)
  if (anyNA(ht) || any(!is.finite(ht)) || any(ht <= 0)) {
    stop(
      "ht must contain finite positive numbers. Replace missing, nonfinite, ",
      "or nonpositive heights and try again.", call. = FALSE
    )
  }
  default_stump <- if (units == "imperial") 1 else 0.3
  stump <- if (is.null(stump_ht)) rep(default_stump, size) else
    .mc_recycle(stump_ht, size, "stump_ht")
  if (.mc_numeric(stump)) stump <- as.double(stump)
  if (!is.double(stump) || anyNA(stump) || any(!is.finite(stump)) ||
        any(stump < 0 | stump >= ht)) {
    stop(
      "stump_ht must satisfy 0 <= stump_ht < ht. ",
      "Supply a valid stump height for every tree and try again.",
      call. = FALSE
    )
  }
  height_value <- function(x, name) {
    if (is.null(x)) return(rep(NA_real_, size))
    x <- .mc_recycle(x, size, name)
    if (.mc_numeric(x)) x <- as.double(x)
    if (!is.double(x) || any(!is.na(x) & !is.finite(x))) {
      stop(name, " must be numeric. Supply finite heights or NA and try again.", call. = FALSE)
    }
    x
  }
  jump <- height_value(jump_butt, "jump_butt")
  saw <- height_value(saw_stop, "saw_stop")
  pulp <- height_value(pulp_stop, "pulp_stop")
  always <- if (is.null(always_pulp)) rep(FALSE, size) else
    .mc_recycle(always_pulp, size, "always_pulp")
  always <- .mc_as_logical(always, "always_pulp", allow_na = FALSE)
  if (any(always & (!is.na(jump) | !is.na(saw)))) {
    stop(
      "always_pulp is mutually exclusive with jump_butt and saw_stop. ",
      "Clear the stopper heights or set always_pulp to FALSE.",
      call. = FALSE
    )
  }
  for (row in seq_len(size)) {
    present <- c(jump[row], saw[row], pulp[row])
    present <- present[!is.na(present)]
    out_of_range <- any(present <= stump[row]) || any(present > ht[row])
    bad_order <- is.unsorted(present, strictly = TRUE)
    if (length(present) && (out_of_range || bad_order)) {
      stop(
        "Present stopper heights must be strictly ordered after stump and ",
        "no greater than ht. Correct their order and try again.",
        call. = FALSE
      )
    }
  }
  rows <- vector("list", size * 3L)
  at <- 0L
  add <- function(stem, from, to, effect, defect_id) {
    if (from >= to) return()
    at <<- at + 1L
    rows[[at]] <<- defect(
      id[stem], as.double(from), as.double(to), effect, defect_id = defect_id
    )
  }
  for (stem in seq_len(size)) {
    if (!is.na(jump[stem])) add(stem, stump[stem], jump[stem], "cull", ".stopper.jump_butt")
    pulp_from <- if (always[stem]) stump[stem] else saw[stem]
    if (!is.na(pulp_from)) {
      pulp_to <- if (is.na(pulp[stem])) ht[stem] else pulp[stem]
      add(stem, pulp_from, pulp_to, "pulp", ".stopper.pulp")
    }
    if (!is.na(pulp[stem])) add(stem, pulp[stem], ht[stem], "cull", ".stopper.cull")
  }
  if (!at) return(.mc_empty_defects(id))
  result <- do.call(rbind, rows[seq_len(at)])
  result <- .mc_canonicalize_defects(result, id)
  rownames(result) <- NULL
  result
}

#' Convert stem-third defect percentages to one whole-tree percentage
#'
#' Return a whole-tree percentage weighted by inside-bark volume in each third of total height.
#'
#' @inheritParams dib
#' @param pct_lower,pct_middle,pct_upper Required numeric percentages, one
#'   per tree or length one to repeat, each from zero through 100. Missing
#'   percentages produce a missing answer with `na_input`. Omission is an error.
#'
#' @return A numeric whole-tree percentage or a status data frame.
#' @export
#' @usage
#'
#' ## Call signatures
#' defect_pct_from_thirds(dbh, ht, model, pct_lower, pct_middle, pct_upper, ..., units =
#'   'imperial', status = FALSE)
#' @examples
#' ## Weight stem-third percentages by modeled volume
#' defect_pct_from_thirds(dbh = example_trees$dbh,
#'                        ht = example_trees$ht,
#'                        model = example_trees$model,
#'                        pct_lower = 10,
#'                        pct_middle = 20,
#'                        pct_upper = 30)
#' @section Status and missing values:
#' Missing percentages return `NA` with `na_input`. Percentages outside zero
#' through 100 stop the call. The calculation uses each third's modeled
#' inside-bark volume and divides their weighted deductions by total modeled
#' volume.
#'
#' A zero or nonfinite denominator returns `NA`. The status may still indicate success without a
#' warning.
#'
#' `species_out_of_scope` and `not_unique` from usable volume calculations are treated as
#' completion. Other volume failures propagate the first failing third's code.
#' @seealso [apply_defect_pct()] to apply the percentage, [defect()] when
#'   locations should change cutting, [stem_volume()] to inspect each third.
#' @inheritSection dib Equation inputs through dots
#' @param status One nonmissing logical or numeric `0`/`1` value, default
#'   `FALSE` for a numeric percentage vector with row warnings. `TRUE` returns
#'   the two-column table listed in the corresponding section and suppresses those warnings.
#' Unitless.
#'
#' Example: `status = TRUE`.
#' @return A numeric percentage vector, length one per input tree, or a data
#'   frame with numeric `value` in percent and integer unitless `status`.
#'   Missing results remain missing. There are no list columns.
#' @section Other volume diagnoses:
#' The underlying volume calculation retains input, bounds, species, model, and equation-failure
#' diagnoses from [stem_volume()]. These results are missing. Correct the named measurement or
#' choose an available equation and rerun.
#'
#' Source equation failures return missing quantities and retain their source diagnosis.
defect_pct_from_thirds <- function(dbh, ht, model, pct_lower, pct_middle,
                                   pct_upper, ..., units = "imperial",
                                   status = FALSE) {
  units <- .mc_scalar_choice(units, "units", c("imperial", "metric"))
  status <- .mc_as_logical(status, "status", allow_na = FALSE)
  if (length(status) != 1L) {
    stop(
      "status must be one TRUE/FALSE or 0/1 value. ",
      "Supply one flag and try again.", call. = FALSE
    )
  }
  dots <- list(...)
  values <- c(list(
    dbh = dbh, ht = ht, model = model, pct_lower = pct_lower,
    pct_middle = pct_middle, pct_upper = pct_upper
  ), dots)
  size <- .mc_common_size(values)
  values <- Map(function(value, name) .mc_recycle(value, size, name), values, names(values))
  percentages <- cbind(values$pct_lower, values$pct_middle, values$pct_upper)
  if (.mc_numeric(percentages)) percentages <- matrix(
    as.double(percentages), nrow = size, ncol = 3L
  )
  invalid_percentages <- !is.na(percentages) &
    (!is.finite(percentages) | percentages < 0 | percentages > 100)
  if (!is.double(percentages) || any(invalid_percentages)) {
    stop(
      "Third percentages must be numeric values from zero through 100. ",
      "Correct the percentages and try again.", call. = FALSE
    )
  }
  lower <- c(0, 1 / 3, 2 / 3)
  upper <- c(1 / 3, 2 / 3, 1)
  volume <- matrix(NA_real_, nrow = size, ncol = 3L)
  volume_status <- matrix(0L, nrow = size, ncol = 3L)
  aux <- values[setdiff(names(values), c(
    "dbh", "ht", "model", "pct_lower", "pct_middle", "pct_upper"
  ))]
  for (third in seq_len(3L)) {
    answer <- do.call(stem_volume, c(list(
      dbh = values$dbh, ht = values$ht, model = values$model,
      lower = values$ht * lower[third], lower_type = "height",
      upper = values$ht * upper[third], upper_type = "height",
      bark = "inside", units = units, status = TRUE
    ), aux))
    volume[, third] <- answer$value
    volume_status[, third] <- answer$status
  }
  output_status <- apply(volume_status, 1L, function(x) {
    invalid <- x[!x %in% c(0L, 52L, 102L)]
    if (length(invalid)) invalid[[1L]] else 0L
  })
  missing_pct <- apply(percentages, 1L, anyNA)
  output_status[output_status == 0L & missing_pct] <- 1L
  denominator <- rowSums(volume)
  value <- rowSums(volume * percentages / 100) / denominator * 100
  value[output_status != 0L | !is.finite(value)] <- NA_real_
  if (status) return(data.frame(value = value, status = as.integer(output_status)))
  if (any(output_status != 0L)) .mc_status_warning("defect_pct_from_thirds", output_status)
  value
}
