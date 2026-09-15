#' Record a section that changes log selection
#'
#' Record culls, product restrictions, stem ends, and sweep as heights above ground.
#' The records change available stem sections and product eligibility.
#'
#' @details
#' Culls remove intervals, and restrictions allow only their named product.
#' Both split the stem and restart product log counts.
#' Cascade also restarts cutting priority.
#' An end stops cutting at its lower height.
#' Sweep rejects overlapping logs above the product's percentage limit.
#' @param tree_id Identify the tree each record belongs to. Atomic vector, unitless, required.
#'   This identifier ties every log, residual, and status row back to its tree and is the only
#'   link between a log and its tree.
#' @param start_height Set the lower height of the affected section. Numeric, feet above ground,
#'   required.
#' @param end_height Set the upper height of the affected section. Numeric, feet above ground,
#'   required.
#'   `NA` means the tree top. For an end, use `NA` or the same height as `start_height`.
#' @param effect Describe how the section changes cutting. Character, unitless, required.
#'   Use `'cull'`, `'restrict'`, `'end'`, or `'sweep'`.
#' @param product Name the only product allowed in a restriction. Character, unitless,
#'   default `NULL`. Required for a restriction and unused for other effects.
#' @param percent Record the sweep, crook, or spike knot severity. Numeric, percent from zero
#'   through 100, default `NULL`. Required for sweep and unused for other effects.
#' @return A `merch_defects` data frame with `tree_id` (input identifier), `start_height` and
#'   `end_height`
#'   (feet above ground), `effect` and `product` (text), and `percent` (percent).
#' @export
#' @usage
#' defect(
#'   tree_id,
#'   start_height,
#'   end_height,
#'   effect,
#'   product = NULL,
#'   percent = NULL
#' )
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Record a product restriction
#' defect(tree_id = example_trees$tree_id[1],
#'        start_height = 40,
#'        end_height = NA,
#'        effect = 'restrict',
#'        product = 'pulp') %>%
#'   rename(`start_height (feet)` = start_height,
#'          `end_height (feet)` = end_height,
#'          `percent (%)` = percent)
defect <- function(
  tree_id,
  start_height,
  end_height,
  effect,
  product = NULL,
  percent = NULL
) {
  inputs <- list(tree_id = tree_id, start_height = start_height, end_height = end_height,
                 effect = effect)
  if (!is.null(product))
    inputs$product <- product
  if (!is.null(percent))
    inputs$percent <- percent
  size <- .common_size(inputs)
  inputs <- Map(function(x, name) .mc_recycle(x, size, name), inputs, names(inputs))
  .mc_require_atomic_id(inputs$tree_id, "tree_id")
  inputs$start_height <- .mc_as_double(inputs$start_height, "start_height")
  if (is.logical(inputs$end_height) && all(is.na(inputs$end_height)))
    inputs$end_height <- as.double(inputs$end_height)
  inputs$end_height <- .mc_as_double(inputs$end_height, "end_height")
  if (is.null(inputs$product))
    inputs$product <- rep(NA_character_, size)
  if (is.null(inputs$percent))
    inputs$percent <- rep(NA_real_, size)
  inputs$percent <- .mc_as_double(inputs$percent, "percent")
  if (!is.character(inputs$effect) || !is.character(inputs$product)) {
    stop("effect and product must be text.", call. = FALSE)
  }
  effects <- c("cull", "restrict", "end", "sweep")
  unknown_effect <- inputs$effect[is.na(inputs$effect) | !inputs$effect %in% effects]
  if (length(unknown_effect)) {
    stop("effect must be cull, restrict, end, or sweep, not ", unknown_effect[[1L]], call. = FALSE)
  }
  inputs <- inputs[c("tree_id", "start_height", "end_height", "effect", "product", "percent")]
  structure(as.data.frame(inputs, stringsAsFactors = FALSE), class = c(
    "merch_defects", "data.frame"
  ))
}

.mc_empty_defects <- function(tree_id) {
  defect(tree_id[FALSE], numeric(), numeric(), character())
}

#' Check defects against tree heights and products
#'
#' Check each record before using it to select logs.
#' @param defects Supply recorded tree defects. Data frame from [defect()], required.
#' @param products Supply the products allowed in restrictions. Product data frame, required.
#' @param ht Supply each tree's total height. Numeric, feet, required.
#' @param tree_id Identify each tree in the height vector. Unique atomic vector, unitless, required.
#' @return The columns from [defect()] with integer `status` added. Heights remain in feet,
#'   `percent` remains percent, and `status` is a unitless code from [status_codes()].
#' @export
#' @usage
#' validate_defects(
#'   defects,
#'   tree_id,
#'   ht,
#'   products
#' )
#' @examples
#' ## Check the first mapped cull with an explicit product
#' checked <- validate_defects(defects = head(example_defects_pnw, n = 1),
#'                             products = product(product = 'pulp',  ## name, unitless
#'                                                min_length = 8,  ## feet
#'                                                max_length = 40,  ## feet
#'                                                min_sed = 3,  ## inches
#'                                                volume_unit = 'cubic'),  ## cubic feet
#'                             ht = example_trees_pnw$ht,
#'                             tree_id = example_trees_pnw$tree_id)
#'
#' ## Show the validation status
#' checked$status
validate_defects <- function(
  defects,
  tree_id,
  ht,
  products
) {
  x <- defects
  products <- .mc_validate_products(products)
  .mc_require_atomic_id(tree_id, "tree_id")
  if (anyDuplicated(tree_id))
    stop("tree_id must be unique.", call. = FALSE)
  if (!is.numeric(ht) || length(ht) != length(tree_id)) {
    stop("ht needs one numeric height per tree_id.", call. = FALSE)
  }
  fields <- c("tree_id", "start_height", "end_height", "effect", "product", "percent")
  if (!is.data.frame(x) || !all(fields %in% names(x)) || any(!names(x) %in% c(
    fields, "status"
  ))) {
    stop("Defects must have the columns returned by defect().", call. = FALSE)
  }
  x <- do.call(defect, as.list(x[fields]))
  id_mode <- function(value) if (is.numeric(value)) "numeric" else mode(value)
  if (!identical(id_mode(x$tree_id), id_mode(tree_id))) {
    stop("defects tree_id must have the same mode as tree_id in the tree list.", call. = FALSE)
  }
  tree <- match(x$tree_id, tree_id)
  endpoint <- ifelse(is.na(x$end_height), ht[tree], x$end_height)
  code <- integer(nrow(x))
  code[is.na(tree)] <- 405L
  code[code == 0 & x$effect != "restrict" & !is.na(x$product)] <- 409L
  invalid_height <- !is.finite(ht[tree]) | !is.finite(x$start_height) | x$start_height < 0 |
    x$start_height > ht[tree] |
    !is.finite(endpoint) | endpoint < 0 | endpoint > ht[tree]
  code[code == 0 & invalid_height] <- 401L
  invalid_interval <- ifelse(x$effect == "end", !is.na(x$end_height) &
                               x$end_height != x$start_height, endpoint <=
                               x$start_height)
  code[code == 0 & !is.na(invalid_interval) & invalid_interval] <- 402L
  bad_percent <- ifelse(x$effect == "sweep", !is.finite(x$percent) | x$percent < 0 |
                          x$percent >
                            100, !is.na(x$percent))
  code[code == 0 & !is.na(bad_percent) & bad_percent] <- 403L
  restrict <- x$effect == "restrict" & code == 0
  code[restrict & (is.na(x$product) | !x$product %in% products$product)] <- 407L
  x$status <- code
  duplicate <- duplicated(x)
  if (any(duplicate)) {
    message("Dropped exact duplicate defect rows.")
    x <- x[!duplicate, , drop = FALSE]
  }
  .mc_merge_culls(x, tree_id, ht)
}

.mc_merge_culls <- function(x, tree_id, ht) {
  drop <- logical(nrow(x))
  for (id in unique(x$tree_id[x$effect == "cull" & x$status == 0L])) {
    rows <- which(x$tree_id == id & x$effect == "cull" & x$status == 0L)
    rows <- rows[order(x$start_height[rows])]
    current <- rows[1L]
    top <- ht[match(id, tree_id)]
    for (row in rows[-1L]) {
      end <- if (is.na(x$end_height[current])) top else x$end_height[current]
      if (x$start_height[row] < end) {
        x$end_height[current] <- if (anyNA(x$end_height[c(current, row)])) {
          NA_real_
        } else {
          max(x$end_height[c(current, row)])
        }
        drop[row] <- TRUE
      } else {
        current <- row
      }
    }
  }
  x <- x[!drop, , drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Convert southern stopping heights to defect records
#'
#' Translate a saw stop, pulp stop, jump butt, or whole pulp tree into located records.
#' @inheritParams defect
#' @param ht Supply each tree's total height. Numeric, feet, required.
#' @param saw_stop Stop saw products at this height and allow only `topwood_product` above it.
#'   Numeric, feet, default `NULL` records no stop. Missing heights also record no stop.
#' @param pulp_stop End the usable stem at this height. Numeric, feet, default `NULL` records
#'   no stop. Missing heights also record no stop.
#' @param jump_butt Remove the wood between the stump and this height. Numeric, feet,
#'   default `NULL` records no cull. Missing heights also record no cull.
#' @param pulp_tree Restrict the whole tree to `topwood_product`. Logical, unitless,
#'   default `FALSE`.
#' @param stump_ht Set the lower height of a jump butt cull. Numeric, feet, default `1`.
#' @param topwood_product Name the product allowed above a saw stop or on a whole pulp tree.
#'   Character scalar, unitless, required.
#' @return A `merch_defects` data frame with the columns and units listed in [defect()].
#'   A saw stop becomes `restrict`, a pulp stop becomes `end`, and a jump butt becomes `cull`.
#' @details
#' Heights must be finite and at most 500 feet. Total height must be positive. Other
#'   heights must be nonnegative. Optional missing stopping heights record no stop.
#' @export
#' @usage
#' defects_from_stoppers(
#'   tree_id,
#'   ht,
#'   topwood_product,
#'   saw_stop = NULL,
#'   pulp_stop = NULL,
#'   jump_butt = NULL,
#'   pulp_tree = FALSE,
#'   stump_ht = 1
#' )
#' @examples
#' ## Convert the shipped stopping heights
#' records <- defects_from_stoppers(
#'   tree_id = example_trees_south$tree_id,
#'   ht = example_trees_south$ht,
#'   topwood_product = "pulpwood",
#'   saw_stop = example_trees_south$saw_stop,
#'   pulp_stop = example_trees_south$pulp_stop,
#'   jump_butt = example_trees_south$jump_butt
#' )
#'
#' ## Show the converted effects
#' head(records$effect, n = 3)
defects_from_stoppers <- function(
  tree_id,
  ht,
  topwood_product,
  saw_stop = NULL,
  pulp_stop = NULL,
  jump_butt = NULL,
  pulp_tree = FALSE,
  stump_ht = 1
) {
  inputs <- Filter(Negate(is.null), list(
    tree_id = tree_id, ht = ht, saw_stop = saw_stop, pulp_stop = pulp_stop,
    jump_butt = jump_butt, pulp_tree = pulp_tree, stump_ht = stump_ht
  ))
  size <- .common_size(inputs)
  inputs <- Map(function(x, name) .mc_recycle(x, size, name), inputs, names(inputs))
  .mc_require_atomic_id(inputs$tree_id, "tree_id")
  if (!is.numeric(inputs$ht) || any(!is.finite(inputs$ht) | inputs$ht <= 0 |
                                      inputs$ht > 500)) {
    stop("ht must contain finite positive heights at most 500 feet.", call. = FALSE)
  }
  for (name in c("saw_stop", "pulp_stop", "jump_butt", "stump_ht")) {
    if (is.null(inputs[[name]]))
      inputs[[name]] <- rep(NA_real_, size)
    if (!is.numeric(inputs[[name]]))
      stop(name, " must be numeric.", call. = FALSE)
    height <- inputs[[name]]
    invalid <- is.nan(height) | (!is.na(height) &
                                   (!is.finite(height) | height < 0 | height > 500))
    if (any(invalid))
      stop(name, " must contain heights from zero through 500 feet or NA.", call. = FALSE)
  }
  if (!is.logical(inputs$pulp_tree) || anyNA(inputs$pulp_tree)) {
    stop("pulp_tree must contain TRUE or FALSE.", call. = FALSE)
  }
  .scalar_character(topwood_product, "topwood_product")
  rows <- list()
  for (i in seq_len(size)) {
    stump <- inputs$stump_ht[i]
    if (!is.finite(stump) || stump < 0 || stump >= inputs$ht[i]) {
      stop("stump_ht must be at least zero and below ht.", call. = FALSE)
    }
    stops <- c(inputs$jump_butt[i], inputs$saw_stop[i], inputs$pulp_stop[i])
    present <- stops[!is.na(stops)]
    if (any(!is.finite(present) | present <= stump | present > inputs$ht[i]) ||
          is.unsorted(present,
            strictly = TRUE
          )) {
      stop("Stopping heights must increase above the stump and cannot exceed ht.",
        call. = FALSE
      )
    }
    if (inputs$pulp_tree[i] && any(!is.na(stops[1:2]))) {
      stop("A whole pulp tree cannot also have a saw stop or jump butt.", call. = FALSE)
    }
    id <- inputs$tree_id[i]
    if (!is.na(stops[1]))
      rows[[length(rows) + 1]] <- defect(id, stump, stops[1], "cull")
    if (inputs$pulp_tree[i] || !is.na(stops[2])) {
      rows[[length(rows) + 1]] <- defect(id, if (inputs$pulp_tree[i])
                                           0 else stops[2], NA_real_, "restrict", topwood_product)
    }
    if (!is.na(stops[3]))
      rows[[length(rows) + 1]] <- defect(id, stops[3], NA_real_, "end")
  }
  if (!length(rows))
    return(.mc_empty_defects(tree_id))
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Weight stem-third defect percentages by volume
#'
#' Convert percentages recorded by stem third into one whole-tree percentage.
#' The package does not apply this percentage inside [merchandise()]. Application to volume is a
#'   separate script calculation.
#' @inheritParams stem_volume
#' @param lower Record the defect in the lower third. Numeric, percent, required.
#' @param middle Record the defect in the middle third. Numeric, percent, required.
#' @param upper Record the defect in the upper third. Numeric, percent, required.
#' @return A data frame with `value` (whole-tree percent) and `status` (unitless integer code).
#' @export
#' @usage
#' defect_by_thirds(
#'   dbh,
#'   ht,
#'   spcd,
#'   lower,
#'   middle,
#'   upper,
#'   model = NULL,
#'   ...
#' )
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Calculate a separate whole-tree percentage for an example tree.
#' defect_by_thirds(dbh = example_trees$dbh[1],
#'                        ht = example_trees$ht[1],
#'                        spcd = example_trees$spcd[1],
#'                        model = example_trees$model[1],
#'                        lower = 10,
#'                        middle = 0,
#'                        upper = 0) %>%
#'   rename(`defect (percent)` = value)
defect_by_thirds <- function(
  dbh,
  ht,
  spcd,
  lower,
  middle,
  upper,
  model = NULL,
  ...
) {
  values <- list(
    dbh = dbh, ht = ht, spcd = spcd, lower = lower, middle = middle,
    upper = upper
  )
  if (!is.null(model))
    values$model <- model
  size <- .common_size(values)
  values <- Map(function(x, name) .mc_recycle(x, size, name), values, names(values))
  for (name in c("dbh", "ht", "lower", "middle", "upper")) {
    if (!is.numeric(values[[name]]))
      stop(name, " must be numeric.", call. = FALSE)
  }
  percentages <- cbind(values$lower, values$middle, values$upper)
  if (!is.numeric(percentages) || any(!is.na(percentages) & (!is.finite(
    percentages
  ) | percentages <
    0 | percentages > 100))) {
    stop("Third percentages must be between zero and 100.", call. = FALSE)
  }
  volume <- matrix(NA_real_, size, 3)
  code <- integer(size)
  for (third in 1:3) {
    answer <- stem_volume(
      values$dbh,
      values$ht,
      values$spcd,
      values$model,
      from = values$ht *
        (third - 1) / 3,
      to = values$ht * third / 3,
      ...
    )
    volume[, third] <- answer$value
    bad <- code == 0 & !answer$status %in% c(0, 52, 102)
    code[bad] <- answer$status[bad]
  }
  value <- rowSums(volume * percentages / 100) / rowSums(volume) * 100
  code[code == 0 & !is.finite(value)] <- 1L
  value[code != 0] <- NA_real_
  data.frame(value = value, status = code)
}
