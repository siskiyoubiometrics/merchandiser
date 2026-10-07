#' Record a located stem defect
#'
#' Describe a cull, usable-stem end, product restriction, or sweep interval. Construction checks
#'   field types. Use [validate_defects()] to check heights and product relationships.
#'
#' @param tree_id Identifiers linking each defect to an input tree. Accepts a nonmissing atomic
#'   vector. Required, with no default. Its type must match the tree list when records are
#'   validated.
#' @param start_height Lower height of the affected interval, in feet above ground. Accepts
#'   numeric values. Required, with no default. Validation requires a nonnegative height within
#'   the tree.
#' @param end_height Upper height of the affected interval, in feet above ground. Accepts numeric
#'   values or `NA`. Required, with no default. Missing values extend intervals to the tip. An
#'   end effect accepts missing or equal starting and ending heights. Other intervals require a
#'   higher end.
#' @param effect Effect on cutting as a character vector. Accepts `'cull'` to remove wood,
#'   `'end'` to stop cutting, `'restrict'` to allow only a named product, or `'sweep'` to compare
#'   severity with product limits. Required, with no default.
#' @param product Required product label for a restriction, as a character vector. Defaults to
#'   `NULL`, represented by missing values. Must match a supplied specification for restrict
#'   effects and remain missing for other effects.
#' @param percent Sweep severity, in percent, as numeric values from 0 through 100. Defaults to
#'   `NULL`, represented by missing values. Required for sweep effects and must remain missing
#'   for other effects. It affects eligibility, not a scale deduction.
#' @return A `merch_defects` data frame with `tree_id` (identifier), `start_height` and
#'   `end_height` (feet above ground), `effect` (cutting effect), `product` (restriction label,
#'   otherwise missing), and `percent` (sweep percentage, otherwise missing). Scalars recycle to
#'   the record count.
#' @usage
#' defect(
#'   tree_id,
#'   start_height,
#'   end_height,
#'   effect,
#'   product = NULL,
#'   percent = NULL
#' )
#' @export
#' @examples
#' ## Record a cull on the first example tree
#' defect(tree_id = example_trees$tree_id[1],
#'        start_height = 1,
#'        end_height = 8,
#'        effect = 'cull')
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

#' Check defect heights and product restrictions
#'
#' Validate records against tree heights and product names. Exact duplicates are removed with a
#'   message, and overlapping valid culls are merged.
#'
#' @param defects Defect records with the columns returned by [defect()]. Required, without a
#'   default. Invalid records are retained with a status rather than silently used for cutting.
#' @param tree_id Unique tree identifiers corresponding to `ht`. Accepts a nonmissing atomic
#'   vector of the same identifier type as the defects. Required, without a default.
#' @param ht Total tree heights above ground, in feet. Accepts one numeric value for each tree
#'   identifier. Required, without a default. Used to resolve missing interval ends and check
#'   physical bounds.
#' @param products Validated product rows from [product()] or [products()]. Required, with no
#'   default. Names must be unique, and row order supplies priority for the default cascade
#'   strategy.
#' @return A `merch_defects` data frame with `tree_id` (identifier), `start_height` and
#'   `end_height` (feet above ground), `effect` (cutting effect), `product` (restriction label,
#'   otherwise missing), and `percent` (sweep percentage, otherwise missing). Scalars recycle to
#'   the record count. An integer `status` column reports each validation condition. See
#'   [status_codes()] for descriptions.
#' @usage
#' validate_defects(
#'   defects,
#'   tree_id,
#'   ht,
#'   products
#' )
#' @export
#' @examples
#' ## Define an unpriced cubic-foot product
#' saw <- product(product = 'saw',  ## product label
#'                min_length = 16,  ## feet
#'                max_length = 32,  ## feet
#'                min_sed = 6,  ## inches inside bark
#'                volume_unit = 'cubic')  ## cubic feet
#'
#' ## Define the product named by the shipped restrictions
#' pulp <- product(product = 'pulp',  ## product label
#'                 min_length = 8,  ## feet
#'                 max_length = 20,  ## feet
#'                 min_sed = 3,  ## inches inside bark
#'                 volume_unit = 'green_ton')  ## green short tons
#'
#' ## Validate shipped defects against their matching trees
#' checked <- validate_defects(defects = example_defects_pnw,
#'                             tree_id = example_trees_pnw$tree_id,
#'                             ht = example_trees_pnw$ht,
#'                             products = products(saw, pulp))
#'
#' ## Inspect validated records
#' head(checked)
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

#' Convert stopper heights to located defects
#'
#' Translate saw stops to restrictions, pulp stops to usable-stem ends, and jump butts to culls.
#'   Supplied stops must remain above the stump, within the tree, and in increasing physical
#'   order.
#'
#' @param tree_id Identifiers linking each defect to an input tree. Accepts a nonmissing atomic
#'   vector. Required, with no default. Its type must match the tree list when records are
#'   validated.
#' @param ht Total height above ground, in feet. Accepts numeric values greater than zero and no
#'   greater than 500. Required, with no default. Measurement heights and section bounds must
#'   fall within the tree.
#' @param topwood_product Product allowed above a saw stop or on a whole pulp tree, as a nonempty
#'   character scalar. Required, without a default. The label must match the product used in
#'   subsequent merchandising.
#' @param saw_stop Height above which only `topwood_product` is allowed, in feet. Accepts
#'   positive numeric values or missing values. Defaults to `NULL`, adding no saw restriction.
#'   Must follow any jump butt and precede any pulp stop.
#' @param pulp_stop Height ending the usable stem, in feet. Accepts positive numeric values or
#'   missing values. Defaults to `NULL`, adding no end record. Must lie above other supplied
#'   stops and no higher than total height.
#' @param jump_butt Upper height of an unusable butt section, in feet. Accepts positive numeric
#'   values or missing values. Defaults to `NULL`, adding no butt cull. A cull spans from
#'   `stump_ht` to this height.
#' @param pulp_tree Whether the entire tree is restricted to `topwood_product`. Accepts `TRUE` or
#'   `FALSE` per tree, without missing values. Defaults to `FALSE`. Cannot accompany a saw stop
#'   or jump butt for the same tree.
#' @param stump_ht Stump height above ground, in feet. Accepts finite, nonnegative numeric values
#'   below total height. Defaults to `1`. A jump-butt cull starts here, and supplied stopping
#'   heights must lie above it.
#' @return A `merch_defects` data frame with `tree_id` (identifier), `start_height` and
#'   `end_height` (feet above ground), `effect` (cutting effect), `product` (restriction label,
#'   otherwise missing), and `percent` (sweep percentage, otherwise missing). Scalars recycle to
#'   the record count.
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
#' @export
#' @examples
#' ## Convert the southern list's stopping heights
#' converted <- defects_from_stoppers(tree_id = example_trees_south$tree_id,
#'                                    ht = example_trees_south$ht,
#'                                    topwood_product = 'pulp',
#'                                    saw_stop = example_trees_south$saw_stop,
#'                                    pulp_stop = example_trees_south$pulp_stop,
#'                                    jump_butt = example_trees_south$jump_butt)
#'
#' ## Inspect the resulting defect intervals
#' head(converted)
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

#' Combine defect percentages by stem-volume thirds
#'
#'
#' Combine lower, middle, and upper defect percentages using inside bark volume weights. Use
#' the returned percentage in a separate deduction calculation, with diameters in inches and
#' heights in feet. The thirds divide total height from ground to tip, and merchandise()
#' does not apply the returned percentage. Scalar inputs recycle across trees, and invalid rows
#' retain their
#' input position and a status.
#'
#' @param dbh Outside bark diameter at breast height, in inches. Accepts numeric values greater
#'   than zero and no greater than 400. Required, with no default. Invalid measurement rows
#'   return missing results with a status.
#' @param ht Total height above ground, in feet. Accepts numeric values greater than zero and no
#'   greater than 500. Required, with no default. Measurement heights and section bounds must
#'   fall within the tree.
#' @param spcd Species identifier as a numeric vector of positive whole-number codes. Required,
#'   with no default. The species must be recognized for species properties and within the
#'   selected equation's scope.
#' @param lower Defect in the lower third of total tree height, in percent. Accepts numeric
#'   values from 0 through 100, or missing values. Required, with no default. The contribution is
#'   weighted by inside bark volume in that third.
#' @param middle Defect in the middle third of total tree height, in percent. Accepts numeric
#'   values from 0 through 100, or missing values. Required, with no default. The contribution is
#'   weighted by inside bark volume in that third.
#' @param upper Defect in the upper third of total tree height, in percent. Accepts numeric
#'   values from 0 through 100, or missing values. Required, with no default. The contribution is
#'   weighted by inside bark volume in that third.
#' @param model Taper equation identifier as a character vector. Defaults to `NULL`, selecting
#'   the stored species default. A supplied identifier overrides that choice. Scalar identifiers
#'   recycle across trees.
#' @param ... Additional named inputs accepted by the selected model, with none supplied by
#'   default. Numeric inputs must be finite: positive `upper_ht1`, `upper_ht2`, and `site_index`
#'   use feet, positive `upper_d1` and `upper_d2` use inches, and positive `basal_area` uses
#'   square feet per acre. `form_class` accepts positive numbers. `bark_ratio` is inside diameter
#'   divided by outside diameter, greater than zero and no greater than one. `decay_class`
#'   accepts whole numbers from 0 through 5 and `cull` accepts percentages from 0 through 100.
#'   `upper_bark` accepts `'ib'` or `'ob'`. Upper heights and diameters must be supplied in
#'   pairs. Only inputs declared by the selected model are accepted.
#' @return A data frame in input order with `value` (whole-tree defect, percent) and `status`
#'   (integer code described by [status_codes()]). Missing results remain in the table.
#'   Nonfatal section statuses are not propagated to this percentage result.
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
#' @export
#' @examples
#' ## Measure the first shipped tree with its stored equation
#' defect_by_thirds(dbh = example_trees$dbh[1],
#'                  ht = example_trees$ht[1],
#'                  spcd = example_trees$spcd[1],
#'                  lower = 10,
#'                  middle = 5,
#'                  upper = 0,
#'                  model = example_trees$model[1])
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
