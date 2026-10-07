#' Define log dimensions, eligibility, and pricing for a product
#'
#' Define the tree and log limits, scaling rule, and optional price for a product. Returns a
#'   validated product row for `merchandise()`. Combine rows with `products()` to set cutting
#'   priority under the default strategy.
#'
#' @param product Product label as a single nonempty character string. Required, with no default.
#'   Names must be unique when rows are combined and must match names used in defect
#'   restrictions.
#' @param spcd Species allowed for this product as a numeric vector of positive whole-number
#'   codes. Defaults to `NULL`, allowing all species. Tree calculations still require a usable
#'   taper equation and recognized species properties.
#' @param min_dbh Minimum outside bark diameter at breast height, in inches, for a tree to
#'   qualify. Accepts a single finite, nonnegative number. The lower limit is inclusive. Defaults
#'   to `0`, which imposes no additional minimum on a valid tree. Cannot exceed a supplied
#'   `max_dbh`.
#' @param max_dbh Maximum outside bark diameter at breast height, in inches, for tree
#'   eligibility. Accepts a single finite positive number or `NA`. The upper limit is exclusive.
#'   Defaults to `NA_real_`, imposing no maximum. Cannot be smaller than `min_dbh`.
#' @param min_age Minimum tree age, in years. Accepts a single finite nonnegative number or `NA`.
#'   The lower limit is inclusive. Defaults to `NA_real_`, imposing no minimum age. Requires
#'   `age` in [merchandise()] and cannot exceed `max_age`.
#' @param max_age Maximum tree age, in years. Accepts a single finite nonnegative number or `NA`.
#'   The upper limit is exclusive. Defaults to `NA_real_`, imposing no maximum age. Requires
#'   `age` in [merchandise()] and cannot be below `min_age`.
#' @param requires_pruned Whether logs must fit within the pruned portion of the tree. Accepts
#'   `TRUE` or `FALSE`, without missing values. Defaults to `FALSE`. When `TRUE`, [merchandise()]
#'   requires `pruned_ht` and checks the physical log end against that height.
#' @param min_length Shortest nominal log length, in feet. Accepts one finite positive number.
#'   Required, with no default. Cannot exceed `max_length`, and the interval must include a
#'   candidate on the half-foot grid.
#' @param max_length Longest nominal log length, in feet. Accepts one finite positive number.
#'   Required, with no default. Cannot be below `min_length`. Trim occupies additional stem
#'   length beyond this nominal length.
#' @param length_round Increment for rounding scaling length down, in feet. Accepts one finite
#'   positive number. Defaults to `1`. Board foot rules accept only `1` or `2`. Cubic, cord, and
#'   green weight scales use nominal length even when reported scaling length differs.
#' @param trim Extra length occupied above the nominal log body, in feet. Accepts one finite
#'   nonnegative number. Defaults to `0`. Physical end diameters include trim, but scale excludes
#'   it and the wood remains in the residual record.
#' @param min_sed Minimum diameter at the physical small end of a candidate log, in inches.
#'   Accepts one finite nonnegative number. Required, with no default. The lower limit is
#'   inclusive, uses `inside_bark`, and cannot exceed `max_sed`.
#' @param max_sed Maximum diameter at the physical small end of a candidate log, in inches.
#'   Accepts one finite nonnegative number or `NA`. The upper limit is inclusive. Defaults to
#'   `NA_real_`, imposing no maximum. Uses `inside_bark` and cannot be below `min_sed`.
#' @param min_led Minimum diameter at the physical large end of a candidate log, in inches.
#'   Accepts a single finite, nonnegative number. The lower limit is inclusive. Defaults to `0`,
#'   which imposes no additional large end minimum. Uses the bark basis selected by `inside_bark`
#'   and cannot exceed a supplied `max_led`.
#' @param max_led Maximum diameter at the physical large end of a candidate log, in inches.
#'   Accepts one finite nonnegative number or `NA`. The upper limit is inclusive. Defaults to
#'   `NA_real_`, imposing no maximum. Uses `inside_bark` and cannot be below `min_led`.
#' @param inside_bark Bark basis for the log end diameter limits. Accepts `TRUE` or `FALSE`, with
#'   no missing value. Defaults to `TRUE` for inside bark. `FALSE` uses outside bark. Cubic and
#'   cord scales use the same basis. Board foot rules still use inside bark diameters, and green
#'   weight includes wood and attached bark. This setting does not change the tree diameter basis
#'   for `min_dbh`.
#' @param max_sweep Largest sweep percentage accepted by a log overlapping a sweep interval.
#'   Accepts one finite number from 0 through 100 or `NA`. Defaults to `NA_real_`, imposing no
#'   sweep limit. Comparison uses the percentage in [defect()], without deducting it from scale.
#' @param max_logs Maximum number of logs from this product within each available stem segment.
#'   Accepts one positive whole number or `NA`. Defaults to `NA_integer_`, imposing no count
#'   limit. Counts restart at cull and restriction boundaries.
#' @param volume_unit Scaling rule as one character string. Accepts `'scribner'`,
#'   `'international'`, and `'doyle'` for board feet, `'cubic'` for cubic feet, `'cord'` for
#'   cords, or `'green_ton'` for green short tons. Required, with no default. The selected rule
#'   determines the scale and price basis.
#' @param split_scale Whether Scribner logs use shorter scaling sections.
#'   Accepts `TRUE` or `FALSE`, without missing values. Defaults to `FALSE`. It affects
#'   Scribner scale only and does not add physical cuts.
#' @param round Scaling diameter rounding as one character string. Accepts `'default'`, `'down'`,
#'   `'nearest'`, or `'none'`. Defaults to `'default'`, using the selected board foot rule's
#'   convention. Does not round tree measurements or physical eligibility limits. `none`
#'   bypasses preprocessing, but the source rule can still round internally.
#' @param cord_solid_fraction Solid volume divided by stacked cord volume. Accepts one finite
#'   number strictly between zero and one, or `NA`. Defaults to `NA_real_`. A finite fraction is
#'   required when `volume_unit = 'cord'` and converts the selected solid volume to cords.
#' @param price Amount per `price_per` scale units, in a caller-selected currency. Accepts one
#'   finite nonnegative number or `NA`. Defaults to `NA_real_`, leaving the product unpriced.
#'   Optimization requires a positive price on every product.
#' @param price_per Number of scale units covered by `price`. Accepts one finite positive number.
#'   Defaults to `1`. Log value is `scale * price / price_per`, using the unit selected by
#'   `volume_unit`.
#' @return A one-row `merch_products` data frame with all specification fields:
#' * `product`: label. `spcd`: list column of allowed numeric codes.
#' * `min_dbh`, `max_dbh`: outside bark diameter limits, inches.
#' * `min_age`, `max_age`: age limits, years. `requires_pruned`: pruning requirement.
#' * `min_length`, `max_length`, `length_round`, `trim`: lengths, feet.
#' * `min_sed`, `max_sed`, `min_led`, `max_led`: end diameter limits, inches.
#' * `inside_bark`: diameter basis. `max_sweep`: percent. `max_logs`: count per segment.
#' * `volume_unit`: scale rule. `split_scale`: split-scaling choice. `round`: rounding choice.
#' * `cord_solid_fraction`: solid-to-stacked volume fraction.
#' * `price`: currency amount. `price_per`: scale units per price.
#' @usage
#' product(
#'   product,
#'   spcd = NULL,
#'   min_dbh = 0,
#'   max_dbh = NA_real_,
#'   min_age = NA_real_,
#'   max_age = NA_real_,
#'   requires_pruned = FALSE,
#'   min_length,
#'   max_length,
#'   length_round = 1,
#'   trim = 0,
#'   min_sed,
#'   max_sed = NA_real_,
#'   min_led = 0,
#'   max_led = NA_real_,
#'   inside_bark = TRUE,
#'   max_sweep = NA_real_,
#'   max_logs = NA_integer_,
#'   volume_unit,
#'   split_scale = FALSE,
#'   round = 'default',
#'   cord_solid_fraction = NA_real_,
#'   price = NA_real_,
#'   price_per = 1
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
#' ## Select logs from the shipped trees
#' result <- merchandise(tree_id = example_trees$tree_id,
#'                       dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       spcd = example_trees$spcd,
#'                       products = saw,
#'                       model = example_trees$model)
#'
#' ## Inspect selected logs
#' head(result$logs)
product <- function(
  product, spcd = NULL, min_dbh = 0, max_dbh = NA_real_, min_age = NA_real_,
  max_age = NA_real_, requires_pruned = FALSE, min_length, max_length,
  length_round = 1, trim = 0,
  min_sed, max_sed = NA_real_, min_led = 0, max_led = NA_real_, inside_bark = TRUE,
  max_sweep = NA_real_,
  max_logs = NA_integer_, volume_unit, split_scale = FALSE, round = "default",
  cord_solid_fraction = NA_real_,
  price = NA_real_, price_per = 1
) {
  values <- as.list(environment())
  values$spcd <- I(list(spcd))
  sizes <- lengths(values)
  if (any(sizes != 1))
    stop(names(sizes)[which(sizes != 1)[1]], " needs one value.", call. = FALSE)
  .mc_validate_products(as.data.frame(values, stringsAsFactors = FALSE))
}

#' Combine products in cutting priority order
#'
#' Combine validated product rows into a specification. Under the default cascade strategy,
#'   earlier rows receive the first opportunity to use each available stem section.
#'
#' @param ... One or more product rows or tables from [product()]. Required, without a default.
#'   Rows are combined in argument order, with duplicate product names rejected.
#' @return A multiple-row `merch_products` data frame with all specification fields:
#' * `product`: label. `spcd`: list column of allowed numeric codes.
#' * `min_dbh`, `max_dbh`: outside bark diameter limits, inches.
#' * `min_age`, `max_age`: age limits, years. `requires_pruned`: pruning requirement.
#' * `min_length`, `max_length`, `length_round`, `trim`: lengths, feet.
#' * `min_sed`, `max_sed`, `min_led`, `max_led`: end diameter limits, inches.
#' * `inside_bark`: diameter basis. `max_sweep`: percent. `max_logs`: count per segment.
#' * `volume_unit`: scale rule. `split_scale`: split-scaling choice. `round`: rounding choice.
#' * `cord_solid_fraction`: solid-to-stacked volume fraction.
#' * `price`: currency amount. `price_per`: scale units per price.
#' @usage
#' products(
#'   ...
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
#' ## Combine the specification before cutting
#' specifications <- products(saw)
#'
#' ## Select logs from the shipped trees
#' result <- merchandise(tree_id = example_trees$tree_id,
#'                       dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       spcd = example_trees$spcd,
#'                       products = specifications,
#'                       model = example_trees$model)
#'
#' ## Inspect selected products
#' head(result$logs)
products <- function(...) {
  rows <- list(...)
  if (!length(rows)) {
    stop("Supply at least one product.", call. = FALSE)
  }
  rows <- lapply(rows, .mc_validate_products)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  .mc_validate_products(result)
}


.mc_validate_products <- function(x) {
  fields <- names(formals(product))
  if (!is.data.frame(x))
    stop("Products must be a data frame.", call. = FALSE)
  if (!nrow(x))
    stop("products must contain at least one product.", call. = FALSE)
  unknown <- setdiff(names(x), fields)
  if (length(unknown))
    stop("Unknown product field: ", unknown[1], call. = FALSE)
  required <- c("product", "min_length", "max_length", "min_sed", "volume_unit")
  missing <- setdiff(required, names(x))
  if (length(missing))
    stop("Missing product field: ", missing[1], call. = FALSE)
  defaults <- formals(product)
  for (name in setdiff(fields, names(x))) {
    x[[name]] <- if (name == "spcd")
      I(rep(list(NULL), nrow(x))) else rep(eval(defaults[[name]]), nrow(x))
  }
  if (!is.list(x$spcd))
    x$spcd <- I(as.list(x$spcd))
  for (codes in x$spcd) if (!is.null(codes))
    .normalize_species_codes(codes)
  for (name in c("product", "volume_unit", "round")) {
    if (is.factor(x[[name]]))
      x[[name]] <- as.character(x[[name]])
    if (!is.character(x[[name]]) || anyNA(x[[name]]) || any(!nzchar(x[[name]]))) {
      stop(name, " must contain nonmissing text.", call. = FALSE)
    }
  }
  if (anyDuplicated(x$product))
    stop("Product names must be unique.", call. = FALSE)
  for (name in c("requires_pruned", "inside_bark", "split_scale")) {
    if (!is.logical(x[[name]]) || anyNA(x[[name]])) {
      stop(name, " must contain TRUE or FALSE.", call. = FALSE)
    }
  }
  numeric_fields <- setdiff(fields, c(
    "product", "spcd", "volume_unit", "round", "requires_pruned",
    "inside_bark", "split_scale"
  ))
  optional <- c(
    "max_dbh", "min_age", "max_age", "max_sed", "max_led", "max_sweep", "max_logs",
    "cord_solid_fraction", "price"
  )
  for (name in numeric_fields) {
    v <- x[[name]]
    if (!is.numeric(v) || any(!is.na(v) & (!is.finite(v) | v < 0)) || (
                                                                       !name %in% optional &&
                                                                         anyNA(v))) {
      stop(name, " must contain finite nonnegative numbers.", call. = FALSE)
    }
    x[[name]] <- as.double(v)
  }
  for (name in c("min_length", "max_length", "length_round", "price_per", "max_dbh", "max_logs")) {
    if (any(x[[name]] <= 0, na.rm = TRUE))
      stop(name, " must be positive.", call. = FALSE)
  }
  for (pair in list(
    c("min_dbh", "max_dbh"), c("min_age", "max_age"), c("min_length", "max_length"),
    c("min_sed", "max_sed"), c("min_led", "max_led")
  )) {
    if (any(x[[pair[1]]] > x[[pair[2]]], na.rm = TRUE)) {
      stop(pair[2], " cannot be below ", pair[1], ".", call. = FALSE)
    }
  }
  if (any(ceiling(x$min_length * 2) > floor(x$max_length * 2))) {
    stop("Length bounds must include a cut on the half foot grid.", call. = FALSE)
  }
  if (any(x$max_logs != floor(x$max_logs), na.rm = TRUE)) {
    stop("max_logs must contain whole numbers.", call. = FALSE)
  }
  if (any(x$max_sweep > 100, na.rm = TRUE))
    stop("max_sweep cannot exceed 100.", call. = FALSE)
  if (any(!x$volume_unit %in% c(
    "scribner", "international", "doyle", "cubic", "green_ton",
    "cord"
  )))
    stop("Unknown volume_unit. Choose scribner, international, doyle, cubic, green_ton, or cord.",
      call. = FALSE
    )
  board <- x$volume_unit %in% c("scribner", "international", "doyle")
  if (any(board & !x$length_round %in% c(1, 2))) {
    stop("Source board foot products require length_round of 1 or 2 feet.", call. = FALSE)
  }
  if (any(!x$round %in% c("default", "down", "nearest", "none"))) {
    stop("Unknown diameter rounding choice for round. Choose default, down, nearest, or none.",
      call. = FALSE
    )
  }
  cord <- x$volume_unit == "cord"
  if (any(cord & (is.na(x$cord_solid_fraction) | x$cord_solid_fraction <= 0 |
                    x$cord_solid_fraction >=
                      1))) {
    stop("Cord volume requires cord_solid_fraction strictly between zero and one.",
      call. = FALSE
    )
  }
  x <- x[fields]
  class(x) <- c("merch_products", "data.frame")
  x
}
