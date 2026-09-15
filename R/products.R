#' Define one product
#'
#' Describe the logs a mill accepts and how it measures and prices them.
#' Candidate cuts use an internal half foot grid between the length limits.
#'
#' @details
#' `length_round` rounds nominal length down for scaling. Source board foot products accept
#' `1` for whole feet or `2` for even feet. The source segmentation option follows that choice
#' and the rule scales exactly the rounded length. Other increments are rejected.
#'
#' Cubic, green weight, and cord products scale the nominal body and report the rounded length
#' for the mill's records only.
#' @param product Name the product so its logs can be recognized. Character, unitless, required.
#' @param spcd Limit this product to the listed species codes. Numeric vector of inventory
#'   species codes, unitless, default `NULL` accepts every species.
#' @param min_dbh Set the smallest tree that qualifies for this product. Numeric, inches,
#'   default `0`.
#' @param max_dbh Set the tree diameter at which this product stops qualifying. Numeric, inches,
#'   default `NA` has no upper limit.
#' @param min_age Set the youngest tree that qualifies. Numeric, years, default `NA` has no limit.
#' @param max_age Set the age at which the tree stops qualifying. Numeric, years,
#'   default `NA` has no limit.
#' @param requires_pruned Require the entire cut to lie within the tree's pruned section.
#'   Logical, unitless, default `FALSE`.
#' @param min_length Set the shortest nominal log accepted. Numeric, feet, required.
#' @param max_length Set the longest nominal log accepted. Numeric, feet, required.
#' @param length_round Set the mill's scaling increment. Numeric, feet, default `1`.
#' @param trim Allow extra wood above the nominal log for trimming. Numeric, feet, default `0`.
#' @param min_sed Set the smallest diameter accepted at the physical small end. Numeric, inches,
#'   required.
#' @param max_sed Set the largest diameter accepted at the physical small end. Numeric, inches,
#'   default `NA` has no limit.
#' @param min_led Set the smallest diameter accepted at the physical large end. Numeric, inches,
#'   default `0`.
#' @param max_led Set the largest diameter accepted at the physical large end. Numeric, inches,
#'   default `NA` has no limit.
#' @param inside_bark Measure the diameter limits inside bark. Logical, unitless, default `TRUE`.
#'   `FALSE` uses outside bark. Cubic and cord volume use the same bark basis.
#' @param max_sweep Set the greatest sweep or crook accepted in an overlapping section.
#'   Numeric, percent, default `NA` has no limit.
#' @param max_logs Limit the number of these logs in each segment. Numeric whole number,
#'   logs per segment, default `NA` has no limit. Counts restart above culls and restrictions.
#' @param volume_unit Choose the product's volume measurement. Character, required.
#'   Choices are `'scribner'`, `'international'`, and `'doyle'` in board feet,
#'   `'cubic'` in cubic feet, `'green_ton'` in green short tons, and `'cord'` in cords.
#'   Board foot rules measure inside bark. Green weight includes wood and attached bark.
#' @param split_scale Scale Scribner logs in shorter sections. Logical, unitless, default `FALSE`.
#'   This changes scaling sections only and has no effect on other volume measurements.
#' @param round Choose how to round the scaling diameter before applying the rule.
#'   Character, unitless, default `'default'` uses the rule's rounding.
#'   `'down'` rounds down to whole inches, `'nearest'` rounds to the nearest inch with halves
#'   upward, and `'none'` skips product rounding. Scribner and International still round
#'   to the nearest inch as their rules require. Doyle uses the resulting diameter directly.
#' @param cord_solid_fraction Set the solid wood proportion used to convert volume to cords.
#'   Numeric proportion strictly between zero and one, default `NA`.
#'   Required only for `'cord'`.
#' @param price Set the amount paid per pricing quantity. Numeric, caller's price unit,
#'   default `NA` leaves the product unpriced.
#' @param price_per Set how many volume units the price covers. Numeric, selected volume units,
#'   default `1`. The quantity uses the selected volume unit.
#' @return A one-row `merch_products` data frame. Columns and units match the arguments:
#'   * Identification: `product`, `spcd`.
#'   * Tree limits: `min_dbh`, `max_dbh`, `min_age`, `max_age`, `requires_pruned`.
#'   * Lengths: `min_length`, `max_length`, `length_round`, `trim`.
#'   * Diameters: `min_sed`, `max_sed`, `min_led`, `max_led`, `inside_bark`.
#'   * Cutting limits: `max_sweep`, `max_logs`.
#'   * Scaling: `volume_unit`, `split_scale`, `round`, `cord_solid_fraction`.
#'   * Pricing: `price`, `price_per`.
#'
#'   `spcd` is a list column of numeric code vectors.
#' @export
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
#' @examples
#' ## Define an unpriced product
#' specification <- product(product = 'domestic_saw',  ## name, unitless
#'                          min_length = 16,  ## feet
#'                          max_length = 40,  ## feet
#'                          trim = 1,  ## feet
#'                          min_sed = 6,  ## inches
#'                          volume_unit = 'scribner')  ## board feet
#'
#' ## Show the product name
#' specification$product
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
#' Cascade uses product order as cutting priority: the first product is offered the stem first.
#' Optimization ignores product order.
#' Each cull or restriction boundary starts a fresh segment and restarts this order.
#' @param ... Supply product rows in the order they should be cut. Data frames from [product()],
#'   with the units recorded there, supply at least one product.
#' @return A `merch_products` data frame with the columns and units listed in [product()].
#'   Rows retain the supplied order.
#' @export
#' @usage
#' products(
#'   ...
#' )
#' @examples
#' ## Define an unpriced product
#' saw <- product(product = 'domestic_saw',  ## name, unitless
#'                min_length = 16,  ## feet
#'                max_length = 40,  ## feet
#'                min_sed = 6,  ## inches
#'                volume_unit = 'scribner')  ## board feet
#'
#' ## Combine rows in cutting priority order
#' specifications <- products(saw = saw)
#'
#' ## Show the product order
#' specifications$product
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
