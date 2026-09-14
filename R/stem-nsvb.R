.nsvb_source <- paste(
  "US Forest Service National Volume Estimator Library nsvb.f and tables1.inc",
  "through tables11.inc, commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.nsvb_model <- function(id, species) {
  new_stem_model_unchecked(
    id = id,
    family = "nsvb",
    kernel = list(
      type = "compiled",
      key = paste0("nsvb:", id),
      has_dob = TRUE,
      has_inverse = TRUE,
      has_integral = TRUE
    ),
    inputs = list(required = character(), optional = character(), pairs = list()),
    units = "imperial",
    species = as.integer(species),
    stump_ht = 1,
    bark_ratio = NA_real_,
    source = .nsvb_source,
    oracle_verified = .nvel_oracle_verified(id),
    notes = paste(
      "NSVB equation-6 Kozak volume-ratio profile.",
      "Inside diameter follows NVB_DibAtHT and volume follows Table S5."
    )
  )
}

.nsvb_models <- function() {
  path <- system.file(
    "extdata", "nsvb_models.csv", package = "merchandiser", mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE)
  Map(.nsvb_model, metadata$id, metadata$species)
}

.nsvb_patterns <- function() {
  data.frame(
    id = c("NVB0??????", "NVBM??????"),
    family = "nsvb",
    stringsAsFactors = FALSE
  )
}

.nsvb_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
        !grepl("^NVB[0M][0-9]{6}P?$", id)) {
    return(NULL)
  }
  .nsvb_model(id, as.integer(substr(id, 8L, 10L)))
}

.biomass_names <- c(
  paste0("dry_", c(
    "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood", "stump_bark",
    "saw_wood", "saw_bark", "topwood_wood", "topwood_bark", "tip_wood",
    "tip_bark", "branches", "foliage", "top_and_limb"
  )),
  "carbon", "co2e",
  paste0("green_", c(
    "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood", "stump_bark",
    "saw_wood", "saw_bark", "topwood_wood", "topwood_bark", "tip_wood",
    "tip_bark", "branches", "foliage", "top_and_limb"
  ))
)

.nsvb_dot_defaults <- list(
  region = 0,
  forest = 0,
  decay_class = 0,
  cull = 0,
  primary_top = 6,
  secondary_top = 4,
  stump_ht = 1,
  max_log_length = NA_real_,
  min_log_length = NA_real_,
  trim = NA_real_
)

.validate_integer_values <- function(value, name, minimum, maximum) {
  missing <- !is.finite(value)
  invalid <- !missing & (value != floor(value) | value < minimum | value > maximum)
  if (any(invalid)) {
    stop(name, " has ", sum(invalid), " out-of-domain ",
      if (sum(invalid) == 1L) "value." else "values.", call. = FALSE
    )
  }
  invisible(NULL)
}

.nsvb_remap_spcd <- function(spcd) {
  result <- spcd
  result[spcd == 204] <- 202
  result[spcd == 2042] <- 42
  result[spcd == 2098] <- 98
  result[spcd == 2242] <- 242
  result[spcd == 2263] <- 263
  result
}

.prepare_biomass_call <- function(dbh, ht, spcd, division, id, dots, units) {
  dots <- .capture_aux(dots)
  unknown <- setdiff(names(dots), names(.nsvb_dot_defaults))
  if (length(unknown)) {
    stop("undeclared auxiliary input: ", unknown[[1L]], call. = FALSE)
  }
  defaults <- .nsvb_dot_defaults
  if (identical(units, "metric")) {
    defaults$primary_top <- defaults$primary_top * 2.54
    defaults$secondary_top <- defaults$secondary_top * 2.54
    defaults$stump_ht <- defaults$stump_ht * 0.3048
  }
  supplied <- utils::modifyList(defaults, dots)
  values <- c(
    list(dbh = dbh, ht = ht, spcd = spcd, division = division),
    supplied,
    if (!is.null(id)) list(id = id)
  )
  prepared <- .prepare_vectors(
    values,
    numeric_names = setdiff(names(values), "id"),
    character_names = character(),
    aux = list()
  )
  values <- prepared$values
  result_status <- .input_status(
    prepared$size, values, c("dbh", "ht", "spcd", "division")
  )
  result_status <- .assign_status(result_status, values$dbh <= 0, 2L)
  result_status <- .assign_status(result_status, values$ht <= 0, 3L)

  .validate_integer_values(values$spcd, "spcd", 1, .Machine$integer.max)
  .validate_integer_values(values$division, "division", 0, 1999)
  .validate_integer_values(values$region, "region", 0, 99)
  .validate_integer_values(values$forest, "forest", 0, 99)
  .validate_integer_values(values$decay_class, "decay_class", 0, 5)
  if (any(is.finite(values$cull) & (values$cull < 0 | values$cull > 100))) {
    stop("cull has out-of-domain values.", call. = FALSE)
  }
  for (name in c("primary_top", "secondary_top")) {
    if (any(is.finite(values[[name]]) & values[[name]] <= 0)) {
      stop(name, " has out-of-domain values.", call. = FALSE)
    }
  }
  if (any(is.finite(values$stump_ht) & values$stump_ht < 0)) {
    stop("stump_ht has out-of-domain values.", call. = FALSE)
  }
  for (name in c("max_log_length", "min_log_length", "trim")) {
    if (any(is.finite(values[[name]]) & values[[name]] <= 0)) {
      stop(name, " has out-of-domain values.", call. = FALSE)
    }
  }
  auxiliary_names <- names(.nsvb_dot_defaults)
  for (name in auxiliary_names) {
    result_status <- .assign_status(
      result_status, !is.finite(values[[name]]), 1L,
      eligible = result_status == 0L &
        !name %in% c("max_log_length", "min_log_length", "trim")
    )
  }
  recognized <- .nsvb_remap_spcd(values$spcd) %in% species_reference$spcd
  result_status <- .assign_status(
    result_status, !recognized, 7L, eligible = result_status == 0L
  )
  list(size = prepared$size, values = values, status = result_status)
}

.biomass_impl <- function(dbh, ht, spcd, division, system, id, dots, units,
                          status, function_name = "biomass") {
  system <- .scalar_character(system, "system", "nsvb")
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  call <- .prepare_biomass_call(dbh, ht, spcd, division, id, dots, units)
  output <- matrix(NA_real_, nrow = call$size, ncol = length(.biomass_names))
  result_status <- call$status
  rows <- which(result_status == 0L)
  if (length(rows)) {
    values <- call$values
    native_dbh <- .diameter_to_native(values$dbh[rows], units, "imperial")
    native_ht <- .height_to_native(values$ht[rows], units, "imperial")
    native_primary_top <- .diameter_to_native(
      values$primary_top[rows], units, "imperial"
    )
    native_secondary_top <- .diameter_to_native(
      values$secondary_top[rows], units, "imperial"
    )
    native_stump <- .height_to_native(values$stump_ht[rows], units, "imperial")
    native_max <- .height_to_native(values$max_log_length[rows], units, "imperial")
    native_min <- .height_to_native(values$min_log_length[rows], units, "imperial")
    native_trim <- .height_to_native(values$trim[rows], units, "imperial")
    evaluated <- tv_cpp_nsvb_biomass_impl(
      native_dbh, native_ht, as.integer(values$spcd[rows]),
      as.integer(values$division[rows]), as.integer(values$region[rows]),
      as.integer(values$forest[rows]), as.integer(values$decay_class[rows]),
      values$cull[rows], native_primary_top, native_secondary_top, native_stump,
      native_max, native_min, rep(NA_real_, length(rows)),
      rep(NA_real_, length(rows)), native_trim, integer(length(rows)),
      integer(length(rows)), rep.int(-1L, length(rows)),
      rep.int(utf8ToInt("C"), length(rows)), threads()
    )
    output[rows, ] <- evaluated$value
    returned <- as.integer(evaluated$status)
    result_status[rows[returned != 0L]] <- returned[returned != 0L]
  }
  output <- .weight_from_native(output, units, "imperial")
  output[!result_status %in% c(0L, 52L, 102L), ] <- NA_real_
  colnames(output) <- .biomass_names
  result <- as.data.frame(output, stringsAsFactors = FALSE)
  if (!is.null(id)) {
    result <- cbind(id = call$values$id, result, stringsAsFactors = FALSE)
  }
  if (status_requested) {
    status_frame <- as.data.frame(matrix(
      rep(as.integer(result_status), length(.biomass_names)),
      nrow = call$size, ncol = length(.biomass_names)
    ))
    names(status_frame) <- paste0(.biomass_names, "_status")
    result <- cbind(result, status_frame, stringsAsFactors = FALSE)
  } else {
    .status_warning(function_name, result_status)
  }
  result
}

.nvel_volume_names <- c(
  "vol_total_cu", "vol_bf_gross", "vol_bf_net", "vol_cu_gross",
  "vol_cu_net", "cords", "vol_top_cu_gross", "vol_top_cu_net",
  "cords_top", paste0("vol", 10:13), "vol_stump_cu", "vol_tip_cu"
)

#' Record source-library log-length and measurement rules
#'
#' Validate a record of source-library segmentation and measurement settings.
#'
#' @details
#' Creates and validates the merchant-rule record used by the pinned National
#' Volume Estimator Library `MERCHRULES` interface. Numeric `NA` values
#' request the applicable library default when a model, region, product, and
#' tree are later supplied. Lengths and diameters use the units of the
#' consuming call: feet and inches for imperial calls, or meters and
#' centimeters for metric calls.
#'
#' @param even_or_odd National Volume Estimator Library `EVOD` segment-length code
#'   (unitless): `1` permits
#'   odd nominal lengths and `2` permits only even nominal lengths. Options 11
#'   through 14 define their own fixed-length behavior. `NA` requests the
#'   library default.
#'
#' Numeric nonempty vector. Accepted finite values are 1 or 2. Default `NA` applies when omitted.
#'
#' Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param option source library `OPT` segmentation code (unitless). Codes 11, 12, 13,
#'   and 14 use the library's 16-, 20-, 32-, and 40-foot scale conventions. Codes 21 through 24
#' select the four nominal-log top-segment rules.
#'
#' `NA`
#'   requests the library default. Numeric nonempty vector. Accepted finite values are 11 through
#' 14 or 21 through 24.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param maximum_length source library `MAXLEN`, the maximum nominal segment length, in
#'   the consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param minimum_length source library `MINLEN`, the minimum nominal segment length, in
#'   the consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param minimum_top_length source library `MINLENT`, the minimum accepted top-log
#'   length, in the consuming call's height units. Numeric nonempty vector. Accepted finite values
#' are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param merchantable_length source library `MERCHL`, the minimum length of primary
#'   product required for the tree to be merchantable, in the consuming call's
#'   height units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param primary_top source library `MTOPP`, the primary-product top diameter inside
#'   bark used for board-foot, cubic, and cord volume, in the consuming call's
#'   diameter units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param secondary_top source library `MTOPS`, the secondary-product top diameter inside
#'   bark used for cubic and cord volume, in the consuming call's diameter
#'   units. Numeric nonempty vector. Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param stump source library `STUMP`, the groundline-to-stump height, in the consuming
#'   call's height units. Numeric nonempty vector. Accepted finite values are at least zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param trim source library `TRIM`, the trim allowance added to each segment, in the
#'   consuming call's height units. Numeric nonempty vector. Accepted finite values are greater
#' than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param bark_ratio The source library bark-ratio auxiliary represented by `merchandiser`
#'   as the unitless constant diameter-inside-bark to diameter-outside-bark
#'   ratio in `(0, 1]`. `NA` leaves bark to the selected model or library
#'   default. Numeric nonempty vector.
#'
#' Accepted finite values are greater than zero and at most one. Default `NA` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors.
#' @param minimum_board_foot_dbh source library `MINBFD`, the minimum outside-bark diameter
#'   at breast height
#'   for board-foot eligibility, in the consuming call's diameter units. Numeric nonempty vector.
#' Accepted finite values are greater than zero.
#'
#' Default `NA` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param scribner source library `COR` Scribner-volume mode (unitless): `'table'` uses
#'   the Scribner table, `'factor'` uses factors, and `'regional'` requests
#'   the regional library default. Nonempty character vector, default `'regional'` when omitted.
#' Missing elements and unknown labels are errors.
#'
#' Example: `scribner = 'regional'`.
#' @param prod source library two-character product code (unitless), such as `'01'` for
#'   sawtimber or `'08'` for a nonsaw product.
#'   Nonempty character vector, default `'01'` when omitted.
#'   Missing elements and unknown labels are errors.
#' @param ht_type source library `HTTYPE` code (unitless): `'F'` denotes a physical
#'   height, `'L'` denotes a height expressed in logs, and `''` leaves the
#'   field unspecified for the consuming call. Nonempty character vector, default `''` when
#' omitted. Missing elements and unknown labels are errors.
#'
#' Example: `ht_type = ''`.
#' @param live source library live/dead status code (unitless): `'L'` for live or `'D'`
#'   for dead. Nonempty character vector, default `'L'` when omitted. Missing elements and unknown
#' labels are errors.
#'
#' Example: `live = 'L'`.
#' @param ctype Source call-origin code, unitless. `'C'` selects cruise, `'I'` selects Forest
#' Inventory and Analysis, and `'F'` selects Forest Vegetation Simulator. `'B'` selects the
#' alternate National Scale Volume and Biomass route.
#'
#' Nonempty character vector, default `'C'` when omitted. Missing elements and unknown labels are
#' errors. Example: `ctype = 'C'`.
#'
#' @param cull source library `CULL`, the whole-tree cull deduction as a percentage from
#'   0 through 100. Numeric nonempty vector. Accepted finite values are zero through 100.
#'
#' Default `0` applies when omitted. Numeric `NA` leaves a source
#'   default unset. Infinite values are errors.
#' @param forest source library two-digit national-forest code represented as an integer
#'   from 0 through 99 (unitless). Zero means no forest-specific selection. Numeric nonempty
#' vector.
#'
#' Accepted finite values are whole numbers from zero through 99. Default `0` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors.
#' @param district source library two-digit ranger-district code represented as an
#'   integer from 0 through 99 (unitless). Zero means no district-specific
#'   selection. Numeric nonempty vector.
#'
#' Accepted finite values are whole numbers from zero through 99. Default `0` applies when omitted.
#' Numeric `NA` leaves a source
#'   default unset.
#'
#' Infinite values are errors.
#'
#' @details
#' This record carries source defaults and supplied overrides without
#' calculating logs. Use [products_from_nvel_rules()] to translate one
#' complete record into product and utilization settings. Use [product()]
#' to enter buyer specifications directly.
#'
#' @return A validated list with class `treevolume_nvel_rules`. Input vectors
#'   are preserved for common-size recycling by the consuming calculation.
#' @export
#' @family source library identifiers and defaults
#' @source United States Forest Service source library `mrules.f`, commit
#'   `38548071d5aa652bb90c7f111f86b427f798a1c9`.
#' @usage
#'
#' ## Call signatures
#' nvel_rules(even_or_odd = NA_integer_, option = NA_integer_, maximum_length = NA_real_,
#'   minimum_length = NA_real_, minimum_top_length = NA_real_, merchantable_length = NA_real_,
#'   primary_top = NA_real_, secondary_top = NA_real_, stump = NA_real_, trim = NA_real_,
#'   bark_ratio = NA_real_, minimum_board_foot_dbh = NA_real_, scribner = 'regional', prod =
#'   '01', ht_type = '', live = 'L', ctype = 'C', cull = 0, forest = 0, district = 0)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Select source lengths from an example product
#' settings <- example_products(name = 'douglas_fir')
#'
#' ## Store the source-rule override
#' rules <- nvel_rules(maximum_length = first(x = settings$lengths), trim = settings$trim)
#'
#' ## Inspect the saved length override
#' rules$maximum_length
#' @section Record lengths and missing values:
#' This function preserves each argument vector and does not check that all vector lengths
#' match. A later consumer repeats length-one values and requires other lengths to match. The
#' product translator requires exactly one value in every field.
#'
#' An unset numeric value is not a usable default for that translator's required merchant
#' fields. Complete them first.
#' @section Status and missing values:
#' No tree codes are returned.
#' Invalid fields stop construction.
#'
#' Missing numeric fields remain missing and do not mean zero. No volume is calculated until
#' the record is translated and used in a tree calculation.
#' @seealso
#' [products_from_nvel_rules()] to translate a complete record, [product()] to specify products
#' directly, [nvel_source_revision()] for the source version used by the installed package.
nvel_rules <- function(
    even_or_odd = NA_integer_, option = NA_integer_,
    maximum_length = NA_real_, minimum_length = NA_real_,
    minimum_top_length = NA_real_, merchantable_length = NA_real_,
    primary_top = NA_real_, secondary_top = NA_real_, stump = NA_real_,
    trim = NA_real_,
    bark_ratio = NA_real_, minimum_board_foot_dbh = NA_real_,
    scribner = "regional", prod = "01", ht_type = "", live = "L",
    ctype = "C", cull = 0, forest = 0, district = 0) {
  values <- list(
    even_or_odd = even_or_odd, option = option,
    maximum_length = maximum_length, minimum_length = minimum_length,
    minimum_top_length = minimum_top_length,
    merchantable_length = merchantable_length, primary_top = primary_top,
    secondary_top = secondary_top, stump = stump, trim = trim,
    bark_ratio = bark_ratio,
    minimum_board_foot_dbh = minimum_board_foot_dbh,
    scribner = scribner, prod = prod, ht_type = ht_type, live = live,
    ctype = ctype, cull = cull, forest = forest, district = district
  )
  character_names <- c("scribner", "prod", "ht_type", "live", "ctype")
  for (name in character_names) {
    if (!is.character(values[[name]]) || !length(values[[name]]) ||
          anyNA(values[[name]])) {
      stop(name, " must be a character vector without missing values.",
           call. = FALSE)
    }
  }
  expected_numeric <- setdiff(names(values), character_names)
  for (name in expected_numeric) {
    if (!is.numeric(values[[name]]) || !length(values[[name]])) {
      stop(name, " must be a numeric vector.", call. = FALSE)
    }
  }
  numeric_names <- names(values)[vapply(values, is.numeric, logical(1))]
  for (name in numeric_names) {
    if (any(!is.na(values[[name]]) & !is.finite(values[[name]]))) {
      stop(name, " must contain finite values or NA.", call. = FALSE)
    }
  }
  if (any(!is.na(even_or_odd) & !even_or_odd %in% c(1L, 2L))) {
    stop("even_or_odd must be 1, 2, or NA.", call. = FALSE)
  }
  if (any(!is.na(option) & !option %in% c(11:14, 21:24))) {
    stop("option is not a recognized NVEL segmentation code.", call. = FALSE)
  }
  positive <- c(
    "maximum_length", "minimum_length", "minimum_top_length",
    "merchantable_length", "primary_top", "secondary_top", "trim",
    "bark_ratio", "minimum_board_foot_dbh"
  )
  for (name in positive) {
    if (any(!is.na(values[[name]]) & values[[name]] <= 0)) {
      stop(name, " must be positive or NA.", call. = FALSE)
    }
  }
  if (any(!is.na(bark_ratio) & bark_ratio > 1)) {
    stop("bark_ratio must be at most one.", call. = FALSE)
  }
  if (any(!is.na(stump) & stump < 0)) {
    stop("stump must be nonnegative or NA.", call. = FALSE)
  }
  if (any(!is.na(cull) & (cull < 0 | cull > 100))) {
    stop("cull must be between zero and 100.", call. = FALSE)
  }
  for (name in c("forest", "district")) {
    value <- values[[name]]
    if (any(!is.na(value) & (value != floor(value) | value < 0 | value > 99))) {
      stop(name, " must be an integer from zero through 99.", call. = FALSE)
    }
  }
  if (any(!scribner %in% c("regional", "table", "factor"))) {
    stop("scribner must be regional, table, or factor.", call. = FALSE)
  }
  if (any(!grepl("^[0-9]{2}$", prod))) {
    stop("prod must contain two-digit NVEL product codes.", call. = FALSE)
  }
  if (any(!ht_type %in% c("", "F", "L"))) {
    stop("ht_type must be empty, F, or L.", call. = FALSE)
  }
  if (any(!live %in% c("L", "D"))) {
    stop("live must be L or D.", call. = FALSE)
  }
  if (any(!ctype %in% c("C", "I", "F", "B"))) {
    stop("ctype must be C, I, F, or B.", call. = FALSE)
  }
  class(values) <- c("treevolume_nvel_rules", "list")
  values
}

.nvel_rule_defaults <- function(values, model, dbh, spcd, region, units) {
  defaults <- .nvel_reference("nvel_merchant_defaults.csv")
  length_names <- c(
    "maximum_length", "minimum_length", "minimum_top_length",
    "merchantable_length", "stump", "trim"
  )
  diameter_names <- c(
    "primary_top", "secondary_top", "minimum_board_foot_dbh"
  )
  in_caller_units <- function(value, name) {
    if (is.na(value) || units == "imperial") return(value)
    if (name %in% length_names) return(value * 0.3048)
    if (name %in% diameter_names) return(value * 2.54)
    value
  }
  for (row in seq_along(model)) {
    was_missing <- vapply(values, function(value) is.na(value[[row]]), logical(1))
    selected_region <- region[[row]]
    source_row <- match(selected_region, defaults$region)
    if (is.na(source_row)) source_row <- match(4L, defaults$region)
    for (name in intersect(names(values), names(defaults))) {
      if (is.numeric(values[[name]]) && is.na(values[[name]][[row]])) {
        values[[name]][[row]] <- in_caller_units(
          defaults[[name]][[source_row]], name
        )
      } else if (is.character(values[[name]]) &&
                   is.na(values[[name]][[row]])) {
        values[[name]][[row]] <- defaults[[name]][[source_row]]
      }
    }
    product <- values$prod[[row]]
    if (!is.na(selected_region) && selected_region == 1L) {
      profile_rules <- toupper(substr(model[[row]], 4L, 6L)) %in% c("FW2", "FW3") ||
        toupper(substr(model[[row]], 1L, 3L)) == "NVB"
      if (!profile_rules) {
        region1_other <- c(
          option = 12, maximum_length = 20, minimum_length = 10,
          minimum_top_length = 2, merchantable_length = 10
        )
        for (name in names(region1_other)[was_missing[names(region1_other)]]) {
          values[[name]][[row]] <- in_caller_units(region1_other[[name]], name)
        }
      } else if (product == "08") {
        for (name in c("minimum_length", "merchantable_length")) {
          if (was_missing[[name]]) {
            values[[name]][[row]] <- in_caller_units(16, name)
          }
        }
      }
    }
    if (!is.na(selected_region) && selected_region == 3L) {
      region3 <- if (product == "01") {
        c(minimum_length = 10, minimum_top_length = 10,
          merchantable_length = 10, primary_top = 6, secondary_top = 4,
          stump = 1)
      } else if (product == "08") {
        c(minimum_length = 10, minimum_top_length = 10,
          merchantable_length = 10, primary_top = 4, secondary_top = 4,
          stump = 0.5)
      } else if (product == "14") {
        c(minimum_length = 10, minimum_top_length = 10,
          merchantable_length = 10, primary_top = 4, secondary_top = 1,
          stump = 0.5)
      } else if (product == "20") {
        c(minimum_length = 2, minimum_top_length = 2,
          merchantable_length = 8, primary_top = 1, secondary_top = 1,
          stump = 0.5)
      } else if (product == "07") {
        c(minimum_length = 4, minimum_top_length = 4,
          merchantable_length = 8, primary_top = 2, secondary_top = 2,
          stump = 0.5)
      } else {
        c(minimum_length = 10, minimum_top_length = 10,
          merchantable_length = 10, primary_top = 4, secondary_top = 4,
          stump = 0.5)
      }
      for (name in names(region3)[was_missing[names(region3)]]) {
        values[[name]][[row]] <- in_caller_units(region3[[name]], name)
      }
    }
    if (!is.na(selected_region) && selected_region == 7L &&
          was_missing[["primary_top"]]) {
      dbh_inches <- .diameter_to_native(dbh[[row]], units, "imperial")
      top_inches <- floor(0.184 * dbh_inches + 2.24 + 0.5)
      values$primary_top[[row]] <- .diameter_from_native(
        top_inches, units, "imperial"
      )
    }
    if (!is.na(selected_region) && selected_region == 8L) {
      if (was_missing[["primary_top"]]) {
        top_inches <- if (product == "08") 0.1 else
          if (!is.na(spcd[[row]]) && spcd[[row]] < 300L) 7 else 9
        values$primary_top[[row]] <- .diameter_from_native(
          top_inches, units, "imperial"
        )
      }
      if (was_missing[["stump"]]) {
        values$stump[[row]] <- .height_from_native(
          if (product == "01") 1 else 0.5, units, "imperial"
        )
      }
      if (product == "08") {
        values$merchantable_length[[row]] <- .height_from_native(
          12, units, "imperial"
        )
      }
    }
    if (!is.na(selected_region) && selected_region == 9L) {
      if (was_missing[["primary_top"]]) {
        top_inches <- if (!is.na(spcd[[row]]) && spcd[[row]] < 300L) {
          7.6
        } else {
          9.6
        }
        values$primary_top[[row]] <- .diameter_from_native(
          top_inches, units, "imperial"
        )
      }
      if (was_missing[["stump"]]) {
        values$stump[[row]] <- .height_from_native(
          if (product == "01") 1 else 0.5, units, "imperial"
        )
      }
    }
    if (!is.na(values$primary_top[[row]]) &&
          !is.na(values$secondary_top[[row]]) &&
          values$secondary_top[[row]] > values$primary_top[[row]]) {
      values$secondary_top[[row]] <- values$primary_top[[row]]
    }
  }
  values
}

.nvel_model_region <- function(model) {
  first <- substr(model, 1L, 1L)
  result <- suppressWarnings(as.integer(first))
  result[first == "A"] <- 10L
  result[is.na(result)] <- 0L
  result
}

.nvel_model_species <- function(model) {
  suppressWarnings(as.integer(substr(model, nchar(model) - 2L, nchar(model))))
}

.nvel_model_division <- function(model) {
  result <- suppressWarnings(as.integer(substr(model, 5L, 7L)))
  result[substr(model, 4L, 4L) == "M"] <-
    result[substr(model, 4L, 4L) == "M"] + 1000L
  result
}

.nvel_status_frame <- function(status_matrix) {
  result <- as.data.frame(status_matrix)
  names(result) <- paste0(
    c(.nvel_volume_names, "n_logs_primary", "n_logs_secondary"),
    "_status"
  )
  result[] <- lapply(result, as.integer)
  result
}

# Reproduce the R9LOGS log-count guard without exposing incomplete log arrays.
.nvel_r9_log_overflow <- function(values, resolved, aux, rows, units) {
  source_height <- function(target) {
    result <- rep(NA_real_, length(rows))
    by_model <- split(seq_along(rows), values$model[rows])
    native_dbh <- .diameter_to_native(values$dbh[rows], units, "imperial")
    native_ht <- .height_to_native(values$ht[rows], units, "imperial")
    native_target <- .diameter_to_native(target, units, "imperial")
    for (selected in by_model) {
      row <- rows[selected[[1L]]]
      dictionary_index <- match(values$model[[row]], resolved$dictionary)
      model <- resolved$models[[dictionary_index]]
      evaluated <- .compiled_result(
        model, 11L, native_dbh[selected], native_ht[selected],
        native_target[selected], numeric(length(selected)),
        rep(1, length(selected)), aux, rows[selected]
      )
      result[selected] <- evaluated$value
    }
    result
  }
  saw_height <- source_height(values$primary_top[rows])
  pulp_height <- source_height(values$secondary_top[rows])
  stump <- .height_to_native(values$stump[rows], units, "imperial")
  minimum <- .height_to_native(
    values$minimum_length[rows], units, "imperial"
  )
  maximum <- .height_to_native(
    values$maximum_length[rows], units, "imperial"
  )
  trim <- .height_to_native(values$trim[rows], units, "imperial")

  vapply(seq_along(rows), function(index) {
    saw <- saw_height[[index]]
    pulp <- pulp_height[[index]]
    if (!all(is.finite(c(saw, pulp, stump[[index]], minimum[[index]],
                         maximum[[index]], trim[[index]])))) {
      return(FALSE)
    }
    base <- stump[[index]]
    min_length <- minimum[[index]]
    max_length <- maximum[[index]]
    log_trim <- trim[[index]]
    merch <- saw - base
    primary <- as.integer(merch / (max_length + log_trim))
    if (primary > 20L) return(TRUE)
    leftover <- merch - (max_length + log_trim) * primary - log_trim
    lengths <- numeric(20L)
    if (!(merch < min_length + log_trim ||
            (primary == 0L && leftover < min_length + log_trim))) {
      last <- primary
      if (last > 0L) lengths[seq_len(last)] <- max_length
      if (leftover >= min_length + log_trim) {
        primary <- primary + 1L
        last <- last + 1L
        if (last > 20L) return(TRUE)
        lengths[[last]] <- leftover
      }
      if (primary == 1L) {
        lengths[[1L]] <- as.integer(lengths[[1L]] / 2) * 2
      } else if (leftover < min_length) {
        lengths[[last]] <- as.integer(lengths[[last]] / 2) * 2
      } else {
        lengths[[last]] <-
          as.integer(as.integer((max_length + leftover) / 2) / 2) * 2
        lengths[[last - 1L]] <- as.integer(
          as.integer(max_length + leftover - lengths[[last]]) / 2
        ) * 2
      }
    }
    if (!(pulp > 0 && pulp > saw)) return(FALSE)
    saw_end <- base
    if (primary > 0L) {
      saw_end <- saw_end + sum(lengths[seq_len(primary)] + log_trim)
    }
    merch <- pulp - saw_end
    secondary <- as.integer(merch / (max_length + log_trim))
    leftover <- merch - (max_length + log_trim) * secondary - log_trim
    if (merch < min_length + log_trim ||
          (secondary == 0L && leftover < min_length + log_trim)) {
      secondary <- 0L
    }
    if (secondary > 0L || leftover > min_length + log_trim) {
      last <- primary + secondary
      if (last > 20L) return(TRUE)
      if (leftover >= min_length + log_trim && last + 1L > 20L) {
        return(TRUE)
      }
    }
    FALSE
  }, logical(1))
}

#' Compare the source library's volume components
#'
#' Return source-library volume components for fixture comparison. Unsupported components
#' remain missing.
#'
#' @inheritParams dib
#' @param rules A list made by [nvel_rules()], default `nvel_rules()`. Its fields are listed in the
#' corresponding section. Omission requests source defaults where
#'   the chosen equation and region provide them.
#'
#' A missing object or other
#'   class is an error.
#' @param id Optional character identifiers, length one or one per tree. Default `NULL` omits the
#'   column. Missing labels are retained, and labels
#'   need not be unique.
#'
#' Unitless. Example: `example_trees$tree`.
#' @param ... Named equation inputs listed in the corresponding section, with optional numeric
#'   `spcd`, `division`, and `region` described in the source selection section. Length one or one
#' per tree.
#'
#' Unknown names stop the call.
#' @param status One nonmissing logical flag, default `FALSE` for row warnings.
#'   `TRUE` adds an integer status field for each component and both log counts.
#'   Unitless. Example: `status = TRUE`.
#'
#' @details Source defaults are applied before equation evaluation. Ordinary
#'   stem equations supply total and stump volume, with unsupported log scales
#'   missing. The national biomass equations supply their source volume record,
#'   while merchantable components unavailable through this comparison remain
#'   missing. This function does not select the logs returned by [merchandise()].
#'
#' @section Source selection through dots:
#' `spcd` is a numeric inventory species code, unitless, defaulting to the identifier's final
#' three characters. `division` is a numeric ecological code, unitless, derived from positions
#' five through seven and the mountain prefix when omitted. `region` is a numeric source
#' region, unitless, derived from the first identifier character when omitted, with `A`
#' interpreted as 10 and other nondigits as zero.
#'
#' These values are not independently checked against complete geographic tables here. Supply
#' recognized finite whole-number codes for source comparisons. Examples are `spcd = 202`,
#' `division = 1240`, and `region = 6`.
#'
#' Missing values do not request a second geographic lookup. Use the exported source lookup
#' functions for assignments.
#'
#' @return A data frame with the following numeric source components in source
#'   order, optional character `id`, integer `n_logs_primary` and
#'   `n_logs_secondary`, and integer `errflag`. Counts are unitless source
#'   log counts, not counts of merchandising-result rows. The error flag is
#'   the source error number when a source error was returned, or the package
#'   status otherwise.
#'
#' Component status columns append `_status` to each
#'   quantity and count name. There are no list columns or user-filled defaults.
#' @section Volume component fields:
#' `vol_total_cu` is total inside-bark volume. `vol_bf_gross` and `vol_bf_net`
#' are gross and net primary-product board-foot quantities. `vol_cu_gross`
#' and `vol_cu_net` are gross and net primary-product cubic volume.
#'
#' `cords`
#' is its source cord quantity. `vol_top_cu_gross` and `vol_top_cu_net` are
#' secondary-product cubic quantities, and `cords_top` is the corresponding
#' cord quantity. These merchantable fields are currently unavailable in
#' this comparison and are returned missing with `capability_missing`.
#'
#' `vol10`, `vol11`, `vol12`, and `vol13` retain source-array positions whose measurement
#' definitions are not supplied by this source comparison. Do not interpret them as
#' merchantable quantities. Their source units are not converted by a metric call.
#'
#' Ordinary stem equations set `vol11` and `vol13` to zero for source comparison and leave
#' `vol10` and `vol12` missing. `vol_stump_cu` is source stump volume and `vol_tip_cu` is
#' source tip volume. Imperial cubic fields use cubic feet.
#'
#' Board feet and cords retain source units. The metric conversion limitation applies to ordinary
#' stem equations.
#' @section Metric limitation:
#' For ordinary stem equations, total and stump
#' volumes already use the requested units before the final metric conversion is applied again.
#' Thus those metric values are converted twice and must not be used as correct cubic meters.
#'
#' The national biomass equation record starts in cubic feet and receives the intended
#' conversion. Use [stem_volume()] for bounded cubic volume.
#' @section Status and missing values:
#' Zero means an available source
#' comparison quantity, not necessarily merchantable volume.
#'
#' Missing inputs, invalid dimensions, unknown equations, unavailable components, and source
#' failures return missing quantities. [status_codes()] describes the input and model diagnoses.
#' Source failures retain their library error offset.
#'
#' `species_out_of_scope` retains an out-of-scope value. `not_unique` retains the highest crossing.
#'
#' Partial component availability does not justify filling other components with zero.
#' @inheritSection dib Equation inputs through dots
#' @inheritSection products_from_nvel_rules Source rule fields
#' @seealso [stem_volume()] for cubic volume, [biomass()] for biomass,
#' [merchandise()] for proposed logs and scaled quantities.
#' @keywords internal
#' @usage
#'
#' ## Call signatures
#' nvel_volume(dbh, ht, model, rules = nvel_rules(), id = NULL, ..., units = 'imperial', status =
#'   FALSE)
#' @examples
#' ## Calculate bounded volume for the shipped trees
#' stem_volume(dbh = example_trees$dbh,
#'             ht = example_trees$ht,
#'             model = example_trees$model)
nvel_volume <- function(dbh, ht, model, rules = nvel_rules(), id = NULL, ...,
                        units = "imperial", status = FALSE) {
  if (!inherits(rules, "treevolume_nvel_rules")) {
    stop("rules must be created by nvel_rules().", call. = FALSE)
  }
  units <- .validate_units(units)
  status_requested <- .validate_status(status)
  dots <- .capture_aux(list(...))
  nvel_names <- intersect(names(dots), c("spcd", "division", "region"))
  nvel_values <- dots[nvel_names]
  aux <- dots[setdiff(names(dots), nvel_names)]
  if (is.null(nvel_values$spcd)) nvel_values$spcd <- .nvel_model_species(model)
  if (is.null(nvel_values$division)) {
    nvel_values$division <- .nvel_model_division(model)
  }
  if (is.null(nvel_values$region)) nvel_values$region <- .nvel_model_region(model)
  if (!is.null(rules$bark_ratio) && is.null(aux$bark_ratio)) {
    aux$bark_ratio <- rules$bark_ratio
  }
  numeric_rule_names <- names(rules)[vapply(rules, is.numeric, logical(1))]
  character_rule_names <- setdiff(names(rules), numeric_rule_names)
  numeric_values <- c(
    list(dbh = dbh, ht = ht), rules[numeric_rule_names],
    nvel_values[c("spcd", "division", "region")]
  )
  character_values <- c(list(model = model), rules[character_rule_names])
  if (!is.null(id)) character_values$id <- id
  prepared <- .prepare_vectors(
    c(numeric_values, character_values),
    numeric_names = names(numeric_values),
    character_names = names(character_values), aux = aux
  )
  values <- prepared$values
  values[names(rules)] <- .nvel_rule_defaults(
    values[names(rules)], values$model, values$dbh, values$spcd,
    values$region, units
  )
  prepared$aux <- .set_aux_caller_units(prepared$aux, units)
  if (!is.null(prepared$aux$bark_ratio) &&
        all(is.na(prepared$aux$bark_ratio))) prepared$aux$bark_ratio <- NULL
  result_status <- .input_status(
    prepared$size, values, c("dbh", "ht", "model")
  )
  result_status <- .assign_status(result_status, values$dbh <= 0, 2L)
  result_status <- .assign_status(result_status, values$ht <= 0, 3L)
  resolved <- .resolve_models(values$model, prepared$aux, result_status)
  result_status <- resolved$status
  output <- matrix(NA_real_, nrow = prepared$size, ncol = 15L)
  component_status <- matrix(
    rep(result_status, 17L), nrow = prepared$size, ncol = 17L
  )
  primary_logs <- secondary_logs <- rep(NA_integer_, prepared$size)
  for (group_index in seq_along(resolved$dictionary)) {
    model_object <- resolved$models[[group_index]]
    if (is.null(model_object)) next
    group_rows <- resolved$groups[[as.character(group_index)]]
    rows <- group_rows[result_status[group_rows] %in% c(0L, 52L, 102L)]
    if (!length(rows)) next
    if (!identical(model_object$family, "nsvb")) next
    native <- function(name) {
      .height_to_native(values[[name]][rows], units, "imperial")
    }
    scribner <- ifelse(
      values$scribner[rows] == "regional", -1L,
      ifelse(values$scribner[rows] == "table", 1L, 0L)
    )
    evaluated <- tv_cpp_nsvb_biomass_impl(
      .diameter_to_native(values$dbh[rows], units, "imperial"),
      native("ht"), as.integer(values$spcd[rows]),
      as.integer(values$division[rows]), as.integer(values$region[rows]),
      as.integer(values$forest[rows]), integer(length(rows)),
      values$cull[rows],
      .diameter_to_native(values$primary_top[rows], units, "imperial"),
      .diameter_to_native(values$secondary_top[rows], units, "imperial"),
      native("stump"), native("maximum_length"), native("minimum_length"),
      native("minimum_top_length"), native("merchantable_length"),
      native("trim"), as.integer(values$even_or_odd[rows]),
      as.integer(values$option[rows]), as.integer(scribner),
      vapply(values$ctype[rows], utf8ToInt, integer(1)), threads()
    )
    output[rows, ] <- evaluated$volume
    primary_logs[rows] <- evaluated$n_logs_primary
    secondary_logs[rows] <- evaluated$n_logs_secondary
    result_status[rows] <- evaluated$status
    component_status[rows, ] <- evaluated$status
    missing_merch <- evaluated$status %in% c(0L, 52L, 102L)
    output[rows[missing_merch], 2:9] <- NA_real_
    component_status[rows[missing_merch], 2:9] <- 53L
  }
  generic <- which(
    result_status %in% c(0L, 52L, 102L) &
      vapply(seq_len(prepared$size), function(row) {
        index <- match(values$model[[row]], resolved$dictionary)
        object <- if (is.na(index)) NULL else resolved$models[[index]]
        !is.null(object) && !identical(object$family, "nsvb")
      }, logical(1))
  )
  if (length(generic)) {
    compatibility <- vapply(generic, function(row) {
      index <- match(values$model[[row]], resolved$dictionary)
      family <- resolved$models[[index]]$family
      if (family %in% c("clark_r8", "clark_r9")) {
        "clark"
      } else if (identical(family, "r4_driver")) {
        "r4"
      } else {
        "public"
      }
    }, character(1))
    total <- data.frame(
      value = rep(NA_real_, length(generic)),
      status = rep(50L, length(generic))
    )
    for (path in c("public", "clark", "r4")) {
      at <- which(compatibility == path)
      if (!length(at)) next
      selected <- generic[at]
      subset_aux <- lapply(prepared$aux, `[`, selected)
      if (path == "public") {
        total[at, ] <- do.call(stem_volume, c(list(
          dbh = values$dbh[selected], ht = values$ht[selected],
          model = values$model[selected], lower = 0,
          lower_type = "height", upper_type = "tip",
          units = units, status = TRUE
        ), subset_aux))
      } else {
        total[at, ] <- .stem_volume_impl(
          dbh = values$dbh[selected], ht = values$ht[selected],
          model = values$model[selected],
          lower = if (path == "clark") values$stump[selected] else 0,
          lower_type = "height", upper = 0, upper_type = "tip",
          bark = "inside", stump_ht = NULL, aux = subset_aux,
          units = units, status = TRUE, function_name = "nvel_volume",
          operation = 9L
        )
      }
    }
    total_good <- total$status %in% c(0L, 52L, 102L)
    output[generic[total_good], 1L] <- total$value[total_good]
    component_status[generic, 1L] <- total$status
    failed <- generic[!total_good]
    if (length(failed)) {
      result_status[failed] <- total$status[!total_good]
      component_status[failed, ] <- total$status[!total_good]
    }
    good <- generic[total_good]
    if (length(good)) {
      good_aux <- lapply(prepared$aux, `[`, good)
      clark9 <- vapply(good, function(row) {
        index <- match(values$model[[row]], resolved$dictionary)
        identical(resolved$models[[index]]$family, "clark_r9")
      }, logical(1))
      if (any(clark9)) {
        rows <- good[clark9]
        stump <- .stem_volume_impl(
          dbh = values$dbh[rows], ht = values$ht[rows],
          model = values$model[rows], lower = 0, lower_type = "height",
          upper = values$stump[rows], upper_type = "height",
          bark = "inside", stump_ht = NULL,
          aux = lapply(prepared$aux, `[`, rows), units = units, status = TRUE,
          function_name = "nvel_volume", operation = 10L
        )
        stump_ok <- stump$status %in% c(0L, 52L, 102L)
        output[rows[stump_ok], 14L] <- stump$value[stump_ok]
        component_status[rows, 14L] <- stump$status
      }
      if (any(!clark9)) {
        rows <- good[!clark9]
        stump_diameter <- do.call(dib, c(list(
          dbh = values$dbh[rows], ht = values$ht[rows],
          h = values$stump[rows], model = values$model[rows],
          units = units, status = TRUE
        ), lapply(prepared$aux, `[`, rows)))
        stump_ok <- stump_diameter$status %in% c(0L, 52L, 102L)
        stump_height <- .height_to_native(
          values$stump[rows], units, "imperial"
        )
        stump_dib <- .diameter_to_native(
          stump_diameter$value, units, "imperial"
        )
        stump_volume <- 0.005454154 * stump_dib^2 * stump_height
        output[rows[stump_ok], 14L] <- .volume_from_native(
          stump_volume[stump_ok], units, "imperial"
        )
        component_status[rows, 14L] <- stump_diameter$status
      }
      output[good, c(11L, 13L)] <- 0
      component_status[good, c(11L, 13L)] <- 0L
      component_status[good, c(2:10, 12, 15:17)] <- 53L
      if (identical(.treevolume_compat(), "nvel") && any(clark9)) {
        rows <- good[clark9]
        overflow <- .nvel_r9_log_overflow(
          values, resolved, prepared$aux, rows, units
        )
        rows <- rows[overflow]
        if (length(rows)) {
          result_status[rows] <- 312L
          component_status[rows, ] <- 312L
          output[rows, ] <- NA_real_
          primary_logs[rows] <- secondary_logs[rows] <- NA_integer_
        }
      }
    }
  }
  colnames(output) <- .nvel_volume_names
  if (identical(units, "metric")) {
    cubic <- c(1L, 4L, 5L, 7L, 8L, 14L, 15L)
    output[, cubic] <- .volume_from_native(output[, cubic, drop = FALSE],
                                           "metric", "imperial")
  }
  failed <- !result_status %in% c(0L, 52L, 102L)
  output[failed, ] <- NA_real_
  primary_logs[failed] <- secondary_logs[failed] <- NA_integer_
  errflag <- ifelse(
    result_status >= 300L & result_status <= 399L,
    result_status - 300L, result_status
  )
  result <- data.frame(
    output, n_logs_primary = primary_logs,
    n_logs_secondary = secondary_logs, errflag = as.integer(errflag),
    check.names = FALSE
  )
  if (!is.null(id)) {
    result <- cbind(id = values$id, result, stringsAsFactors = FALSE)
  }
  if (status_requested) {
    result <- cbind(result, .nvel_status_frame(component_status),
                    stringsAsFactors = FALSE)
  } else {
    warning_status <- result_status
    capability <- apply(component_status == 53L, 1L, any)
    warning_status[warning_status %in% c(0L, 52L, 102L) & capability] <- 53L
    .status_warning("nvel_volume", warning_status, resolved$details)
  }
  result
}

#' Estimate tree biomass and carbon by component
#'
#' Estimate biomass components, aboveground carbon, and carbon dioxide equivalent from tree
#' measurements and ecological divisions.
#'
#' @param dbh Required positive finite numeric outside-bark breast-height
#'   diameter, length one or one per tree. Inches in imperial units or
#'   centimeters in metric units. Omission is an error.
#'
#' Missing gives `na_input`. Example: `example_trees$dbh`.
#' @param ht Required positive finite numeric total height above ground,
#'   length one or one per tree. Feet or meters. Omission is an error.
#'
#' Missing gives `na_input`. Example: `example_trees$ht`.
#' @param spcd Required positive whole-number numeric inventory species codes,
#'   length one or one per tree. Unitless. Missing gives `na_input`.
#'
#' Invalid
#'   finite codes stop the call, and unrecognized species give `unknown_species`. Omission is an
#' error.
#' @param division Required numeric whole-number ecological code from zero
#'   through 1999, length one or one per tree. Unitless. Missing gives `na_input`,
#'   invalid finite values stop the call.
#'
#' Province codes are rounded down to
#'   the broader division for equation selection.
#' @param system One character value, `'nsvb'` (default), selecting the national
#'   biomass equations. No other system is supported. Missing or unknown
#'   labels are errors.
#'
#' Unitless. Example: `system = 'nsvb'`.
#' @param id Optional identifiers, length one or one per tree. Default `NULL`
#'   omits the identifier column. Values are retained without uniqueness or
#'   missing-value validation.
#'
#' Unitless. Example: `example_trees$tree`.
#' @param ... Named inputs . Each has length one
#'   or one per tree.
#'
#' Unknown names and invalid finite values stop the call.
#' @param units One character value, `'imperial'` (default) or `'metric'`. Imperial uses inches,
#'   feet, and pounds. Metric uses centimeters, meters,
#'   and kilograms.
#'
#' Missing or unknown labels are errors. Example: `'metric'`.
#' @param status One nonmissing logical value, default `FALSE`. `TRUE` adds
#'   one integer code column for every quantity and suppresses row warnings. These codes are
#' unitless.
#'
#' Invalid flags stop the call. Example: `TRUE`.
#' @details The National Scale Volume and Biomass system supplies the national
#'   biomass equations. Wood, bark, and branch predictions are adjusted so
#'   their combined mass agrees with the aboveground prediction. Stem
#'   sections are allocated using the source product-length rules and top
#'   limits.
#'
#' Carbon excludes foliage. The green quantities use one
#'   green-to-dry multiplier from regional or species factors across the dry
#'   components. They are separate from [green_weight()]'s volume conversion.
#'
#'   These product limits belong to the biomass calculation. They do not use
#'   a product table or reproduce the cuts in a merchandising result.
#' @return A data frame with one row per tree in input order and the fields. Mass units follow
#' `units`, including the `co2e` column.
#' @section Tree condition and measurement limits through dots:
#' All inputs are numeric, length one or one per tree. Defaults apply when
#' omitted, not when explicitly missing, except the length overrides below.
#' \describe{
#' \item{`decay_class`}{Whole number from zero through 5, unitless. Default
#' `0` describes a live tree. Classes 1 through 5 apply the dead-tree density,
#' bark, branch, foliage, and carbon rules.
#'
#' Missing gives `na_input`. }
#' \item{`cull`}{Percentage from zero through 100. Default `0` means no cull
#' adjustment. Missing gives `na_input`.  This is the biomass
#' system's adjustment and is not a located merchandising defect.}
#' \item{`region`, `forest`}{Whole-number geographic codes from zero through
#' 99. Defaults `0` select species-reference green-weight factors instead of
#' a forest-specific factor. Missing gives `na_input`.
#'
#' Unitless. }
#' \item{`primary_top`, `secondary_top`}{Positive inside-bark product top
#' diameters. Defaults are 6 and 4 inches or 15.24 and 10.16 centimeters.
#' Missing gives `na_input`. }
#' \item{`stump_ht`}{Nonnegative stump height above ground. Default is 1 foot
#' or 0.3048 meter. Missing gives `na_input`. }
#' \item{`max_log_length`, `min_log_length`, `trim`}{Positive length overrides
#' in feet or meters. Default `NA_real_` leaves the source rule unchanged. Nonfinite values also
#' leave these overrides unset.
#'
#' Finite zero or negative
#' values are errors, including zero trim. }
#' }
#' @section Biomass result fields:
#' Every quantity is numeric pounds under imperial units or kilograms under
#' metric units. The `dry_` prefix means oven-dry mass and `green_` includes
#' moisture. These overlapping totals and components must not all be added.
#'
#' Failed rows are missing. \describe{
#' \item{`dry_agb_no_foliage`, `green_agb_no_foliage`}{Oven-dry and green aboveground biomass
#'   excluding foliage, respectively.}
#' \item{`dry_stem_wood`, `green_stem_wood`}{Oven-dry and green whole-stem wood, respectively.}
#' \item{`dry_stem_bark`, `green_stem_bark`}{Oven-dry and green whole-stem bark, respectively.}
#' \item{`dry_stump_wood`, `green_stump_wood`}{Oven-dry and green wood below stump height,
#'   respectively.}
#' \item{`dry_stump_bark`, `green_stump_bark`}{Oven-dry and green bark below stump height,
#'   respectively.}
#' \item{`dry_saw_wood`, `green_saw_wood`}{Oven-dry and green wood assigned to the primary
#'   saw section, respectively.}
#' \item{`dry_saw_bark`, `green_saw_bark`}{Oven-dry and green bark assigned to the primary
#'   saw section, respectively.}
#' \item{`dry_topwood_wood`, `green_topwood_wood`}{Oven-dry and green wood in the secondary
#'   merchantable section, respectively.}
#' \item{`dry_topwood_bark`, `green_topwood_bark`}{Oven-dry and green bark in the secondary
#'   merchantable section, respectively.}
#' \item{`dry_tip_wood`, `green_tip_wood`}{Oven-dry and green wood above the secondary
#'   merchantable section, respectively.}
#' \item{`dry_tip_bark`, `green_tip_bark`}{Oven-dry and green bark above the secondary
#'   merchantable section, respectively.}
#' \item{`dry_branches`, `green_branches`}{Oven-dry and green branch biomass, respectively.}
#' \item{`dry_foliage`, `green_foliage`}{Oven-dry and green foliage biomass, respectively.}
#' \item{`dry_top_and_limb`, `green_top_and_limb`}{Oven-dry and green aboveground biomass
#'   excluding foliage, stump, and primary and secondary merchantable sections,
#'   respectively.}
#' \item{`carbon`}{Carbon mass in aboveground biomass excluding foliage.}
#' \item{`co2e`}{Carbon multiplied by 44/12, in the same pounds or kilograms.
#' The separate [co2e()] function instead always returns metric tons.}
#' \item{`id`}{Optional input identifier, present only when supplied. Unitless.}
#' }
#' With `status = TRUE`, every quantity has an integer, unitless companion
#' named by appending `_status`, for example `dry_stem_wood_status`. The same
#' row code is repeated for every component.
#'
#' There are no list columns.
#' @section Status and missing values:
#' `ok` means available model output. Missing inputs, nonpositive dimensions, unrecognized species,
#' and equation failures leave every component missing. [status_codes()] identifies these
#' diagnoses.
#'
#' A component can be zero for dead foliage or an absent merchantable section. Component statuses
#' do not describe merchandising results.
#' @seealso [biomass_component()] for one quantity, [co2e()] for metric tons
#'   of carbon dioxide equivalent, [nsvb_division_xy()] for a location lookup.
#' @references Westfall, J.A. et al. (2024). *A national-scale tree volume,
#'   biomass, and carbon modeling system for the United States*. General
#'   Technical Report WO-104. \doi{10.2737/WO-GTR-104}.
#' @export
#' @usage
#'
#' ## Call signatures
#' biomass(dbh, ht, spcd, division, system = 'nsvb', id = NULL, ..., units = 'imperial', status =
#'   FALSE)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Estimate biomass for the first example trees
#' biomass(dbh = example_trees$dbh,
#'         ht = example_trees$ht,
#'         spcd = example_trees$spcd,
#'         division = 1240) %>%
#'   select(dry_stem_wood, dry_stem_bark, dry_branches, carbon) %>%
#'   slice_head(n = 3)
biomass <- function(dbh, ht, spcd, division, system = "nsvb", id = NULL, ...,
                    units = "imperial", status = FALSE) {
  .biomass_impl(
    dbh, ht, spcd, division, system, id, list(...), units, status
  )
}

#' Calculate one tree biomass component
#'
#' Return one named biomass component per tree, optionally paired with calculation status.
#'
#' @inheritParams biomass
#' @return Numeric mass in pounds or kilograms per tree, or a data frame with numeric `value`
#'   in those units and integer unitless `status`.
#' @details The selected component follows the same equations and limits as
#'   the full biomass table. No inventory expansion is applied.
#' @section Tree condition and measurement limits through dots:
#' All inputs are numeric, length one or one per tree. Defaults apply when
#' omitted, not when explicitly missing, except the length overrides below.
#' \describe{
#' \item{`decay_class`}{Whole number from zero through 5, unitless. Default
#' `0` describes a live tree. Classes 1 through 5 apply the dead-tree density,
#' bark, branch, foliage, and carbon rules.
#'
#' Missing gives `na_input`. }
#' \item{`cull`}{Percentage from zero through 100. Default `0` means no cull
#' adjustment. Missing gives `na_input`.  This is the biomass
#' system's adjustment and is not a located merchandising defect.}
#' \item{`region`, `forest`}{Whole-number geographic codes from zero through
#' 99. Defaults `0` select species-reference green-weight factors instead of
#' a forest-specific factor. Missing gives `na_input`.
#'
#' Unitless. }
#' \item{`primary_top`, `secondary_top`}{Positive inside-bark product top
#' diameters. Defaults are 6 and 4 inches or 15.24 and 10.16 centimeters.
#' Missing gives `na_input`. }
#' \item{`stump_ht`}{Nonnegative stump height above ground. Default is 1 foot
#' or 0.3048 meter. Missing gives `na_input`. }
#' \item{`max_log_length`, `min_log_length`, `trim`}{Positive length overrides
#' in feet or meters. Default `NA_real_` leaves the source rule unchanged. Nonfinite values also
#' leave these overrides unset.
#'
#' Finite zero or negative
#' values are errors, including zero trim. }
#' }
#' @section Status and missing values:
#' `ok` means available model output. Missing inputs, nonpositive dimensions, unrecognized species,
#' and equation failures leave every component missing. [status_codes()] identifies these
#' diagnoses.
#'
#' A component can be zero for dead foliage or an absent merchantable section. Component statuses
#' do not describe merchandising results.
#' @seealso [biomass()] for all components, [green_weight()] for weight from
#'   cubic volume, [carbon_fraction()] for the live-tree carbon fraction.
#' @export
#' @usage
#'
#' ## Call signatures
#' biomass_component(dbh, ht, spcd, division, component, system = 'nsvb', ..., units =
#'   'imperial', status = FALSE)
#' @examples
#' ## Estimate stem mass from inventory measurements
#' biomass_component(dbh = example_trees$dbh,
#'                   ht = example_trees$ht,
#'                   spcd = example_trees$spcd,
#'                   division = 1240,
#'                   component = 'dry_stem_wood')
#' @param component Required single character quantity name from the fields, for example
#' `'dry_stem_wood'`. No default. Missing or unknown
#'   names are errors.
#'
#' The label is unitless.
#' @section Biomass result fields:
#' Every quantity is numeric pounds under imperial units or kilograms under
#' metric units. The `dry_` prefix means oven-dry mass and `green_` includes
#' moisture. These overlapping totals and components must not all be added.
#'
#' Failed rows are missing. \describe{
#' \item{`dry_agb_no_foliage`, `green_agb_no_foliage`}{Oven-dry and green aboveground biomass
#'   excluding foliage, respectively.}
#' \item{`dry_stem_wood`, `green_stem_wood`}{Oven-dry and green whole-stem wood, respectively.}
#' \item{`dry_stem_bark`, `green_stem_bark`}{Oven-dry and green whole-stem bark, respectively.}
#' \item{`dry_stump_wood`, `green_stump_wood`}{Oven-dry and green wood below stump height,
#'   respectively.}
#' \item{`dry_stump_bark`, `green_stump_bark`}{Oven-dry and green bark below stump height,
#'   respectively.}
#' \item{`dry_saw_wood`, `green_saw_wood`}{Oven-dry and green wood assigned to the primary
#'   saw section, respectively.}
#' \item{`dry_saw_bark`, `green_saw_bark`}{Oven-dry and green bark assigned to the primary
#'   saw section, respectively.}
#' \item{`dry_topwood_wood`, `green_topwood_wood`}{Oven-dry and green wood in the secondary
#'   merchantable section, respectively.}
#' \item{`dry_topwood_bark`, `green_topwood_bark`}{Oven-dry and green bark in the secondary
#'   merchantable section, respectively.}
#' \item{`dry_tip_wood`, `green_tip_wood`}{Oven-dry and green wood above the secondary
#'   merchantable section, respectively.}
#' \item{`dry_tip_bark`, `green_tip_bark`}{Oven-dry and green bark above the secondary
#'   merchantable section, respectively.}
#' \item{`dry_branches`, `green_branches`}{Oven-dry and green branch biomass, respectively.}
#' \item{`dry_foliage`, `green_foliage`}{Oven-dry and green foliage biomass, respectively.}
#' \item{`dry_top_and_limb`, `green_top_and_limb`}{Oven-dry and green aboveground biomass
#'   excluding foliage, stump, and primary and secondary merchantable sections,
#'   respectively.}
#' \item{`carbon`}{Carbon mass in aboveground biomass excluding foliage.}
#' \item{`co2e`}{Carbon multiplied by 44/12, in the same pounds or kilograms.
#' The separate [co2e()] function instead always returns metric tons.}
#' \item{`id`}{Optional input identifier, present only when supplied. Unitless.}
#' }
#' With `status = TRUE`, every quantity has an integer, unitless companion
#' named by appending `_status`, for example `dry_stem_wood_status`. The same
#' row code is repeated for every component.
#'
#' There are no list columns.
#' @param status One nonmissing logical flag, default `FALSE` for a numeric
#'   vector with row warnings. `TRUE` returns a data frame with numeric
#'   `value` and integer unitless `status`, and suppresses row warnings.
#'   Invalid flags stop the call. Example: `status = TRUE`.
biomass_component <- function(dbh, ht, spcd, division, component,
                              system = "nsvb", ..., units = "imperial",
                              status = FALSE) {
  component <- .scalar_character(component, "component", .biomass_names)
  status_requested <- .validate_status(status)
  result <- .biomass_impl(
    dbh, ht, spcd, division, system, NULL, list(...), units, status_requested,
    "biomass_component"
  )
  if (status_requested) {
    return(data.frame(
      value = result[[component]],
      status = result[[paste0(component, "_status")]]
    ))
  }
  result[[component]]
}

#' Express tree carbon as metric tons of carbon dioxide
#'
#' Return aboveground carbon excluding foliage as metric tons of carbon dioxide equivalent.
#'
#' @inheritParams biomass
#' @return Numeric metric tons of carbon dioxide equivalent per tree regardless of `units`,
#'   or a data frame with numeric `value` in metric tons and integer unitless `status`.
#' @details The calculation uses carbon excluding foliage. The conversion returns carbon dioxide
#' equivalent mass.
#' @section Tree condition and measurement limits through dots:
#' All inputs are numeric, length one or one per tree. Defaults apply when
#' omitted, not when explicitly missing, except the length overrides below.
#' \describe{
#' \item{`decay_class`}{Whole number from zero through 5, unitless. Default
#' `0` describes a live tree. Classes 1 through 5 apply the dead-tree density,
#' bark, branch, foliage, and carbon rules.
#'
#' Missing gives `na_input`. }
#' \item{`cull`}{Percentage from zero through 100. Default `0` means no cull
#' adjustment. Missing gives `na_input`.  This is the biomass
#' system's adjustment and is not a located merchandising defect.}
#' \item{`region`, `forest`}{Whole-number geographic codes from zero through
#' 99. Defaults `0` select species-reference green-weight factors instead of
#' a forest-specific factor. Missing gives `na_input`.
#'
#' Unitless. }
#' \item{`primary_top`, `secondary_top`}{Positive inside-bark product top
#' diameters. Defaults are 6 and 4 inches or 15.24 and 10.16 centimeters.
#' Missing gives `na_input`. }
#' \item{`stump_ht`}{Nonnegative stump height above ground. Default is 1 foot
#' or 0.3048 meter. Missing gives `na_input`. }
#' \item{`max_log_length`, `min_log_length`, `trim`}{Positive length overrides
#' in feet or meters. Default `NA_real_` leaves the source rule unchanged. Nonfinite values also
#' leave these overrides unset.
#'
#' Finite zero or negative
#' values are errors, including zero trim. }
#' }
#' @section Status and missing values:
#' `ok` means available model output. Missing inputs, nonpositive dimensions, unrecognized species,
#' and equation failures leave every component missing. [status_codes()] identifies these
#' diagnoses.
#'
#' A component can be zero for dead foliage or an absent merchantable section. Component statuses
#' do not describe merchandising results.
#' @seealso [biomass()] for all components, [green_weight()] for weight from
#'   cubic volume, [carbon_fraction()] for the live-tree carbon fraction.
#' @export
#' @usage
#'
#' ## Call signatures
#' co2e(dbh, ht, spcd, division, system = 'nsvb', ..., units = 'imperial', status = FALSE)
#' @examples
#' ## Estimate carbon dioxide equivalent from inventory measurements
#' co2e(dbh = example_trees$dbh,
#'      ht = example_trees$ht,
#'      spcd = example_trees$spcd,
#'      division = 1240)
#' @param status One nonmissing logical flag, default `FALSE` for a numeric
#'   vector with row warnings. `TRUE` returns a data frame with numeric
#'   `value` and integer unitless `status`, and suppresses row warnings.
#'   Invalid flags stop the call. Example: `status = TRUE`.
co2e <- function(dbh, ht, spcd, division, system = "nsvb", ...,
                 units = "imperial", status = FALSE) {
  status_requested <- .validate_status(status)
  result <- biomass_component(
    dbh, ht, spcd, division, "carbon", system = system, ...,
    units = units, status = status_requested
  )
  factor <- if (identical(.validate_units(units), "imperial")) {
    0.45359237 / 1000
  } else {
    1 / 1000
  }
  if (status_requested) {
    result$value <- result$value * factor * 44 / 12
    return(result)
  }
  result * factor * 44 / 12
}

#' Look up the carbon fraction of dry tree biomass
#'
#' Return the live-tree carbon fraction for each species, with a source attribute.
#'
#' @details
#' Uses the Table S10 lookup and species-999 fallback in `NVB_CarbonFrac`, then
#' applies the three-decimal rounding used when National Scale Volume and Biomass equations
#'   calculates biomass carbon.
#' The returned vector records the pinned National Volume Estimator Library source in its
#'   `source` attribute.
#' With `options(merchandiser.compat = 'nvel')`, the function returns the raw
#' Table S10 fraction instead.
#'
#' @param spcd Required numeric vector of inventory species codes, any length. Positive whole
#'   numbers in the reference are recognized. Missing or
#'   nonfinite inputs return missing fractions silently.
#'
#' Invalid or unknown
#'   finite codes return missing fractions with a warning. Unitless. Example: `example_trees$spcd`.
#'
#' Omission or nonnumeric values are errors.
#'
#' @return A numeric vector with a `source` attribute. Unknown Forest Inventory and Analysis
#'   tree inventory codes are
#'   `NA` with one warning.
#' @export
#' @usage
#'
#' ## Call signatures
#' carbon_fraction(spcd)
#' @examples
#' ## Look up live-tree carbon fractions for the shipped species
#' carbon_fraction(spcd = example_trees$spcd)
#' @section Status and missing values:
#' No status column is returned. Missing species give missing fractions. Unknown finite codes
#' warn and return missing values.
#'
#' The fraction is a live-tree reference, not the decay-specific carbon fraction used for dead
#' trees by [biomass()]. Do not replace missing fractions with zero.
#' @seealso [biomass()] to
#' calculate carbon with tree condition, [co2e()] to convert estimated carbon to carbon dioxide
#' equivalent.
carbon_fraction <- function(spcd) {
  if (!is.numeric(spcd)) {
    stop("spcd must be numeric.", call. = FALSE)
  }
  missing <- !is.finite(spcd)
  invalid <- !missing & (
    spcd <= 0 | spcd > .Machine$integer.max | spcd != floor(spcd)
  )
  remapped <- .nsvb_remap_spcd(spcd)
  recognized <- !missing & !invalid & remapped %in% species_reference$spcd
  output <- rep(NA_real_, length(spcd))
  output[recognized] <- tv_cpp_nsvb_carbon_fraction_impl(
    as.integer(remapped[recognized]),
    identical(.treevolume_compat(), "nvel")
  )
  if (any(invalid | (!missing & !recognized))) {
    warning(
      "carbon_fraction(): unknown_species for ",
      sum(invalid | (!missing & !recognized)), " of ", length(spcd),
      " trees (NA returned)", call. = FALSE
    )
  }
  fraction_note <- if (identical(.treevolume_compat(), "nvel")) {
    "raw Table S10 fraction"
  } else {
    "three-decimal NVBC carbon rounding"
  }
  attr(output, "source") <- paste(
    "NVEL tables10.inc and nsvb.f NVB_CarbonFrac, commit",
    "38548071d5aa652bb90c7f111f86b427f798a1c9,",
    fraction_note
  )
  output
}

.nsvb_ecoprov_data <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      path <- system.file(
        "extdata", "nsvb_divisions.csv", package = "merchandiser", mustWork = TRUE
      )
      value <<- utils::read.csv(path, stringsAsFactors = FALSE)
    }
    value
  }
})

.nsvb_county_division_data <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      path <- system.file(
        "extdata", "nsvb_county_divisions.csv",
        package = "merchandiser", mustWork = TRUE
      )
      value <<- utils::read.csv(path, stringsAsFactors = FALSE)
    }
    value
  }
})

#' Look up the county ecological code for biomass equations
#'
#' Return the county-predominant ecological code used by the national biomass equations.
#'
#' @details
#' Uses the predominant county ecological subregion in Table 1 of United States Forest
#' Service General Technical Report SRS-36. The returned value retains the
#' mountain prefix as 1,000, so ecological province M261 is returned as 1261.
#' Equation selection rounds this county-predominant province down to the broader division
#'   used by the equations.
#'
#' @param state Required numeric whole-number state code from 1 through 99,
#'   length one or one per county. The Federal Information Processing
#'   Standards geographic codes identify states and counties. Missing or
#'   nonfinite values return `NA`.
#'
#' Invalid finite codes are errors. Unitless.
#' @param county Required numeric whole-number county code from 1 through 999,
#'   length one or one per state. Missing or nonfinite values return `NA`. Invalid finite codes are
#' errors.
#'
#' Unitless. Omission is an error for either geographic argument.
#'
#' @return An integer vector. A state and county pair absent from the conterminous United
#'   States table
#'   is `NA`.
#' @references Rudis, V.A. (1999). *Ecological subregion codes by county,
#'   coterminous United States*. General Technical Report SRS-36.
#'   \doi{10.2737/SRS-GTR-36}.
#' @export
#' @usage
#'
#' ## Call signatures
#' nsvb_division(state, county)
#' @examples
#' ## Look up a county-predominant ecological code
#' nsvb_division(state = 6, county = 93)
#' @section Status and missing values:
#' No code column is returned. Missing or unmatched geographic pairs return missing ecological
#' codes without a warning. Check the county or use a coordinate lookup.
#'
#' County predominance does not identify every site's province. The broader division used by
#' the equations is obtained by rounding the province code down to the next multiple of ten.
#' @seealso [nsvb_division_xy()] for coordinate assignment, [biomass()] to use an ecological
#' code in a biomass calculation.
nsvb_division <- function(state, county) {
  prepared <- .prepare_vectors(
    list(state = state, county = county),
    numeric_names = c("state", "county"),
    character_names = character(), aux = list()
  )
  values <- prepared$values
  .validate_integer_values(values$state, "state", 1, 99)
  .validate_integer_values(values$county, "county", 1, 999)
  output <- rep(NA_integer_, prepared$size)
  valid <- is.finite(values$state) & is.finite(values$county)
  if (!any(valid)) {
    return(output)
  }
  table <- .nsvb_county_division_data()
  key <- values$state * 1000 + values$county
  table_key <- table$state * 1000 + table$county
  output[valid] <- table$division[match(key[valid], table_key)]
  as.integer(output)
}

.nsvb_ecoprov <- function(region, forest, district) {
  prepared <- .prepare_vectors(
    list(region = region, forest = forest, district = district),
    numeric_names = c("region", "forest", "district"),
    character_names = character(), aux = list()
  )
  values <- prepared$values
  for (name in names(values)) {
    .validate_integer_values(values[[name]], name, 0, 99)
  }
  output <- rep(NA_integer_, prepared$size)
  valid <- is.finite(values$region) & is.finite(values$forest) &
    is.finite(values$district)
  if (!any(valid)) {
    return(output)
  }
  table <- .nsvb_ecoprov_data()
  district_table <- table[table$level == "district", ]
  forest_table <- table[table$level == "forest", ]
  region_table <- table[table$level == "region", ]
  district_key <- values$region * 10000 + values$forest * 100 + values$district
  forest_key <- values$region * 100 + values$forest
  output[valid] <- district_table$division[match(
    district_key[valid], district_table$key
  )]
  needs_forest <- valid & is.na(output)
  output[needs_forest] <- forest_table$division[match(
    forest_key[needs_forest], forest_table$key
  )]
  needs_region <- valid & is.na(output)
  output[needs_region] <- region_table$division[match(
    values$region[needs_region], region_table$key
  )]
  as.integer(output)
}

#' Interpret diameter and volume from the national biomass equations
#'
#' The national biomass system supplies separate fitted diameter and cumulative-volume
#' relationships.
#'
#' @details
#' The National Scale Volume and Biomass system derives diameter from the derivative of its
#' equation-6 Kozak cumulative-volume ratio. `dib()` follows `NVB_DibAtHT`, and
#' `height_at_dib()` follows `NVB_HT2TOPDib`. `stem_volume()` follows the Table S5 cumulative
#' inside-bark ratio used for national biomass equations component volumes.
#'
#' The diameter path uses source Tables S3 and S4. Cumulative volume uses Tables S1 and S5.
#' Accordingly, `stem_volume()` does not numerically integrate the displayed inside-bark
#' diameter curve.
#'
#' The default package mode returns the outside-bark diameter computed internally by the national
#' biomass equations. With `options(merchandiser.compat = 'nvel')`, [dob()] reproduces
#' CALCDIA's unassigned zero output.
#'
#' @name nsvb_profiles
#' @keywords internal
#' @section Status and missing values:
#' Unknown equations, missing required measurements, and unavailable bark calculations return
#' missing quantities. [status_codes()] lists the diagnoses.
#'
#' `not_unique` retains the highest detected diameter crossing. Review the profile when this
#' occurs.
#' @seealso [get_taper_model()]
#' to inspect declared inputs, [dib()] for diameter, [stem_volume()] for volume,
#' [stem_profile()] for a table.
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Estimate national biomass for the example species
#' example_trees %>%
#'   slice_head(n = 3) %>%
#'   transmute(tree,
#'             mass = biomass_component(dbh = dbh,
#'                                      ht = ht,
#'                                      spcd = spcd,
#'                                      division = 1240,
#'                                      component = 'dry_stem_wood'))
NULL
