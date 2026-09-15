.nsvb_source <- paste(
  "US Forest Service National Volume Estimator Library nsvb.f and tables1.inc",
  "through tables11.inc, commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.nsvb_model <- function(id, species) {
  new_stem_model_unchecked(
    id = id,
    form = "nsvb",
    kernel = list(
      type = "compiled", key = paste0(
        "nsvb:",
        id
      ), has_dob = TRUE, has_inverse = TRUE, has_integral = TRUE
    ),
    inputs = list(
      required = character(),
      optional = character(), pairs = list()
    ),
    measurement_system = "imperial",
    spcd = as.integer(species),
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
  path <- system.file("extdata", "nsvb_models.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE)
  Map(.nsvb_model, metadata$id, metadata$species)
}

.nsvb_patterns <- function() {
  data.frame(id = c("NVB0??????", "NVBM??????"), family = "nsvb", stringsAsFactors = FALSE)
}

.nsvb_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl(
    "^NVB[0M][0-9]{6}P?$", id
  )) {
    return(NULL)
  }
  .nsvb_model(id, as.integer(substr(id, 8L, 10L)))
}

.biomass_names <- c(paste0("dry_", c(
  "aboveground_no_foliage", "stem_wood", "stem_bark",
  "stump_wood",
  "stump_bark", "saw_wood", "saw_bark", "topwood_wood", "topwood_bark", "tip_wood",
  "tip_bark",
  "branches", "foliage", "top_and_limb"
)), "carbon", "co2e", paste0("green_", c(
  "agb_no_foliage",
  "stem_wood", "stem_bark", "stump_wood", "stump_bark", "saw_wood", "saw_bark",
  "topwood_wood",
  "topwood_bark", "tip_wood", "tip_bark", "branches", "foliage", "top_and_limb"
)))

.nsvb_dot_defaults <- list(
  region = 0, forest = 0, decay_class = 0, cull = 0,
  primary_top = 6,
  secondary_top = 4, stump_ht = 1, max_log_length = NA_real_,
  min_log_length = NA_real_, trim = NA_real_
)

.validate_integer_values <- function(value, name, minimum, maximum) {
  missing <- !is.finite(value)
  invalid <- !missing & (value != floor(value) | value < minimum | value > maximum)
  if (any(invalid)) {
    stop(name, " has ", sum(invalid), " out-of-domain ", if (sum(invalid) == 1L)
           "value." else "values.", call. = FALSE)
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

.prepare_biomass_call <- function(dbh, ht, spcd, division, id, dots, measurement_system) {
  dots <- .capture_aux(dots)
  unknown <- setdiff(names(dots), names(.nsvb_dot_defaults))
  if (length(unknown)) {
    stop("undeclared auxiliary input: ", unknown[[1L]], call. = FALSE)
  }
  defaults <- .nsvb_dot_defaults
  if (identical(measurement_system, "metric")) {
    defaults$primary_top <- defaults$primary_top * 2.54
    defaults$secondary_top <- defaults$secondary_top * 2.54
    defaults$stump_ht <- defaults$stump_ht * 0.3048
  }
  supplied <- utils::modifyList(defaults, dots)
  values <- c(
    list(dbh = dbh, ht = ht, spcd = spcd, division = division), supplied,
    if (!is.null(id)) list(id = id)
  )
  prepared <- .prepare_vectors(values,
    numeric_names = setdiff(names(values), "id"),
    character_names = character(),
    aux = list()
  )
  values <- prepared$values
  result_status <- .input_status(prepared$size, values, c("dbh", "ht", "spcd", "division"))
  result_status <- .assign_status(result_status, values$dbh <= 0 | values$dbh > 400, 2L)
  result_status <- .assign_status(result_status, values$ht <= 0 | values$ht > 500, 3L)

  .validate_integer_values(values$spcd, "spcd", 1, .Machine$integer.max)
  divisions <- unique(c(0, .nvel_reference("nsvb_divisions.csv")$division,
                        nsvb_division_polygons$division))
  result_status <- .assign_status(result_status, !values$division %in% divisions, 8L)
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
    result_status <- .assign_status(result_status, !is.finite(values[[name]]), 1L,
      eligible = result_status ==
        0L & !name %in% c("max_log_length", "min_log_length", "trim")
    )
  }
  recognized <- .nsvb_remap_spcd(values$spcd) %in% species_reference$spcd
  result_status <- .assign_status(result_status, !recognized, 7L,
    eligible = result_status ==
      0L
  )
  list(size = prepared$size, values = values, status = result_status)
}

.biomass_impl <- function(
  dbh, ht, spcd, division, system, id, dots, measurement_system, status,
  function_name = "biomass"
) {
  system <- .scalar_character(system, "system", "nsvb")
  measurement_system <- .validate_units(measurement_system)
  status_requested <- .validate_status(status)
  call <- .prepare_biomass_call(dbh, ht, spcd, division, id, dots, measurement_system)
  output <- matrix(NA_real_, nrow = call$size, ncol = length(.biomass_names))
  result_status <- call$status
  rows <- which(result_status == 0L)
  if (length(rows)) {
    values <- call$values
    native_dbh <- .diameter_to_native(values$dbh[rows], measurement_system, "imperial")
    native_ht <- .height_to_native(values$ht[rows], measurement_system, "imperial")
    native_primary_top <-
      .diameter_to_native(values$primary_top[rows], measurement_system, "imperial")
    native_secondary_top <- .diameter_to_native(
      values$secondary_top[rows], measurement_system,
      "imperial"
    )
    native_stump <- .height_to_native(values$stump_ht[rows], measurement_system, "imperial")
    native_max <- .height_to_native(values$max_log_length[rows], measurement_system, "imperial")
    native_min <- .height_to_native(values$min_log_length[rows], measurement_system, "imperial")
    native_trim <- .height_to_native(values$trim[rows], measurement_system, "imperial")
    evaluated <- tv_cpp_nsvb_biomass_impl(
      native_dbh, native_ht, as.integer(
        values$spcd[rows]
      ),
      as.integer(values$division[rows]), as.integer(values$region[rows]),
      as.integer(values$forest[rows]),
      as.integer(values$decay_class[rows]), values$cull[rows], native_primary_top,
      native_secondary_top,
      native_stump, native_max, native_min, rep(NA_real_, length(rows)), rep(
        NA_real_,
        length(rows)
      ), native_trim, integer(length(rows)), integer(length(rows)),
      rep.int(
        -1L,
        length(rows)
      ), rep.int(utf8ToInt("C"), length(rows)), threads()
    )
    output[rows, ] <- evaluated$value
    returned <- as.integer(evaluated$status)
    result_status[rows[returned != 0L]] <- returned[returned != 0L]
  }
  output <- .weight_from_native(output, measurement_system, "imperial")
  output[!result_status %in% c(0L, 52L, 102L), ] <- NA_real_
  colnames(output) <- .biomass_names
  result <- as.data.frame(output, stringsAsFactors = FALSE)
  if (!is.null(id)) {
    result <- cbind(id = call$values$id, result, stringsAsFactors = FALSE)
  }
  if (status_requested) {
    status_frame <- as.data.frame(matrix(
      rep(as.integer(result_status), length(
        .biomass_names
      )),
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
  "vol_total_cu", "vol_bf_gross", "vol_bf_net",
  "vol_cu_gross", "vol_cu_net",
  "cords", "vol_top_cu_gross", "vol_top_cu_net", "cords_top", paste0("vol", 10:13),
  "vol_stump_cu",
  "vol_tip_cu"
)

#' Record source-library log-length and measurement rules
#'
#' @param even_or_odd Choose the source library's log-length rounding convention. Numeric whole-
#' number code, unitless, default NA selects the library default. Code 1 allows odd lengths and
#' code 2 uses even lengths.
#' @param option Choose how the source library divides the merchantable stem. Numeric whole-
#' number code, unitless, default NA selects the library default. Codes 11 through 14 use its 16,
#' 20, 32, and 40 foot conventions. Codes 21 through 24 select its top-segment rules.
#' @param maximum_length Set the longest nominal segment allowed by the source rules.
#' Numeric scalar,
#' feet, default NA selects the library default.
#' @param minimum_length Set the shortest nominal segment allowed by the source rules.
#' Numeric scalar,
#' feet, default NA selects the library default.
#' @param minimum_top_length Set the shortest top segment allowed by the source rules.
#' Numeric scalar,
#' feet, default NA selects the library default.
#' @param merchantable_length Set the primary-product stem length needed for the tree to qualify.
#' Numeric, feet, default NA selects the library default.
#' @param primary_top Set the smallest inside-bark diameter for the primary product. Numeric scalar,
#' inches, default NA selects the library default.
#' @param secondary_top Set the smallest inside-bark diameter for the secondary product.
#' Numeric scalar,
#' inches, default NA selects the library default.
#' @param stump Set the height left below the first source-library segment. Numeric scalar, feet,
#' default NA selects the library default.
#' @param trim Set the extra wood cut above each nominal source-library segment.
#' Numeric scalar, feet,
#' default NA selects the library default.
#' @param bark_ratio The bark ratio estimates inside diameter from outside diameter when needed.
#'   Numeric scalar, inside diameter divided by outside diameter. Default: \code{NA_real_}.
#' @param minimum_board_foot_dbh Set the smallest tree eligible for board foot volume.
#' Numeric scalar
#' outside-bark diameter at breast height, inches, default NA selects the library default.
#' @param scribner Choose how the source library scales Scribner volume. Character scalar,
#'   unitless, default `'regional'` uses its regional setting. `'table'` uses the Scribner table
#'   and `'factor'` uses regional factors. Missing values and unknown labels are errors.
#' @param prod Choose the source product class. Character scalar of two-character codes,
#'   unitless, default `'01'` means sawtimber. `'08'` means a nonsaw product.
#' @param ht_type Tell the source library how height is recorded. Character scalar, unitless,
#'   default `''` leaves the setting unspecified. `'F'` means feet and `'L'` means logs.
#' @param live Tell the source library whether the tree is alive. Character scalar, unitless,
#'   default `'L'` means live. `'D'` means dead.
#' @param ctype Identify the kind of inventory supplying the tree. Character scalar, unitless,
#'   default `'C'` means cruise. `'I'` selects Forest Inventory and Analysis, `'F'` selects
#'   Forest Vegetation Simulator, and `'B'` selects the alternate National Scale Volume and
#'   Biomass route. Missing values and unknown labels are errors.
#' @param cull Supply the whole-tree cull percentage required by a source equation. Numeric scalar,
#' percent, default 0. This source input does not reduce merchandise log scales.
#' @param forest The national forest code selects local source rules. Numeric scalar, whole-number
#'   forest codes. Default: \code{0}.
#' @param district The ranger district code selects local source rules. Numeric scalar,
#'   whole-number district codes. Default: \code{0}.
#' @return A validated list of scalar fields with class `treevolume_nvel_rules`.
#' @usage
#' nvel_rules(
#'   even_or_odd = NA_integer_,
#'   option = NA_integer_,
#'   maximum_length = NA_real_,
#'   minimum_length = NA_real_,
#'   minimum_top_length = NA_real_,
#'   merchantable_length = NA_real_,
#'   primary_top = NA_real_,
#'   secondary_top = NA_real_,
#'   stump = NA_real_,
#'   trim = NA_real_,
#'   bark_ratio = NA_real_,
#'   minimum_board_foot_dbh = NA_real_,
#'   scribner = 'regional',
#'   prod = '01',
#'   ht_type = '',
#'   live = 'L',
#'   ctype = 'C',
#'   cull = 0,
#'   forest = 0,
#'   district = 0
#' )
#' @export
#' @examples
#' ## Inspect an explicit whole foot source rule record.
#' nvel_rules(even_or_odd = 1,
#'            maximum_length = 32,
#'            minimum_length = 16,
#'            primary_top = 6)
nvel_rules <- function(
  even_or_odd = NA_integer_, option = NA_integer_,
  maximum_length = NA_real_,
  minimum_length = NA_real_, minimum_top_length = NA_real_, merchantable_length = NA_real_,
  primary_top = NA_real_, secondary_top = NA_real_, stump = NA_real_,
  trim = NA_real_, bark_ratio = NA_real_,
  minimum_board_foot_dbh = NA_real_, scribner = "regional", prod = "01",
  ht_type = "", live = "L",
  ctype = "C", cull = 0, forest = 0, district = 0
) {
  values <- list(
    even_or_odd = even_or_odd, option = option,
    maximum_length = maximum_length,
    minimum_length = minimum_length, minimum_top_length = minimum_top_length,
    merchantable_length = merchantable_length,
    primary_top = primary_top, secondary_top = secondary_top, stump = stump, trim = trim,
    bark_ratio = bark_ratio, minimum_board_foot_dbh = minimum_board_foot_dbh,
    scribner = scribner,
    prod = prod, ht_type = ht_type, live = live, ctype = ctype, cull = cull,
    forest = forest,
    district = district
  )
  for (name in names(values)) {
    if (length(values[[name]]) != 1L)
      stop(name, " needs one value.", call. = FALSE)
  }
  character_names <- c("scribner", "prod", "ht_type", "live", "ctype")
  for (name in character_names) {
    if (!is.character(values[[name]]) || !length(values[[name]]) || anyNA(values[[name]])) {
      stop(name, " must be a character vector without missing values.", call. = FALSE)
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
    "merchantable_length",
    "primary_top", "secondary_top", "trim", "bark_ratio", "minimum_board_foot_dbh"
  )
  for (name in positive) {
    if (any(!is.na(values[[name]]) & values[[name]] <= 0)) {
      stop(name, " must be positive or NA.", call. = FALSE)
    }
  }
  if (!is.na(minimum_length) && !is.na(maximum_length) && minimum_length > maximum_length) {
    stop("minimum_length must be at most maximum_length.", call. = FALSE)
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

.nvel_rule_defaults <- function(values, model, dbh, spcd, region, measurement_system) {
  defaults <- .nvel_reference("nvel_merchant_defaults.csv")
  length_names <- c(
    "maximum_length", "minimum_length", "minimum_top_length",
    "merchantable_length",
    "stump", "trim"
  )
  diameter_names <- c("primary_top", "secondary_top", "minimum_board_foot_dbh")
  in_caller_units <- function(value, name) {
    if (is.na(value) || measurement_system == "imperial")
      return(value)
    if (name %in% length_names)
      return(value * 0.3048)
    if (name %in% diameter_names)
      return(value * 2.54)
    value
  }
  for (row in seq_along(model)) {
    was_missing <- vapply(values, function(value) is.na(value[[row]]), logical(1))
    selected_region <- region[[row]]
    source_row <- match(selected_region, defaults$region)
    if (is.na(source_row))
      source_row <- match(4L, defaults$region)
    for (name in intersect(names(values), names(defaults))) {
      if (is.numeric(values[[name]]) && is.na(values[[name]][[row]])) {
        values[[name]][[row]] <- in_caller_units(defaults[[name]][[source_row]], name)
      } else if (is.character(values[[name]]) && is.na(values[[name]][[row]])) {
        values[[name]][[row]] <- defaults[[name]][[source_row]]
      }
    }
    product <- values$prod[[row]]
    if (!is.na(selected_region) && selected_region == 1L) {
      profile_rules <- toupper(substr(model[[row]], 4L, 6L)) %in% c("FW2", "FW3") ||
        toupper(substr(
          model[[row]],
          1L, 3L
        )) == "NVB"
      if (!profile_rules) {
        region1_other <- c(
          option = 12, maximum_length = 20, minimum_length = 10,
          minimum_top_length = 2,
          merchantable_length = 10
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
        c(
          minimum_length = 10, minimum_top_length = 10, merchantable_length = 10,
          primary_top = 6,
          secondary_top = 4, stump = 1
        )
      } else if (product == "08") {
        c(
          minimum_length = 10, minimum_top_length = 10, merchantable_length = 10,
          primary_top = 4,
          secondary_top = 4, stump = 0.5
        )
      } else if (product == "14") {
        c(
          minimum_length = 10, minimum_top_length = 10, merchantable_length = 10,
          primary_top = 4,
          secondary_top = 1, stump = 0.5
        )
      } else if (product == "20") {
        c(
          minimum_length = 2, minimum_top_length = 2, merchantable_length = 8,
          primary_top = 1,
          secondary_top = 1, stump = 0.5
        )
      } else if (product == "07") {
        c(
          minimum_length = 4, minimum_top_length = 4, merchantable_length = 8,
          primary_top = 2,
          secondary_top = 2, stump = 0.5
        )
      } else {
        c(
          minimum_length = 10, minimum_top_length = 10, merchantable_length = 10,
          primary_top = 4,
          secondary_top = 4, stump = 0.5
        )
      }
      for (name in names(region3)[was_missing[names(region3)]]) {
        values[[name]][[row]] <- in_caller_units(region3[[name]], name)
      }
    }
    if (!is.na(selected_region) && selected_region == 7L && was_missing[["primary_top"]]) {
      dbh_inches <- .diameter_to_native(dbh[[row]], measurement_system, "imperial")
      top_inches <- floor(0.184 * dbh_inches + 2.24 + 0.5)
      values$primary_top[[row]] <-
        .diameter_from_native(top_inches, measurement_system, "imperial")
    }
    if (!is.na(selected_region) && selected_region == 8L) {
      if (was_missing[["primary_top"]]) {
        top_inches <- if (product == "08")
          0.1 else if (!is.na(spcd[[row]]) && spcd[[row]] < 300L)
          7 else 9
        values$primary_top[[row]] <-
          .diameter_from_native(top_inches, measurement_system, "imperial")
      }
      if (was_missing[["stump"]]) {
        values$stump[[row]] <- .height_from_native(if (product == "01")
                                                     1 else 0.5, measurement_system, "imperial")
      }
      if (product == "08") {
        values$merchantable_length[[row]] <-
          .height_from_native(12, measurement_system, "imperial")
      }
    }
    if (!is.na(selected_region) && selected_region == 9L) {
      if (was_missing[["primary_top"]]) {
        top_inches <- if (!is.na(spcd[[row]]) && spcd[[row]] < 300L) {
          7.6
        } else {
          9.6
        }
        values$primary_top[[row]] <-
          .diameter_from_native(top_inches, measurement_system, "imperial")
      }
      if (was_missing[["stump"]]) {
        values$stump[[row]] <- .height_from_native(if (product == "01")
                                                     1 else 0.5, measurement_system, "imperial")
      }
    }
    if (!is.na(values$primary_top[[row]]) && !is.na(values$secondary_top[[row]]) &&
          values$secondary_top[[row]] >
            values$primary_top[[row]]) {
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
  result[substr(model, 4L, 4L) == "M"] <- result[substr(model, 4L, 4L) == "M"] + 1000L
  result
}

.nvel_status_frame <- function(status_matrix) {
  result <- as.data.frame(status_matrix)
  names(result) <- paste0(c(
    .nvel_volume_names, "n_logs_primary",
    "n_logs_secondary"
  ), "_status")
  result[] <- lapply(result, as.integer)
  result
}

# Reproduce the R9LOGS log-count guard without exposing incomplete log arrays.
.nvel_r9_log_overflow <- function(values, resolved, aux, rows, measurement_system) {
  source_height <- function(target) {
    result <- rep(NA_real_, length(rows))
    by_model <- split(seq_along(rows), values$model[rows])
    native_dbh <- .diameter_to_native(values$dbh[rows], measurement_system, "imperial")
    native_ht <- .height_to_native(values$ht[rows], measurement_system, "imperial")
    native_target <- .diameter_to_native(target, measurement_system, "imperial")
    for (selected in by_model) {
      row <- rows[selected[[1L]]]
      dictionary_index <- match(values$model[[row]], resolved$dictionary)
      model <- resolved$models[[dictionary_index]]
      evaluated <- .compiled_result(
        model, 11L, native_dbh[selected], native_ht[selected],
        native_target[selected], numeric(length(selected)), rep(1, length(selected)),
        aux, rows[selected]
      )
      result[selected] <- evaluated$value
    }
    result
  }
  saw_height <- source_height(values$primary_top[rows])
  pulp_height <- source_height(values$secondary_top[rows])
  stump <- .height_to_native(values$stump[rows], measurement_system, "imperial")
  minimum <- .height_to_native(values$minimum_length[rows], measurement_system, "imperial")
  maximum <- .height_to_native(values$maximum_length[rows], measurement_system, "imperial")
  trim <- .height_to_native(values$trim[rows], measurement_system, "imperial")

  vapply(seq_along(rows), function(index) {
    saw <- saw_height[[index]]
    pulp <- pulp_height[[index]]
    if (!all(is.finite(c(
      saw, pulp, stump[[index]], minimum[[index]],
      maximum[[index]], trim[[index]]
    )))) {
      return(FALSE)
    }
    base <- stump[[index]]
    min_length <- minimum[[index]]
    max_length <- maximum[[index]]
    log_trim <- trim[[index]]
    merch <- saw - base
    primary <- as.integer(merch / (max_length + log_trim))
    if (primary > 20L)
      return(TRUE)
    leftover <- merch - (max_length + log_trim) * primary - log_trim
    lengths <- numeric(20L)
    if (!(merch < min_length + log_trim || (primary == 0L && leftover < min_length +
                                              log_trim))) {
      last <- primary
      if (last > 0L)
        lengths[seq_len(last)] <- max_length
      if (leftover >= min_length + log_trim) {
        primary <- primary + 1L
        last <- last + 1L
        if (last > 20L)
          return(TRUE)
        lengths[[last]] <- leftover
      }
      if (primary == 1L) {
        lengths[[1L]] <- as.integer(lengths[[1L]] / 2) * 2
      } else if (leftover < min_length) {
        lengths[[last]] <- as.integer(lengths[[last]] / 2) * 2
      } else {
        lengths[[last]] <- as.integer(as.integer((max_length + leftover) / 2) / 2) * 2
        lengths[[last - 1L]] <- as.integer(as.integer(max_length + leftover -
                                                        lengths[[last]]) / 2) *
          2
      }
    }
    if (!(pulp > 0 && pulp > saw))
      return(FALSE)
    saw_end <- base
    if (primary > 0L) {
      saw_end <- saw_end + sum(lengths[seq_len(primary)] + log_trim)
    }
    merch <- pulp - saw_end
    secondary <- as.integer(merch / (max_length + log_trim))
    leftover <- merch - (max_length + log_trim) * secondary - log_trim
    if (merch < min_length + log_trim || (secondary == 0L && leftover < min_length +
                                            log_trim)) {
      secondary <- 0L
    }
    if (secondary > 0L || leftover > min_length + log_trim) {
      last <- primary + secondary
      if (last > 20L)
        return(TRUE)
      if (leftover >= min_length + log_trim && last + 1L > 20L) {
        return(TRUE)
      }
    }
    FALSE
  }, logical(1))
}

nvel_volume <- function(
  dbh, ht, model, rules = nvel_rules(), id = NULL, ...,
  measurement_system = "imperial",
  status = FALSE
) {
  if (!inherits(rules, "treevolume_nvel_rules")) {
    stop("rules must be created by nvel_rules().", call. = FALSE)
  }
  measurement_system <- .validate_units(measurement_system)
  status_requested <- .validate_status(status)
  dots <- .capture_aux(list(...))
  nvel_names <- intersect(names(dots), c("spcd", "division", "region"))
  nvel_values <- dots[nvel_names]
  aux <- dots[setdiff(names(dots), nvel_names)]
  if (is.null(nvel_values$spcd))
    nvel_values$spcd <- .nvel_model_species(model)
  if (is.null(nvel_values$division)) {
    nvel_values$division <- .nvel_model_division(model)
  }
  if (is.null(nvel_values$region))
    nvel_values$region <- .nvel_model_region(model)
  if (!is.null(rules$bark_ratio) && is.null(aux$bark_ratio)) {
    aux$bark_ratio <- rules$bark_ratio
  }
  numeric_rule_names <- names(rules)[vapply(rules, is.numeric, logical(1))]
  character_rule_names <- setdiff(names(rules), numeric_rule_names)
  numeric_values <- c(
    list(dbh = dbh, ht = ht), rules[numeric_rule_names],
    nvel_values[c(
      "spcd",
      "division", "region"
    )]
  )
  character_values <- c(list(model = model), rules[character_rule_names])
  if (!is.null(id))
    character_values$id <- id
  prepared <- .prepare_vectors(c(numeric_values, character_values),
    numeric_names = names(numeric_values),
    character_names = names(character_values), aux = aux
  )
  values <- prepared$values
  values[names(rules)] <- .nvel_rule_defaults(
    values[names(rules)], values$model,
    values$dbh,
    values$spcd, values$region, measurement_system
  )
  prepared$aux <- .set_aux_caller_units(prepared$aux, measurement_system)
  if (!is.null(prepared$aux$bark_ratio) && all(is.na(prepared$aux$bark_ratio)))
    prepared$aux$bark_ratio <- NULL
  result_status <- .input_status(prepared$size, values, c("dbh", "ht", "model"))
  result_status <- .assign_status(result_status, values$dbh <= 0 | values$dbh > 400, 2L)
  result_status <- .assign_status(result_status, values$ht <= 0 | values$ht > 500, 3L)
  resolved <- .resolve_models(values$model, prepared$aux, result_status)
  result_status <- resolved$status
  output <- matrix(NA_real_, nrow = prepared$size, ncol = 15L)
  component_status <- matrix(rep(result_status, 17L), nrow = prepared$size, ncol = 17L)
  primary_logs <- secondary_logs <- rep(NA_integer_, prepared$size)
  for (group_index in seq_along(resolved$dictionary)) {
    model_object <- resolved$models[[group_index]]
    if (is.null(model_object))
      next
    group_rows <- resolved$groups[[as.character(group_index)]]
    rows <- group_rows[result_status[group_rows] %in% c(0L, 52L, 102L)]
    if (!length(rows))
      next
    if (!identical(model_object$form, "nsvb"))
      next
    native <- function(name) {
      .height_to_native(values[[name]][rows], measurement_system, "imperial")
    }
    scribner <- ifelse(values$scribner[rows] == "regional", -1L, ifelse(
      values$scribner[rows] ==
        "table", 1L, 0L
    ))
    evaluated <- tv_cpp_nsvb_biomass_impl(
      .diameter_to_native(
        values$dbh[rows],
        measurement_system, "imperial"
      ),
      native("ht"), as.integer(values$spcd[rows]), as.integer(
        values$division[rows]
      ), as.integer(values$region[rows]),
      as.integer(values$forest[rows]), integer(length(rows)), values$cull[rows],
      .diameter_to_native(
        values$primary_top[rows],
        measurement_system, "imperial"
      ), .diameter_to_native(
        values$secondary_top[rows], measurement_system,
        "imperial"
      ),
      native("stump"), native("maximum_length"), native("minimum_length"), native(
        "minimum_top_length"
      ),
      native("merchantable_length"), native("trim"), as.integer(values$even_or_odd[rows]),
      as.integer(values$option[rows]), as.integer(scribner), vapply(
        values$ctype[rows],
        utf8ToInt, integer(1)
      ), threads()
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
  generic <- which(result_status %in% c(0L, 52L, 102L) & vapply(seq_len(
    prepared$size
  ), function(row) {
    index <- match(values$model[[row]], resolved$dictionary)
    object <- if (is.na(index))
      NULL else resolved$models[[index]]
    !is.null(object) && !identical(object$form, "nsvb")
  }, logical(1)))
  if (length(generic)) {
    compatibility <- vapply(generic, function(row) {
      index <- match(values$model[[row]], resolved$dictionary)
      family <- resolved$models[[index]]$form
      if (family %in% c("clark_r8", "clark_r9")) {
        "clark"
      } else if (identical(family, "r4_driver")) {
        "r4"
      } else {
        "public"
      }
    }, character(1))
    total <- data.frame(value = rep(NA_real_, length(generic)), status = rep(
      50L,
      length(generic)
    ))
    for (path in c("public", "clark", "r4")) {
      at <- which(compatibility == path)
      if (!length(at))
        next
      selected <- generic[at]
      subset_aux <- lapply(prepared$aux, `[`, selected)
      if (path == "public") {
        total[at, ] <- do.call(.nvel_stem_volume, c(list(
          dbh = values$dbh[selected],
          ht = values$ht[selected], model = values$model[selected], lower = 0,
          lower_type = "height",
          upper_type = "tip", measurement_system = measurement_system, status = TRUE
        ), subset_aux))
      } else {
        total[at, ] <- .stem_volume_impl(
          dbh = values$dbh[selected],
          ht = values$ht[selected],
          model = values$model[selected], lower = if (path == "clark")
            values$stump[selected] else 0, lower_type = "height", upper = 0,
          upper_type = "tip", bark = "inside",
          stump_ht = NULL,
          aux = subset_aux, measurement_system = measurement_system, status = TRUE,
          function_name = "nvel_volume",
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
        identical(resolved$models[[index]]$form, "clark_r9")
      }, logical(1))
      if (any(clark9)) {
        rows <- good[clark9]
        stump <- .stem_volume_impl(
          dbh = values$dbh[rows], ht = values$ht[rows],
          model = values$model[rows],
          lower = 0, lower_type = "height", upper = values$stump[rows],
          upper_type = "height",
          bark = "inside", stump_ht = NULL, aux = lapply(prepared$aux, `[`, rows),
          measurement_system = measurement_system,
          status = TRUE, function_name = "nvel_volume", operation = 10L
        )
        stump_ok <- stump$status %in% c(0L, 52L, 102L)
        output[rows[stump_ok], 14L] <- stump$value[stump_ok]
        component_status[rows, 14L] <- stump$status
      }
      if (any(!clark9)) {
        rows <- good[!clark9]
        stump_diameter <- do.call(.nvel_dib, c(
          list(
            dbh = values$dbh[rows],
            ht = values$ht[rows],
            h = values$stump[
              rows
            ],
            model = values$model[
              rows
            ], measurement_system = measurement_system, status = TRUE
          ),
          lapply(prepared$aux, `[`, rows)
        ))
        stump_ok <- stump_diameter$status %in% c(0L, 52L, 102L)
        stump_height <- .height_to_native(values$stump[rows], measurement_system, "imperial")
        stump_dib <- .diameter_to_native(stump_diameter$value, measurement_system, "imperial")
        stump_volume <- 0.005454154 * stump_dib^2 * stump_height
        output[rows[stump_ok], 14L] <- .volume_from_native(
          stump_volume[stump_ok], measurement_system,
          "imperial"
        )
        component_status[rows, 14L] <- stump_diameter$status
      }
      output[good, c(11L, 13L)] <- 0
      component_status[good, c(11L, 13L)] <- 0L
      component_status[good, c(2:10, 12, 15:17)] <- 53L
      if (identical(.treevolume_compat(), "nvel") && any(clark9)) {
        rows <- good[clark9]
        overflow <-
          .nvel_r9_log_overflow(values, resolved, prepared$aux, rows, measurement_system)
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
  if (identical(measurement_system, "metric")) {
    cubic <- c(1L, 4L, 5L, 7L, 8L, 14L, 15L)
    output[, cubic] <- .volume_from_native(
      output[, cubic, drop = FALSE], "metric",
      "imperial"
    )
  }
  failed <- !result_status %in% c(0L, 52L, 102L)
  output[failed, ] <- NA_real_
  primary_logs[failed] <- secondary_logs[failed] <- NA_integer_
  errflag <- ifelse(result_status >= 300L & result_status <= 399L, result_status -
                      300L, result_status)
  result <- data.frame(output,
    n_logs_primary = primary_logs,
    n_logs_secondary = secondary_logs,
    errflag = as.integer(errflag), check.names = FALSE
  )
  if (!is.null(id)) {
    result <- cbind(id = values$id, result, stringsAsFactors = FALSE)
  }
  if (status_requested) {
    result <- cbind(result, .nvel_status_frame(component_status), stringsAsFactors = FALSE)
  } else {
    warning_status <- result_status
    capability <- apply(component_status == 53L, 1L, any)
    warning_status[warning_status %in% c(0L, 52L, 102L) & capability] <- 53L
    .status_warning("nvel_volume", warning_status, resolved$details)
  }
  result
}

#' Dry biomass in metric tonnes from tree measurements in inches and feet
#'
#' Outputs are metric tonnes and inputs are inches and feet.
#'
#' @param dbh Diameter at breast height outside bark. Numeric vector,
#'   inches. Required, with no default.
#' @param ht Total height above ground. Numeric vector, feet.
#'   Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param division The ecological division selects biomass coefficients for the tree. Numeric
#'   vector, division codes, 0 selects national coefficients. Default: \code{0}.
#' @param ... Additional named inputs supply measurements required by the selected model. Named
#'   vectors in inches for diameters and feet for heights, none by default.
#' @return One row per input row in input order. All mass columns use metric tonnes:
#'   * `dry_aboveground_no_foliage`: dry aboveground mass excluding foliage.
#'   * `dry_stem_wood`, `dry_stem_bark`: dry stem components.
#'   * `dry_stump_wood`, `dry_stump_bark`: dry stump components.
#'   * `dry_saw_wood`, `dry_saw_bark`: dry sawlog components.
#'   * `dry_topwood_wood`, `dry_topwood_bark`: dry topwood components.
#'   * `dry_tip_wood`, `dry_tip_bark`: dry tip components.
#'   * `dry_branches`, `dry_foliage`, `dry_top_and_limb`: dry crown components.
#'   * `carbon`: carbon mass excluding foliage.
#'   * `tco2e`: carbon dioxide equivalent.
#'
#'   The integer status column describes the row, including rows with invalid inputs.
#' @usage
#' biomass(
#'   dbh,
#'   ht,
#'   spcd,
#'   division = 0,
#'   ...
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Estimate mass with national coefficients
#' biomass(dbh = example_trees$dbh[1],
#'         ht = example_trees$ht[1],
#'         spcd = example_trees$spcd[1]) %>%
#'   transmute(`dry mass excluding foliage (tonnes)` = dry_aboveground_no_foliage,
#'             `carbon (tonnes)` = carbon,
#'             `carbon dioxide equivalent (tonnes)` = tco2e)
biomass <- function(dbh, ht, spcd, division = 0, ...) {
  if (!is.numeric(spcd) || !is.numeric(division)) {
    stop("spcd and division must be numeric.", call. = FALSE)
  }
  invalid_species <- is.finite(spcd) & (spcd <= 0 | spcd != floor(spcd) |
                                          spcd > .Machine$integer.max)
  invalid_division <- is.finite(division) & (division < 0 | division > 1999 |
                                               division != floor(division))
  spcd[invalid_species] <- NA_real_
  division[invalid_division] <- NA_real_
  native <- .biomass_impl(
    dbh, ht, spcd, division, "nsvb", NULL, list(...),
    "imperial", TRUE
  )
  mass <- native[c(.biomass_names[startsWith(.biomass_names, "dry_")], "carbon")]
  mass[] <- lapply(mass, function(x) x * 0.45359237 / 1000)
  mass$tco2e <- mass$carbon * 44 / 12
  mass$status <- native$carbon_status
  invalid_species <- rep_len(invalid_species, nrow(mass))
  mass$status[invalid_species] <- 7L
  .public_status(mass)
}

#' Look up the carbon fraction of dry tree biomass
#'
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @return A numeric vector of carbon fractions of dry mass, with a source attribute. Unknown
#'   codes return NA with a warning.
#' @usage
#' carbon_fraction(
#'   spcd
#' )
#' @export
#' @examples
#' ## Look up the carbon fraction for the example species.
#' fraction <- carbon_fraction(spcd = example_trees$spcd[1])
#'
#' ## Show the fraction without its provenance attribute
#' c(`carbon fraction (unitless)` = fraction)
carbon_fraction <- function(spcd) {
  if (!is.numeric(spcd)) {
    stop("spcd must be numeric.", call. = FALSE)
  }
  missing <- !is.finite(spcd)
  invalid <- !missing & (spcd <= 0 | spcd > .Machine$integer.max | spcd != floor(spcd))
  remapped <- .nsvb_remap_spcd(spcd)
  recognized <- !missing & !invalid & remapped %in% species_reference$spcd
  output <- rep(NA_real_, length(spcd))
  output[recognized] <- tv_cpp_nsvb_carbon_fraction_impl(
    as.integer(remapped[recognized]),
    identical(.treevolume_compat(), "nvel")
  )
  if (any(invalid | (!missing & !recognized))) {
    warning("carbon_fraction(): unknown_species for ", sum(invalid | (!missing &
                                                                        !recognized)),
      " of ", length(spcd), " trees (NA returned)",
      call. = FALSE
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
      path <- system.file("extdata", "nsvb_divisions.csv",
        package = "merchandiser",
        mustWork = TRUE
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
      path <- system.file("extdata", "nsvb_county_divisions.csv",
        package = "merchandiser",
        mustWork = TRUE
      )
      value <<- utils::read.csv(path, stringsAsFactors = FALSE)
    }
    value
  }
})

#' Look up the county ecological code for biomass equations
#'
#' @param state The state code locates the tree for ecological division lookup. Numeric vector,
#'   federal state codes. Required, with no default.
#' @param county The county code locates the tree within its state. Numeric vector, federal county
#'   codes. Required, with no default.
#' @return A data frame with value (ecological division code) and status (integer result code),
#'   in input order. A county without a division returns status 8, outside_divisions.
#' @usage
#' nsvb_division(
#'   state,
#'   county
#' )
#' @export
#' @examples
#' ## Find the ecological division for the source example county.
#' nsvb_division(state = 41,
#'               county = 39)
nsvb_division <- function(state, county) {
  prepared <- .prepare_vectors(list(state = state, county = county),
    numeric_names = c(
      "state",
      "county"
    ), character_names = character(), aux = list()
  )
  values <- prepared$values
  .validate_integer_values(values$state, "state", 1, 99)
  .validate_integer_values(values$county, "county", 1, 999)
  output <- rep(NA_integer_, prepared$size)
  valid <- is.finite(values$state) & is.finite(values$county)
  table <- .nsvb_county_division_data()
  key <- values$state * 1000 + values$county
  table_key <- table$state * 1000 + table$county
  output[valid] <- table$division[match(key[valid], table_key)]
  status <- ifelse(!valid, 1L, ifelse(is.na(output), 8L, 0L))
  data.frame(value = as.integer(output), status = status)
}

.nsvb_ecoprov <- function(region, forest, district) {
  prepared <- .prepare_vectors(list(region = region, forest = forest, district = district),
    numeric_names = c(
      "region", "forest", "district"
    ), character_names = character(), aux = list()
  )
  values <- prepared$values
  for (name in names(values)) {
    .validate_integer_values(values[[name]], name, 0, 99)
  }
  output <- rep(NA_integer_, prepared$size)
  valid <- is.finite(values$region) & is.finite(values$forest) & is.finite(values$district)
  if (!any(valid)) {
    return(output)
  }
  table <- .nsvb_ecoprov_data()
  district_table <- table[table$level == "district", ]
  forest_table <- table[table$level == "forest", ]
  region_table <- table[table$level == "region", ]
  district_key <- values$region * 10000 + values$forest * 100 + values$district
  forest_key <- values$region * 100 + values$forest
  output[valid] <- district_table$division[match(district_key[valid], district_table$key)]
  needs_forest <- valid & is.na(output)
  output[needs_forest] <- forest_table$division[match(
    forest_key[needs_forest],
    forest_table$key
  )]
  needs_region <- valid & is.na(output)
  output[needs_region] <- region_table$division[match(
    values$region[needs_region],
    region_table$key
  )]
  as.integer(output)
}

NULL

.nvel_stem_volume <- function(
  dbh, ht, model, lower = 0, lower_type = "stump",
  upper = 0, upper_type = "tip",
  bark = "inside", measurement_system = "imperial", status = TRUE, ...
) {
  .stem_volume_impl(
    dbh, ht, model, lower, lower_type, upper, upper_type, bark,
    NULL, list(...),
    measurement_system, status
  )
}

.nvel_dib <- function(dbh, ht, h, model, measurement_system = "imperial", status = TRUE, ...) {
  .diameter_impl(dbh, ht, h, model, list(...), measurement_system, status, "dib", "dib")
}
