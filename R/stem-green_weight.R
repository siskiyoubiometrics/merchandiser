.green_weight_species_data <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      path <- system.file("extdata", "nsvb_species_reference.csv",
        package = "merchandiser",
        mustWork = TRUE
      )
      value <<- utils::read.csv(path, stringsAsFactors = FALSE)
    }
    value
  }
})

.species_labels <- function(spcd) {
  matched <- match(spcd, species_reference$spcd)
  common <- species_reference$common[matched]
  ifelse(!is.na(common) & nzchar(common), paste0(common, " (", spcd, ")"), paste0(
    "species ",
    spcd
  ))
}

.green_weight_water_density <- 62.4

.validate_weight_override <- function(
  value, name, minimum, maximum = Inf,
  minimum_inclusive = TRUE
) {
  finite <- is.finite(value)
  below_minimum <- if (minimum_inclusive) {
    value < minimum
  } else {
    value <= minimum
  }
  invalid <- finite & (below_minimum | value > maximum)
  if (any(invalid)) {
    count <- sum(invalid)
    stop(name, " has ", count, " out-of-domain ", if (count == 1L)
           "value." else "values.", call. = FALSE)
  }
  invisible(NULL)
}

.validate_weight_choice <- function(value, name, choices) {
  present <- !is.na(value)
  matched <- vapply(value[present], function(item) pmatch(item, choices), integer(1L))
  if (anyNA(matched)) {
    stop(name, " must contain only: ", paste(choices, collapse = ", "), ".", call. = FALSE)
  }
  value[present] <- choices[matched]
  value
}

#' Convert cubic volume to merchandising weight
#'
#' @param volume The cubic volume is the amount of solid wood to weigh. Numeric vector, cubic
#'   feet. Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param inside_bark The bark flag says whether the supplied volume excludes bark. Logical,
#'   TRUE for volume inside bark and FALSE for volume including bark. Default: \code{TRUE}.
#' @param component The component selects the part of the tree to measure. Character scalar,
#'   'stem', 'wood', or 'bark'. Default: \code{'stem'}.
#' @param moisture The moisture choice selects green or dry merchandising weight. Character
#'   scalar, 'green' or 'dry'. Default: \code{'green'}.
#' @param specific_gravity The wood specific gravity replaces the species table value. Numeric
#'   vector, ratio to water density. Default: \code{NULL}.
#' @param moisture_pct The wood moisture replaces the species table value. Numeric vector, percent
#'   of dry mass. Default: \code{NULL}.
#' @param bark_specific_gravity The bark specific gravity replaces the species table value.
#'   Numeric vector, ratio to water density. Default: \code{NULL}.
#' @param bark_moisture_pct The bark moisture replaces the species table value. Numeric vector,
#'   percent of dry mass. Default: \code{NULL}.
#' @param bark_volume_pct The bark volume replaces the species table value. Numeric vector,
#'   percent of inside-bark wood volume. Default: \code{NULL}.
#' @details
#' Specific gravities must be at most 2. Wood and bark moisture percentages must be at
#'   most 300. Bark volume percentage must be at most 100.
#' @return A data frame with value (merchandising weight, short tons) and status
#'   (integer result code), one row per input row.
#' @usage
#' green_weight(
#'   volume,
#'   spcd,
#'   inside_bark = TRUE,
#'   component = 'stem',
#'   moisture = 'green',
#'   specific_gravity = NULL,
#'   moisture_pct = NULL,
#'   bark_specific_gravity = NULL,
#'   bark_moisture_pct = NULL,
#'   bark_volume_pct = NULL
#' )
#' @export
#' @examples
#' ## Load data verbs
#' library(dplyr)
#'
#' ## Measure the first example stem
#' volume <- stem_volume(dbh = example_trees$dbh[1],
#'                       ht = example_trees$ht[1],
#'                       spcd = example_trees$spcd[1])
#'
#' ## Convert volume using its species properties
#' green_weight(volume = volume$value,
#'              spcd = example_trees$spcd[1]) %>%
#'   rename(`green weight (short tons)` = value)
green_weight <- function(
  volume,
  spcd,
  inside_bark = TRUE,
  component = "stem",
  moisture = "green",
  specific_gravity = NULL,
  moisture_pct = NULL,
  bark_specific_gravity = NULL,
  bark_moisture_pct = NULL,
  bark_volume_pct = NULL
) {
  if (missing(inside_bark) || is.null(inside_bark)) {
    inside_bark <- TRUE
  }
  if (!is.logical(inside_bark)) {
    stop("inside_bark must be logical.", call. = FALSE)
  }
  volume_basis <- ifelse(inside_bark, "inside", "outside")
  if (missing(component) || is.null(component)) {
    component <- "stem"
  }
  if (missing(moisture) || is.null(moisture)) {
    moisture <- "green"
  }
  measurement_system <- "imperial"
  status_requested <- TRUE
  overrides <- Filter(Negate(is.null), list(
    specific_gravity = specific_gravity, moisture_pct = moisture_pct,
    bark_specific_gravity = bark_specific_gravity, bark_moisture_pct = bark_moisture_pct,
    bark_volume_pct = bark_volume_pct
  ))
  inputs <- c(list(
    volume = volume, spcd = spcd, volume_basis = volume_basis, component = component,
    moisture = moisture
  ), overrides)
  prepared <- .prepare_vectors(inputs,
    numeric_names = c("volume", "spcd", names(overrides)),
    character_names = c("volume_basis", "component", "moisture"), aux = list()
  )
  values <- prepared$values
  values$volume_basis <- .validate_weight_choice(values$volume_basis, "volume_basis", c(
    "inside",
    "outside"
  ))
  values$component <- .validate_weight_choice(values$component, "component", c(
    "stem", "wood",
    "bark"
  ))
  values$moisture <- .validate_weight_choice(values$moisture, "moisture", c("green", "dry"))
  for (name in intersect(names(overrides), c(
    "specific_gravity",
    "bark_specific_gravity"
  ))) {
    .validate_weight_override(values[[name]], name, 0, 2, minimum_inclusive = FALSE)
  }
  for (name in intersect(names(overrides), c("moisture_pct", "bark_moisture_pct"))) {
    .validate_weight_override(values[[name]], name, 0, 300)
  }
  if (!is.null(overrides$bark_volume_pct)) {
    .validate_weight_override(values$bark_volume_pct, "bark_volume_pct", 0, 100)
  }
  result_status <- .input_status(prepared$size, values, names(values))
  if (any(is.finite(values$volume) & values$volume < 0)) {
    stop("volume must contain nonnegative values.", call. = FALSE)
  }
  invalid_species <- is.finite(values$spcd) & (values$spcd <= 0 |
                                                 values$spcd > .Machine$integer.max |
                                                 values$spcd != floor(values$spcd))
  result_status <- .assign_status(result_status, invalid_species, 7L,
    eligible = result_status ==
      0L
  )

  reference <- .green_weight_species_data()
  species_row <- match(as.integer(values$spcd), reference$spcd)
  unknown_species <- result_status == 0L & is.na(species_row)
  result_status[unknown_species] <- 7L
  output <- rep(NA_real_, prepared$size)
  candidate <- which(result_status == 0L)
  missing_reference <- integer()
  if (length(candidate)) {
    selected <- reference[species_row[candidate], , drop = FALSE]
    wood_dry_weight <- if (is.null(overrides$specific_gravity)) {
      selected$wood_dry_weight
    } else {
      values$specific_gravity[candidate] * .green_weight_water_density
    }
    wood_moisture <- if (is.null(overrides$moisture_pct)) {
      selected$wood_moisture
    } else {
      values$moisture_pct[candidate]
    }
    bark_dry_weight <- if (is.null(overrides$bark_specific_gravity)) {
      selected$bark_dry_weight
    } else {
      values$bark_specific_gravity[candidate] * .green_weight_water_density
    }
    bark_moisture <- if (is.null(overrides$bark_moisture_pct)) {
      selected$bark_moisture
    } else {
      values$bark_moisture_pct[candidate]
    }
    bark_volume_pct_value <- if (is.null(overrides$bark_volume_pct)) {
      selected$bark_to_wood_volume
    } else {
      values$bark_volume_pct[candidate]
    }
    candidate_component <- values$component[candidate]
    candidate_moisture <- values$moisture[candidate]
    candidate_basis <- values$volume_basis[candidate]
    needs_wood <- candidate_component != "bark"
    needs_bark <- candidate_component != "wood"
    needs_ratio <- needs_bark | candidate_basis == "outside"
    missing_value <- rep(FALSE, length(candidate))
    if (any(needs_wood)) {
      missing_value <- missing_value | needs_wood & (!is.finite(wood_dry_weight) |
                                                       wood_dry_weight <=
                                                         0)
      green_wood <- needs_wood & candidate_moisture == "green"
      if (any(green_wood)) {
        missing_value <- missing_value | green_wood & (!is.finite(wood_moisture) |
                                                         wood_moisture <
                                                           0)
      }
    }
    if (any(needs_bark)) {
      missing_value <- missing_value | needs_bark & (!is.finite(bark_dry_weight) |
                                                       bark_dry_weight <=
                                                         0)
      green_bark <- needs_bark & candidate_moisture == "green"
      if (any(green_bark)) {
        missing_value <- missing_value | green_bark & (!is.finite(bark_moisture) |
                                                         bark_moisture <
                                                           0)
      }
    }
    if (any(needs_ratio)) {
      missing_value <- missing_value | needs_ratio & !is.finite(bark_volume_pct_value)
      if (is.null(overrides$bark_volume_pct)) {
        missing_value <- missing_value | needs_ratio & bark_volume_pct_value <= 0
      }
    }
    missing_reference <- candidate[missing_value]
    result_status[missing_reference] <- 1L

    available <- !missing_value
    computable <- candidate[available]
    if (length(computable)) {
      native_volume <- if (measurement_system == "metric") {
        values$volume[computable] / 0.028316846592
      } else {
        values$volume[computable]
      }
      available_basis <- candidate_basis[available]
      available_component <- candidate_component[available]
      available_moisture <- candidate_moisture[available]
      ratio <- bark_volume_pct_value[available] / 100
      wood_volume <- ifelse(available_basis == "inside", native_volume, native_volume / (1 +
                                                                                           ratio))
      bark_volume <- ifelse(available_basis == "inside", native_volume * ratio,
        native_volume *
          ratio / (1 + ratio)
      )
      wood_multiplier <- ifelse(available_moisture == "green", 1 +
          wood_moisture[available] / 100,
        1
      )
      bark_multiplier <- ifelse(available_moisture == "green", 1 +
          bark_moisture[available] / 100,
        1
      )
      wood_weight <- wood_volume * wood_dry_weight[available] * wood_multiplier
      bark_weight <- bark_volume * bark_dry_weight[available] * bark_multiplier
      native_weight <- ifelse(available_component == "stem", wood_weight + bark_weight,
        ifelse(available_component == "wood", wood_weight, bark_weight)
      )
      output[computable] <- if (measurement_system == "metric") {
        native_weight * 0.45359237
      } else {
        native_weight
      }
    }
  }

  if (status_requested) {
    return(data.frame(value = output / 2000, status = as.integer(result_status)))
  }
  if (length(missing_reference)) {
    named_species <- unique(.species_labels(as.integer(values$spcd[missing_reference])))
    warning("green_weight(): missing table values for ", paste(named_species,
        collapse = ", "
      ),
      " (NA returned)",
      call. = FALSE
    )
  }
  .status_warning("green_weight", result_status)
  output
}

.nvel_weight_factor_data <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      path <- system.file("extdata", "nsvb_coefficients.csv",
        package = "merchandiser",
        mustWork = TRUE
      )
      coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)
      selected <- coefficients$source_file == "regndftdata.inc" & tolower(
        coefficients$target
      ) ==
        "spregndftwf"
      values <- matrix(coefficients$value[selected], ncol = 7L, byrow = TRUE)
      value <<- data.frame(region = as.integer(values[, 1L]), forest = as.integer(values[
        ,
        2L
      ]), spcd = as.integer(values[, 3L]), primary = values[, 4L], secondary = values[
        ,
        5L
      ], dead = values[, 7L])
    }
    value
  }
})

# Internal source-compatible access to the regional NVEL pounds-per-cubic-foot factors.
# This is deliberately separate from green_weight().
nvel_weight_factor <- function(
  spcd, region = 0, forest = 0, product = "01",
  live = TRUE, status = FALSE
) {
  prepared <- .prepare_vectors(
    list(
      spcd = spcd, region = region, forest = forest, product = product,
      live = live
    ),
    numeric_names = c("spcd", "region", "forest"), character_names = "product",
    aux = list()
  )
  values <- prepared$values
  if (!is.logical(values$live)) {
    stop("live must be logical.", call. = FALSE)
  }
  if (any(!is.na(values$product) & !grepl("^[0-9]{2}$", values$product))) {
    stop("product must contain two-digit NVEL product codes.", call. = FALSE)
  }
  .validate_integer_values(values$region, "region", 0, 99)
  .validate_integer_values(values$forest, "forest", 0, 99)
  status_requested <- .validate_status(status)
  result_status <- .input_status(prepared$size, values, c(
    "spcd", "region", "forest", "product",
    "live"
  ))
  invalid_species <- is.finite(values$spcd) & (values$spcd <= 0 |
                                                 values$spcd > .Machine$integer.max |
                                                 values$spcd != floor(values$spcd))
  result_status <- .assign_status(result_status, invalid_species, 7L)
  reference <- .green_weight_species_data()
  species_row <- match(as.integer(values$spcd), reference$spcd)
  output <- rep(NA_real_, prepared$size)
  regional <- .nvel_weight_factor_data()
  fallback_row <- match(999L, reference$spcd)
  for (index in which(result_status == 0L)) {
    matches <- which(regional$region == values$region[[index]] &
                       regional$spcd == values$spcd[[index]] &
                       (regional$forest == values$forest[[index]] | regional$forest == 0L))
    if (values$region[[index]] != 0L && length(matches)) {
      row <- matches[[1L]]
      factor <- regional$primary[[row]]
      if (values$product[[index]] != "01" && regional$secondary[[row]] != 0) {
        factor <- regional$secondary[[row]]
      }
      dead_factor <- regional$dead[[row]]
    } else {
      if (is.na(species_row[[index]])) {
        result_status[[index]] <- 7L
        next
      }
      factor <- reference$green_weight_factor[species_row[[index]]]
      if (!is.finite(factor) || factor == 0) {
        factor <- reference$green_weight_factor[fallback_row]
      }
      dead_factor <- 0
    }
    if (!values$live[[index]]) {
      if (!is.finite(dead_factor) || dead_factor < 1) {
        multiplier <- switch(as.character(as.integer(values$region[[index]])),
          `1` = 0.6749,
          `2` = 0.6381,
          `4` = 0.6113,
          `5` = 0.8254,
          `7` = 0.7951,
          0.7036
        )
        dead_factor <- factor * multiplier
      }
      factor <- dead_factor
    }
    output[[index]] <- factor
  }
  .status_result(output, result_status, status_requested,
    function_name = "nvel_weight_factor"
  )
}
