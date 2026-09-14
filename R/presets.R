.mc_preset_index <- data.frame(
  name = c("pnw", "us_south", "douglas_fir"),
  region = c("Pacific Northwest", "United States South", "Douglas-fir example"),
  nvel_region = c(6L, 8L, 6L),
  forest = c(12L, 8L, 12L),
  district = c(0L, 0L, 0L),
  status = rep("illustrative", 3L),
  stringsAsFactors = FALSE
)

.mc_preset_products <- function(name) {
  if (name == "douglas_fir") return(.mc_example_douglas_fir())
  if (name == "pnw") {
    return(products(
      product(
        "export", 1L, species = c(202L, 263L), min_dbh = 12,
        lengths = c(32, 40), min_boundary_length = 16, trim = 1,
        min_sed = 12, inside_bark = TRUE,
        volume_unit = "scribner", split_scale = FALSE,
        specification_source = "illustrative_pnw"
      ),
      product(
        "domestic_saw", 2L, species = c(202L, 263L), min_dbh = 10,
        lengths = c(16, 20, 24, 32, 40), min_boundary_length = 16, trim = 1,
        min_sed = 6, inside_bark = TRUE,
        volume_unit = "scribner", split_scale = FALSE,
        specification_source = "illustrative_pnw"
      ),
      product(
        "pulp", 3L, species = c(202L, 263L), min_dbh = 5,
        min_length = 8, max_length = 40, length_step = 1,
        min_boundary_length = 8, trim = 0.5, min_sed = 3,
        inside_bark = TRUE, accepts_pulp_restriction = TRUE,
        allow_lower_products = FALSE, volume_unit = "green_ton",
        specification_source = "illustrative_pnw"
      )
    ))
  }
  products(
    product(
      "sawtimber", 1L, species = 131L, min_dbh = 12,
      min_length = 16, max_length = NA_real_, length_step = 1,
      min_boundary_length = 16, trim = 0.5, min_sed = 8,
      inside_bark = FALSE, max_logs_per_segment = 1L,
      volume_unit = "green_ton",
      specification_source = "illustrative_us_south"
    ),
    product(
      "chip_n_saw", 2L, species = 131L, min_dbh = 8, max_dbh = 12,
      allow_after_higher_product = FALSE, min_length = 16, max_length = NA_real_,
      length_step = 1, min_boundary_length = 16, trim = 0.5,
      min_sed = 6, inside_bark = FALSE, max_logs_per_segment = 1L,
      volume_unit = "green_ton",
      specification_source = "illustrative_us_south"
    ),
    product(
      "pulpwood", 3L, species = 131L, min_dbh = 5, max_dbh = 8,
      allow_after_higher_product = TRUE,
      accepts_pulp_restriction = TRUE, min_length = 8, max_length = NA_real_,
      length_step = 1, min_boundary_length = 8, trim = 0.5,
      min_sed = 3, inside_bark = FALSE, max_logs_per_segment = 1L,
      allow_lower_products = FALSE, volume_unit = "green_ton",
      specification_source = "illustrative_us_south"
    )
  )
}

.mc_preset_name <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !x %in% c(.mc_preset_index$name, "douglas_fir")) {
    stop(
      "preset is not recognized. Choose a name returned by example_product_names() and try again.",
      call. = FALSE
    )
  }
  x
}

#' List the shipped example product tables
#'
#' List shipped example specifications and their source-default codes.
#'
#' @return A data frame with one row per example. Character `name` and `region` hold the selection
#' name and region description. Integer `nvel_region`, `forest`, and `district` hold Forest Service
#' agency codes.
#'
#' These fields are unitless. `status` is the character label `'illustrative'`, not a tree code.
#' All fields are supplied by the package and none are missing.
#' @details The geographic codes select example equations when automatic
#'   settings are requested. They do not assign a tree to an actual forest.
#' @section Status and missing values:
#' No calculation codes are returned. An illustrative label means the table
#' needs review against the product specifications before use.
#' @seealso [example_products()] to load a table, [product()] to define a table.
#' @export
#' @usage
#'
#' ## Call signatures
#' example_product_names()
#' @examples
#' ## List the available example product tables
#' example_product_names()
example_product_names <- function() {
  .mc_preset_index
}

#' Start a product table from shipped example specifications
#'
#' Load a shipped example product table by name. These synthetic specifications are example
#' inputs.
#'
#' @param name One character name or factor label, `'pnw'`, `'us_south'`, or
#'   `'douglas_fir'`. Required, with no default. Missing, unknown, empty, or
#'   multiple names are errors.
#'
#' Unitless. Example: `name = 'us_south'`.
#'
#' @details Lengths and trim are initially rounded to the nearest inch with
#'   halves upward. Original supplied lengths are retained for normalization
#'   when [merchandise()] selects imperial or metric units. Preserve the
#'   returned table and its attributes for that later conversion.
#'
#' @return A `merch_products` data frame with the columns described in
#'   [Product specification schema][product_schema].
#'   Labels are character, measurements are numeric, flags are logical, and
#'   priorities and log limits are integers. Additional `meta_` columns retain
#'   caller notes without changing calculations.
#' @section Product specifications:
#' See [Product specification schema][product_schema] for every field, its units,
#' defaults, and constraints. A discrete-length list cell can be constructed as
#' `I(list(c(16, 20)))`. Keep product attributes when passing the table to a run.
#' @section Status and missing values:
#' These functions return no calculation status codes. Invalid specifications
#' stop the call, so correct the named field before selecting logs. Missing
#' optional bounds mean no restriction only where the field definition says
#' so.
#'
#' A valid product table does not establish volume. `no_feasible_log` from a
#' later merchandising call means a valid result with no logs for that tree.
#' @seealso [product()] to specify one product, [products()] to combine rows,
#'   [merchandise()] to select logs using the resulting table.
#' @export
#' @usage
#'
#' ## Call signatures
#' example_products(name)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the example product measurements
#' example_products(name = 'pnw') %>%
#'   select(product, priority, volume_unit, inside_bark)
example_products <- function(name) {
  .mc_preset_products(.mc_preset_name(name))
}

.mc_region_settings <- function(region, preset_name, species, forest, district) {
  if (!is.null(preset_name)) preset_name <- .mc_preset_name(preset_name)
  if (is.null(region)) {
    inferred <- if (!is.null(preset_name)) {
      preset_name
    } else if (length(species) && all(!is.na(species)) && all(species == 131L)) {
      "us_south"
    } else {
      "pnw"
    }
    row <- .mc_preset_index[.mc_preset_index$name == inferred, , drop = FALSE]
  } else {
    if (is.factor(region)) region <- as.character(region)
    if (.mc_numeric(region)) {
      code <- .mc_as_integer(region, "region", allow_na = FALSE)
      if (length(code) != 1L) {
        stop(
          "region must be one value. ",
          "Supply one NVEL region code or preset name and try again.",
          call. = FALSE
        )
      }
      matched <- match(code, .mc_preset_index$nvel_region)
      row <- if (is.na(matched)) {
        data.frame(
          name = NA_character_, region = paste("NVEL region", code),
          nvel_region = code, forest = 0L, district = 0L,
          status = "caller supplied", stringsAsFactors = FALSE
        )
      } else {
        .mc_preset_index[matched, , drop = FALSE]
      }
    } else if (is.character(region) && length(region) == 1L && !is.na(region)) {
      name <- .mc_preset_name(region)
      row <- .mc_preset_index[.mc_preset_index$name == name, , drop = FALSE]
    } else {
      stop(
        "region must be one NVEL region code or preset name. ",
        "Supply one value and try again.", call. = FALSE
      )
    }
  }
  if (!is.null(forest)) {
    forest <- .mc_as_integer(forest, "forest", allow_na = FALSE)
    if (length(forest) != 1L || forest < 0L || forest > 99L) {
      stop(
        "forest must be one whole number from 0 through 99. ",
        "Correct the code and try again.", call. = FALSE
      )
    }
    row$forest <- forest
  }
  if (!is.null(district)) {
    district <- .mc_as_integer(district, "district", allow_na = FALSE)
    if (length(district) != 1L || district < 0L || district > 99L) {
      stop(
        "district must be one whole number from 0 through 99. ",
        "Correct the code and try again.", call. = FALSE
      )
    }
    row$district <- district
  }
  if (is.null(preset_name) && !is.na(row$name)) preset_name <- row$name
  list(
    label = if (is.na(row$name)) row$region else row$name,
    region = row$nvel_region, forest = row$forest, district = row$district,
    preset = preset_name
  )
}
.mc_example_douglas_fir <- function() {
  products(
    product("Large sawlog", 1, lengths = 32, trim = 0.5, min_sed = 12,
            volume_unit = "scribner", price = 900, price_per = 1000),
    product("Medium sawlog", 2, lengths = 16, trim = 0.5, min_sed = 10,
            volume_unit = "scribner", price = 700, price_per = 1000),
    product("Small sawlog", 3, lengths = 16, trim = 0.5, min_sed = 6,
            volume_unit = "scribner", price = 500, price_per = 1000),
    product("Pulp", 4, min_length = 8, max_length = 20, length_step = 1,
            trim = 0.5, min_sed = 3, volume_unit = "green_ton", price = 30,
            accepts_pulp_restriction = TRUE)
  )
}
