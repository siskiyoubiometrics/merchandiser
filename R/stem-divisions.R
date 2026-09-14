#' Inspect the boundaries used to select biomass equations
#'
#' Inspect the shipped boundaries used to assign ecological divisions. The data frame needs no
#' inputs and contains ordered polygon vertices in longitude and latitude. Division labels
#' match the codes used by the national biomass equations.
#'
#' Use [nsvb_division_xy()] for a coordinate lookup.
#' @details Province boundaries from the Forest Service ecological mapping data are combined into
#' broader ecological divisions. The numeric division code retains the mountain prefix.
#'
#' @format A data frame with these columns:
#' \describe{
#'   \item{division}{Integer National Scale Volume and Biomass equations ecological division code.}
#'   \item{ring_id}{Unique polygon ring identifier.}
#'   \item{hole}{Whether the ring is a hole.}
#'   \item{x}{Longitude in decimal degrees east.}
#'   \item{y}{Latitude in decimal degrees north.}
#' }
#' @source United States Department of Agriculture Forest Service ECOMAP 2007, *Ecological
#'   Subregions: Sections
#'   and Subsections of the Conterminous United States*, General Technical Report WO-76.
#' @keywords datasets internal
#' @section Field interpretation:
#' `division` and `ring_id` are integer labels, unitless. `hole` is logical,
#' identifying an excluded interior ring. Numeric `x` and `y` are longitude
#' in decimal degrees east and latitude in decimal degrees north.
#'
#' Rows
#' trace vertices in ring order. All fields are supplied data, not defaults
#' for an argument. There are no list columns.
#'
#' Use [nsvb_division_xy()] to
#' assign coordinates without manipulating the vertices.
#' @section Status and missing values:
#' This table carries no tree status codes. A failed coordinate lookup
#' returns a missing division through [nsvb_division_xy()], not a zero code.
#' @seealso [nsvb_division_xy()] for a location lookup, [nsvb_division()]
#'   for the county-predominant province.
#' @usage
#'
#' ## Call signatures
#' nsvb_division_polygons
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect packaged ecological division attributes
#' nsvb_division_polygons %>%
#'   select(division) %>%
#'   slice_head(n = 3)
"nsvb_division_polygons"

#' Look up an ecological division by coordinates
#'
#' Supply coordinates to select the ecological division used by the national
#' biomass equations. The result is one division code per point, with an
#' optional status column. Use [nsvb_division()] when only county codes are
#' available.
#'
#' @details The shipped polygons combine province boundaries into broader divisions. Boundaries are
#' simplified and stored as longitude and latitude.
#'
#' Coordinate system 4326 requires no additional package. Other reference systems require `sf` to
#' transform the coordinates.
#' @param x Required numeric longitude or horizontal coordinate, length one
#'   or one per point. Default geographic coordinates use decimal degrees
#'   east. Other coordinates use the supplied reference system's units.
#'
#' Missing or nonfinite values give `na_input`.
#' @param y Required numeric latitude or vertical coordinate, length one or
#'   one per point. Default geographic coordinates use decimal degrees north. Other coordinates use
#' the supplied reference system's units.
#'
#' Missing
#'   or nonfinite values give `na_input`. Omission is an error.
#' @param crs One coordinate reference system identifier, numeric or character. Default `4326`
#'   selects longitude and latitude on the standard geographic
#'   reference used by the shipped polygons. Other systems require package
#'   `sf` for transformation.
#'
#' Missing or invalid identifiers are errors. Unitless.
#' @param status One nonmissing logical flag, default `FALSE` for an integer vector.
#'   `TRUE` returns integer `value` and integer `status`, both unitless,
#'   and suppresses row warnings. Example: `status = TRUE`.
#'
#' @return An integer vector with one division per input point, or a data frame
#'   with `value` and `status`.
#' @seealso [nsvb_division()] for predominant county lookup. The county helper
#'   returns its table's county-predominant province. This coordinate
#'   helper returns the broader division used by the biomass equations.
#' @export
#' @usage
#'
#' ## Call signatures
#' nsvb_division_xy(x, y, crs = 4326, status = FALSE)
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Assign ecological divisions to the shipped coordinates
#' example_trees_pnw %>%
#'   distinct(longitude, latitude) %>%
#'   mutate(division = nsvb_division_xy(x = longitude, y = latitude))
#' @section Status and missing values:
#' `ok` means a division was found. `na_input` means a missing or nonfinite coordinate, and
#' `outside_divisions` means no shipped division contains the point. Codes 1 and 8 return missing
#' division codes.
#'
#' Check coordinates and their reference system before using the result. `outside_divisions` warns
#' when
#' `status = FALSE`.
#' @section Boundary rules:
#' Holes are excluded and polygon boundaries are
#' included. On a shared boundary, the smaller numeric division code is selected.
#'
#' These simplified polygons support equation selection, not precise site-boundary surveying.
nsvb_division_xy <- function(x, y, crs = 4326, status = FALSE) {
  status_requested <- .validate_status(status)
  prepared <- .prepare_vectors(
    list(x = x, y = y), numeric_names = c("x", "y"),
    character_names = character(), aux = list()
  )
  values <- prepared$values
  result_status <- .input_status(prepared$size, values, c("x", "y"))
  valid <- which(result_status == 0L)

  if (length(crs) != 1L || is.na(crs)) {
    stop("crs must identify one coordinate reference system.", call. = FALSE)
  }
  is_4326 <- (is.numeric(crs) && is.finite(crs) && crs == 4326) ||
    (is.character(crs) && crs %in% c("4326", "EPSG:4326"))
  if (!is_4326) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(
        "crs values other than 4326 require the suggested sf package.",
        call. = FALSE
      )
    }
    source_crs <- tryCatch(
      suppressWarnings(sf::st_crs(crs)),
      error = function(condition) NULL
    )
    if (is.null(source_crs) || is.na(source_crs)) {
      stop("crs does not identify a valid coordinate reference system.",
           call. = FALSE)
    }
    if (length(valid)) {
      points <- sf::st_as_sf(
        data.frame(x = values$x[valid], y = values$y[valid]),
        coords = c("x", "y"), crs = source_crs
      )
      transformed <- tryCatch(
        sf::st_transform(points, 4326),
        error = function(condition) {
          stop("crs could not be transformed to EPSG:4326: ",
               condition$message, call. = FALSE)
        }
      )
      coordinates <- sf::st_coordinates(transformed)
      values$x[valid] <- coordinates[, "X"]
      values$y[valid] <- coordinates[, "Y"]
    }
  }

  output <- rep(NA_integer_, prepared$size)
  if (length(valid)) {
    polygons <- nsvb_division_polygons
    output[valid] <- tv_cpp_nsvb_division_xy_impl(
      values$x[valid], values$y[valid], polygons$division,
      polygons$ring_id, polygons$hole, polygons$x, polygons$y, threads()
    )
    outside <- valid[is.na(output[valid])]
    result_status[outside] <- 8L
  }
  .status_result(
    output, result_status, status_requested,
    function_name = "nsvb_division_xy"
  )
}
