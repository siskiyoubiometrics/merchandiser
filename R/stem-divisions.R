#' Ecological division boundaries
#'
#' Boundary coordinates support ecological division lookup from longitude and latitude in degrees.
#' Use these polygons to inspect the spatial coverage of coefficient selection.
#' @format A data frame with division (numeric division code), ring_id (polygon
#'   ring identifier), hole (logical interior ring indicator), x (longitude,
#'   degrees), and y (latitude, degrees).
"nsvb_division_polygons"

#' Look up ecological division from coordinates
#'
#' Locate coordinates in the shipped ecological division polygons for biomass coefficient
#'   selection. Use the returned code when estimating biomass with local coefficients.
#'
#' @param x Horizontal coordinate as a numeric vector. Required, without a default. The default
#'   coordinate system uses longitude in decimal degrees. Other systems use their own coordinate
#'   units.
#' @param y Vertical coordinate as a numeric vector. Required, without a default. The default
#'   coordinate system uses latitude in decimal degrees. Coordinates outside the polygon coverage
#'   receive status 8.
#' @param crs Coordinate reference system as one numeric code or character string accepted by
#'   `sf::st_crs()`. Defaults to `4326` for longitude and latitude in degrees. A different
#'   system requires sf to transform coordinates. A constructed sf coordinate-system object
#'   is not accepted.
#' @return A data frame with `value` (integer ecological division code) and `status` (integer
#'   result code). Missing inputs receive status 1, and unrecognized locations receive status 8.
#' @usage
#' nsvb_division_xy(
#'   x,
#'   y,
#'   crs = 4326
#' )
#' @export
#' @examples
#' ## Inspect the ecological division at the shipped location
#' nsvb_division_xy(x = example_trees_pnw$longitude[1],
#'                  y = example_trees_pnw$latitude[1])
nsvb_division_xy <- function(x, y, crs = 4326) {
  status_requested <- TRUE
  prepared <- .prepare_vectors(list(x = x, y = y),
    numeric_names = c("x", "y"), character_names = character(),
    aux = list()
  )
  values <- prepared$values
  result_status <- .input_status(prepared$size, values, c("x", "y"))
  valid <- which(result_status == 0L)

  if (length(crs) != 1L || is.na(crs)) {
    stop("crs must identify one coordinate reference system.", call. = FALSE)
  }
  is_4326 <- (is.numeric(crs) && is.finite(crs) && crs == 4326) || (is.character(
    crs
  ) && crs %in%
    c("4326", "EPSG:4326"))
  if (!is_4326) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("crs values other than 4326 require the suggested sf package.", call. = FALSE)
    }
    source_crs <- tryCatch(suppressWarnings(sf::st_crs(crs)), error = function(condition) NULL)
    if (is.null(source_crs) || is.na(source_crs)) {
      stop("crs does not identify a valid coordinate reference system.", call. = FALSE)
    }
    if (length(valid)) {
      points <- sf::st_as_sf(data.frame(x = values$x[valid], y = values$y[valid]),
        coords = c(
          "x",
          "y"
        ), crs = source_crs
      )
      transformed <- tryCatch(sf::st_transform(points, 4326), error = function(condition) {
        stop("crs could not be transformed to EPSG:4326: ", condition$message,
          call. = FALSE
        )
      })
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
  .status_result(output, result_status, status_requested,
    function_name = "nsvb_division_xy"
  )
}
