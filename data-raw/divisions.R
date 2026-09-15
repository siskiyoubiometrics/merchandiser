# Regenerate the NSVB ecological division polygons used by nsvb_division_xy().  The NSVB
# codes retain the 2007 ECOMAP hierarchy. Mountain divisions use an M prefix in the source,
# encoded by treevolume as 1,000 plus the numeric code.  ECOMAP 2025 restructures the codes
# and does not contain those mountain divisions, so it is not compatible with the NSVB
# coefficient tables.  Run from the package root. sf is used only while regenerating
# package data.

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("install the sf package to regenerate NSVB division polygons")
}

source_name <- "Ecosys_EcoMapProvinces_2007.zip"
source_sha256 <- "6f4463d053a3ca6a36d5a642bbbe4fbb09a9d66576047c1ed789e149f5affd91"
source_root <- Sys.getenv("TREEVOLUME_EXTERNAL", unset = file.path(
  "..", "merchandiser", "tools",
  "external"
))
source_zip <- file.path(source_root, source_name)
if (!file.exists(source_zip)) {
  stop("missing ECOMAP source archive: ", source_zip)
}
actual_sha256 <- strsplit(system2("sha256sum", source_zip, stdout = TRUE), " ",
  fixed = TRUE
)[[1L]][1L]
if (!identical(actual_sha256, source_sha256)) {
  stop("ECOMAP 2007 archive checksum does not match the pinned source")
}

extract_dir <- tempfile("treevolume-ecomap-")
dir.create(extract_dir)
on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
utils::unzip(source_zip, exdir = extract_dir)
shape_path <- list.files(extract_dir,
  pattern = "[.]shp$", full.names = TRUE,
  ignore.case = TRUE
)
if (length(shape_path) != 1L) {
  stop("expected exactly one shapefile in the ECOMAP 2007 archive")
}

provinces <- sf::st_read(shape_path, quiet = TRUE)
required_columns <- c("MAP_UNIT_S", "MAP_LEVEL", "PROJECT")
if (!all(required_columns %in% names(provinces))) {
  stop("ECOMAP 2007 province attributes have an unexpected schema")
}
if (!all(provinces$MAP_LEVEL == "PROV") || !all(provinces$PROJECT == "ECOMAP07")) {
  stop("ECOMAP source does not identify the expected 2007 province layer")
}

source_code <- trimws(as.character(provinces$MAP_UNIT_S))
keep <- source_code != "Water"
provinces <- provinces[keep, ]
source_code <- source_code[keep]
mountain <- startsWith(source_code, "M")
numeric_code <- suppressWarnings(as.integer(sub("^M", "", source_code)))
if (anyNA(numeric_code)) {
  stop("ECOMAP 2007 contains an unrecognized province code")
}
provinces$division <- numeric_code %/% 10L * 10L + ifelse(mountain, 1000L, 0L)

required_nsvb_codes <- c(210L, 230L, 1240L, 1330L)
if (!all(required_nsvb_codes %in% provinces$division)) {
  stop("ECOMAP 2007 does not contain the reviewed NSVB division codes")
}

provinces <- sf::st_make_valid(sf::st_transform(provinces, 5070))
division_codes <- sort(unique(provinces$division))
division_geometry <- sf::st_sfc(lapply(division_codes, function(code) {
  selected <- sf::st_geometry(provinces[provinces$division == code, ])
  sf::st_cast(sf::st_union(selected), "MULTIPOLYGON", warn = FALSE)[[1L]]
}), crs = 5070)
divisions <- sf::st_sf(division = division_codes, geometry = division_geometry)

vertex_count <- function(value) {
  sum(vapply(sf::st_geometry(value), function(geometry) {
    length(unlist(geometry, use.names = FALSE)) / 2
  }, numeric(1)))
}

original_area <- as.numeric(sf::st_area(divisions))
original_vertices <- vertex_count(divisions)
simplification_tolerance_m <- 500
simplified <- sf::st_simplify(divisions,
  dTolerance = simplification_tolerance_m,
  preserveTopology = TRUE
)
simplified$geometry <- sf::st_cast(sf::st_geometry(simplified), "MULTIPOLYGON",
  warn = FALSE
)
area_difference_pct <- 100 * abs(as.numeric(sf::st_area(simplified)) -
                                   original_area) / original_area
if (any(area_difference_pct > 0.5)) {
  stop("simplified division area differs from the source by more than 0.5 percent")
}
simplified_vertices <- vertex_count(simplified)
simplified <- sf::st_transform(simplified, 4326)
simplified$geometry <- sf::st_cast(sf::st_geometry(simplified), "MULTIPOLYGON",
  warn = FALSE
)

coordinates <- sf::st_coordinates(simplified)
level_names <- colnames(coordinates)
if (!all(c("X", "Y", "L1", "L2", "L3") %in% level_names)) {
  stop("unexpected coordinate index structure after ECOMAP conversion")
}
ring_key <- paste(coordinates[, "L3"], coordinates[, "L2"], coordinates[, "L1"], sep = ":")
nsvb_division_polygons <- data.frame(
  division = as.integer(simplified$division[coordinates[
    ,
    "L3"
  ]]), ring_id = as.integer(match(ring_key, unique(ring_key))), hole = coordinates[, "L1"] >
    1, x = as.double(coordinates[, "X"]), y = as.double(coordinates[, "Y"]),
  stringsAsFactors = FALSE
)
attr(nsvb_division_polygons, "source") <- paste(
  "USDA Forest Service ECOMAP 2007 province layer",
  paste0("SHA-256 ", source_sha256)
)
attr(nsvb_division_polygons, "source_crs") <- "NAD83"
attr(nsvb_division_polygons, "crs") <- "EPSG:4326"
attr(nsvb_division_polygons, "simplification_tolerance_m") <- simplification_tolerance_m
attr(nsvb_division_polygons, "maximum_area_difference_pct") <- max(area_difference_pct)

centroids <- sf::st_centroid(sf::st_geometry(divisions))
centroid_in_own_division <- vapply(seq_along(division_codes), function(index) {
  index %in% sf::st_covered_by(centroids[index], divisions)[[1L]]
}, logical(1))
representative_points <- centroids
representative_points[!centroid_in_own_division] <- sf::st_point_on_surface(
  sf::st_geometry(divisions[!centroid_in_own_division, ])
)
representative_points <- sf::st_transform(representative_points, 4326)
point_coordinates <- sf::st_coordinates(representative_points)
nsvb_division_points <- data.frame(
  division = division_codes,
  x = point_coordinates[, "X"], y = point_coordinates[
    ,
    "Y"
  ], method = ifelse(centroid_in_own_division, "centroid", "point_on_surface"),
  stringsAsFactors = FALSE
)

dir.create("data", showWarnings = FALSE)
save(nsvb_division_polygons,
  file = file.path("data", "nsvb_division_polygons.rda"), compress = "xz",
  version = 3L
)
dir.create(file.path("tests", "testthat", "fixtures"),
  recursive = TRUE,
  showWarnings = FALSE
)
utils::write.csv(nsvb_division_points, file.path(
  "tests", "testthat", "fixtures",
  "nsvb_division_points.csv"
),
row.names = FALSE
)

if (!file.exists(file.path("data", "species_reference.rda"))) {
  stop("data/species_reference.rda is required before regenerating R/sysdata.rda")
}
load(file.path("data", "species_reference.rda"))
save(species_reference, nsvb_division_polygons,
  file = file.path("R", "sysdata.rda"), compress = "xz",
  version = 3L
)

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
provenance <- data.frame(
  source_file = source_name, source_sha256 = source_sha256,
  source_layer = "S_USA.Ecosys_EcoMapProvinces_2007",
  source_code_field = "MAP_UNIT_S", source_feature_count = nrow(provinces),
  division_count = length(division_codes),
  original_vertex_count = original_vertices,
  simplification_tolerance_m = simplification_tolerance_m,
  simplified_vertex_count = simplified_vertices, maximum_area_difference_pct = max(
    area_difference_pct
  ),
  polygon_table_rows = nrow(nsvb_division_polygons),
  compressed_data_bytes = file.info(file.path(
    "data",
    "nsvb_division_polygons.rda"
  ))$size, stringsAsFactors = FALSE
)
utils::write.csv(provenance, file.path(
  "inst", "extdata",
  "nsvb_division_provenance.csv"
), row.names = FALSE)

message(
  "Wrote ", length(division_codes), " divisions with ", nrow(
    nsvb_division_polygons
  ), " coordinate rows. The ",
  simplification_tolerance_m, " m tolerance retained ", simplified_vertices, " of ",
  original_vertices,
  " vertices. Maximum area difference: ", format(max(area_difference_pct),
    digits = 12
  ), " percent. Compressed data: ",
  provenance$compressed_data_bytes, " bytes."
)
