# Regenerate the NSVB species reference and exported species_reference data.  The pinned
# NVEL species table supplies physical and NSVB classification fields. FIA REF_SPECIES
# supplies names and symbols when it is reachable.  Package builds never make a network
# request. Set FIA_REF_SPECIES to a local downloaded CSV to perform the join reproducibly.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv("NVEL_SOURCE", unset = file.path(
  "..", "..", "tools",
  "oracle", "nvel"
))
species_file <- file.path(nvel_source, "wdbkwtdata.inc")
if (!file.exists(species_file)) {
  stop("missing NVEL source file: ", species_file)
}
actual_commit <- system2("git", c(
  "-C", normalizePath(nvel_source), "rev-parse",
  "HEAD"
), stdout = TRUE)
if (!identical(actual_commit, nvel_commit)) {
  stop("NVEL checkout is not at the pinned commit: ", actual_commit)
}

lines <- readLines(species_file, warn = FALSE)
starts <- grep("^[ ]{0,7}DATA[ ]*\\(\\(WDBKWT", lines, ignore.case = TRUE)
rows <- list()
for (start in starts) {
  finish <- start
  slash_count <- lengths(regmatches(lines[[finish]], gregexpr("/", lines[[finish]],
                           fixed = TRUE
                         )))
  while (slash_count < 2L && finish < length(lines)) {
    finish <- finish + 1L
    slash_count <- slash_count + lengths(regmatches(lines[[finish]], gregexpr("/",
                                           lines[[finish]],
                                           fixed = TRUE
                                         )))
  }
  statement <- paste(substring(lines[start:finish], 7L), collapse = " ")
  body <- sub("^[^/]*/", "", statement)
  body <- sub("/[ ]*$", "", body)
  literals <- trimws(strsplit(body, ",", fixed = TRUE)[[1L]])
  literals <- literals[nzchar(literals)]
  values <- suppressWarnings(as.double(gsub("[dD]", "e", literals)))
  if (anyNA(values)) {
    stop("non-numeric species literal at wdbkwtdata.inc:", start)
  }
  rows[[length(rows) + 1L]] <- data.frame(
    source_file = "wdbkwtdata.inc", source_line_start = start,
    source_line_end = finish, statement_ordinal = seq_along(literals), literal = literals,
    value = values, upstream_commit = nvel_commit, stringsAsFactors = FALSE
  )
}
species_literals <- do.call(rbind, rows)
if (nrow(species_literals) != 2677L * 12L) {
  stop("species table does not contain the declared 2677 by 12 values")
}
species_literals$column <- ((seq_len(nrow(species_literals)) - 1L) %% 12L) + 1L
species_literals$row <- ((seq_len(nrow(species_literals)) - 1L) %/% 12L) + 1L

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(species_literals, file.path(
  "inst", "extdata",
  "nsvb_species_literals.csv"
),
row.names = FALSE, na = ""
)

matrix_values <- matrix(species_literals$value, ncol = 12L, byrow = TRUE)
species_reference <- data.frame(
  spcd = as.integer(matrix_values[, 1L]), hardwood = as.integer(matrix_values[
    ,
    2L
  ]), jenkins_group = as.integer(matrix_values[, 3L]), wood_dry_weight = matrix_values[
    ,
    4L
  ], bark_dry_weight = matrix_values[, 5L], wood_moisture = matrix_values[, 6L],
  bark_moisture = matrix_values[
    ,
    7L
  ], bark_to_wood_volume = matrix_values[, 8L], green_weight_factor = matrix_values[, 9L],
  dry_weight_factor = matrix_values[, 10L], sapling_adjustment = matrix_values[
    ,
    11L
  ], carbon_ratio = matrix_values[
    ,
    12L
  ], source_file = "wdbkwtdata.inc", source_chain = paste(
    "FIADB REF_SPECIES to NBEL BM_REF_SPECIES to NVEL wdbkwtdata.inc"
  ),
  source_last_modified = "2023-10-16", upstream_commit = nvel_commit,
  stringsAsFactors = FALSE
)
utils::write.csv(species_reference, file.path(
  "inst", "extdata",
  "nsvb_species_reference.csv"
),
row.names = FALSE, na = ""
)

fia_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv"
fia_path <- Sys.getenv("FIA_REF_SPECIES", unset = "")
fia_available <- nzchar(fia_path) && file.exists(fia_path)
if (fia_available) {
  fia <- utils::read.csv(fia_path, stringsAsFactors = FALSE, check.names = FALSE)
  names(fia) <- toupper(names(fia))
  required <- c("SPCD", "COMMON_NAME", "GENUS", "SPECIES", "SPECIES_SYMBOL")
  if (!all(required %in% names(fia))) {
    stop("FIA REF_SPECIES is missing required columns")
  }
  fia$SCIENTIFIC <- trimws(paste(fia$GENUS, fia$SPECIES))
  matched <- match(species_reference$spcd, fia$SPCD)
  symbol <- fia$SPECIES_SYMBOL[matched]
  common <- fia$COMMON_NAME[matched]
  scientific <- fia$SCIENTIFIC[matched]
  genus <- fia$GENUS[matched]
  name_source <- paste0("FIA REF_SPECIES: ", fia_url)
} else {
  # TODO: Run data-raw/ref_species.R when the DataMart accepts the download, set
  # FIA_REF_SPECIES to that local CSV, and rerun this script.
  symbol <- common <- scientific <- genus <- rep(NA_character_, nrow(species_reference))
  name_source <- paste0("FIA REF_SPECIES pending: ", fia_url)
}

species_reference <- data.frame(
  spcd = species_reference$spcd, symbol = symbol, common = common,
  scientific = scientific, genus = genus, softwood_hardwood = ifelse(
    species_reference$hardwood ==
      0L, "softwood", "hardwood"
  ), bark_ratio = sqrt(1 / (1 +
                              species_reference$bark_to_wood_volume / 100)),
  wood_density = species_reference$wood_dry_weight, sources = paste(paste0(
    "FIADB REF_SPECIES to NBEL BM_REF_SPECIES to NVEL wdbkwtdata.inc, ",
    "last modified 2023-10-16, NVEL commit ", nvel_commit
  ), name_source, sep = ", "), stringsAsFactors = FALSE
)
dir.create("data", showWarnings = FALSE)
save(species_reference, file = file.path("data", "species_reference.rda"), version = 2L)
# Keep the same generated object in the namespace so exported functions can use it without
# attaching the package data environment first.
if (file.exists(file.path("data", "nsvb_division_polygons.rda"))) {
  load(file.path("data", "nsvb_division_polygons.rda"))
  save(species_reference, nsvb_division_polygons,
    file = file.path("R", "sysdata.rda"), compress = "xz",
    version = 3L
  )
} else {
  save(species_reference,
    file = file.path("R", "sysdata.rda"), compress = "xz",
    version = 3L
  )
}

cpp_number <- function(value) sprintf("%.17g", value)
cpp_rows <- apply(matrix_values, 1L, function(row) {
  paste0("    Species{", paste(vapply(row, cpp_number, character(1)),
           collapse = ", "
         ), "},")
})
header <- c(
  "#ifndef TREEVOLUME_NSVB_SPECIES_HPP", "#define TREEVOLUME_NSVB_SPECIES_HPP", "",
  "// Generated by data-raw/species.R from NVEL commit", paste0(
    "// ", nvel_commit,
    ". Do not edit by hand."
  ),
  "namespace treevolume {", "namespace nsvb_data {", "struct Species {",
  "  double spcd; double hardwood; double group; double wood_dry;",
  "  double bark_dry; double wood_moisture; double bark_moisture;",
  "  double bark_volume; double green; double dry; double sapling;",
  "  double carbon;", "};", "inline constexpr Species species[] = {", cpp_rows,
  "};", "}  // namespace nsvb_data",
  "}  // namespace treevolume", "", "#endif", ""
)
writeLines(header, file.path("inst", "include", "treevolume", "nsvb_species.hpp"))

message(
  "Wrote ", nrow(species_reference), " species rows; FIA names and symbols ",
  if (fia_available) "joined." else "remain TODO because FIA_REF_SPECIES was unavailable."
)
