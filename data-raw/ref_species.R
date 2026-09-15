# Refresh and compare the official FIADB REF_SPECIES table.  This script never overwrites
# the NVEL-derived package values. It maps the official fields and writes a difference
# report for review. The exact field names below are confirmed in the FIADB Database
# Description version 9.2, REF_SPECIES columns 22, 24, 26, 28, and 30.

fia_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv"
local_source <- Sys.getenv("FIA_REF_SPECIES", unset = "")
downloaded <- FALSE
if (!nzchar(local_source)) {
  local_source <- tempfile("REF_SPECIES-", fileext = ".csv")
  download_status <- tryCatch(
    utils::download.file(fia_url, local_source,
      mode = "wb", quiet = FALSE
    ),
    error = function(condition) condition
  )
  if (inherits(download_status, "condition") || !identical(download_status, 0L) ||
        !file.exists(local_source)) {
    stop(
      "FIADB REF_SPECIES could not be downloaded. Set FIA_REF_SPECIES to a ",
      "browser-downloaded REF_SPECIES.csv and rerun this script."
    )
  }
  downloaded <- TRUE
}
if (!file.exists(local_source)) {
  stop("FIA_REF_SPECIES does not name an existing file")
}

fia <- utils::read.csv(local_source, stringsAsFactors = FALSE, check.names = FALSE)
names(fia) <- toupper(names(fia))
required <- c(
  "SPCD", "SPECIES_SYMBOL", "COMMON_NAME", "GENUS", "SPECIES", "WOOD_SPGR_GREENVOL_DRYWT",
  "BARK_SPGR_GREENVOL_DRYWT", "MC_PCT_GREEN_WOOD", "MC_PCT_GREEN_BARK", "BARK_VOL_PCT"
)
missing_columns <- setdiff(required, names(fia))
if (length(missing_columns)) {
  stop("FIADB REF_SPECIES is missing columns: ", paste(missing_columns, collapse = ", "))
}

fia_mapped <- data.frame(
  spcd = as.integer(fia$SPCD), symbol = trimws(fia$SPECIES_SYMBOL), common = trimws(
    fia$COMMON_NAME
  ),
  genus = trimws(fia$GENUS), scientific = trimws(paste(fia$GENUS, fia$SPECIES)),
  wood_dry_weight = as.double(fia$WOOD_SPGR_GREENVOL_DRYWT) *
    62.4, bark_dry_weight = as.double(fia$BARK_SPGR_GREENVOL_DRYWT) * 62.4,
  wood_moisture = as.double(fia$MC_PCT_GREEN_WOOD),
  bark_moisture = as.double(fia$MC_PCT_GREEN_BARK), bark_to_wood_volume = as.double(
    fia$BARK_VOL_PCT
  ),
  stringsAsFactors = FALSE
)
if (anyNA(fia_mapped$spcd) || anyDuplicated(fia_mapped$spcd)) {
  stop("FIADB REF_SPECIES has missing or duplicate species codes")
}

nvel_path <- file.path("inst", "extdata", "nsvb_species_reference.csv")
if (!file.exists(nvel_path)) {
  stop("regenerate data-raw/species.R before comparing REF_SPECIES")
}
nvel <- utils::read.csv(nvel_path, stringsAsFactors = FALSE)
fields <- c(
  "wood_dry_weight", "bark_dry_weight", "wood_moisture", "bark_moisture",
  "bark_to_wood_volume"
)
comparison <- merge(nvel[c("spcd", fields)], fia_mapped[c("spcd", fields)],
  by = "spcd", all = TRUE,
  suffixes = c("_nvel", "_fiadb"), sort = TRUE
)
for (field in fields) {
  comparison[[paste0(field, "_difference")]] <- comparison[[paste0(
    field,
    "_nvel"
  )]] - comparison[[paste0(
    field,
    "_fiadb"
  )]]
}

has_difference <- Reduce(`|`, lapply(fields, function(field) {
  nvel_value <- comparison[[paste0(field, "_nvel")]]
  fiadb_value <- comparison[[paste0(field, "_fiadb")]]
  xor(is.na(nvel_value), is.na(fiadb_value)) | (!is.na(nvel_value) & !is.na(fiadb_value) &
                                                  nvel_value != fiadb_value)
}))
has_unmatched_row <- !comparison$spcd %in% intersect(nvel$spcd, fia_mapped$spcd)
report <- comparison[has_difference | has_unmatched_row, ]
report_path <- Sys.getenv("REF_SPECIES_REPORT", unset = file.path(
  "data-raw",
  "ref_species_differences.csv"
))
utils::write.csv(report, report_path, row.names = FALSE, na = "")

message("FIADB rows: ", nrow(fia_mapped))
message("NVEL-derived rows: ", nrow(nvel))
message("Matched species: ", length(intersect(nvel$spcd, fia_mapped$spcd)))
message("Rows with differences or an unmatched species: ", nrow(report))
message("Difference report: ", normalizePath(report_path))
message(
  "No NVEL-derived package value was overwritten.",
  if (downloaded) " The FIADB file was downloaded for this comparison." else ""
)
