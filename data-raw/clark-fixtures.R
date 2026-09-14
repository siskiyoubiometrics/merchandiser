# Create the compact committed Clark fixtures. The full oracle remains
# outside the package and is sampled by the optional full-fixture tests.

fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)

models <- data.table::fread(file.path("inst", "extdata", "clark_models.csv"))
models[, wood := ifelse(species < 300, "softwood", "hardwood")]
models[, geocode := substr(id, 2L, 2L)]

region8_ids <- models[
  family == "clark_r8" & geocode %in% c("1", "4", "7")
][order(id), head(.SD, 1L), by = .(top_code, geocode, wood)]$id
region8_on_region9 <- models[
  family == "clark_r9" & region == 8 & geocode %in% c("1", "4", "7")
][order(id), head(.SD, 1L), by = .(geocode, wood)]$id
actual_region9 <- models[
  family == "clark_r9" & region == 9
][order(id), head(.SD, 1L), by = .(table_variant, wood)]$id

fixture_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT",
  "UPSHT1", "BA", "SI", "HTUP", "STEMDIB", "CALCDIA_REQUESTED",
  "HT2TOPD_REQUESTED", "ERRFLAG", "CALCDIA_ERRFLAG",
  "HT2TOPD_ERRFLAG", "VOL1", "DIB", "DOB", "STEMHT"
)

read_identifiers <- function(path, ids) {
  predicate <- paste0("$13 == \"", ids, "\"", collapse = " || ")
  command <- paste(
    "gzip -cd", shQuote(path), "| awk -F,",
    shQuote(paste("NR == 1 ||", predicate))
  )
  data.table::fread(
    cmd = command, select = fixture_columns, showProgress = FALSE
  )
}

select_rows <- function(path, ids) {
  values <- read_identifiers(path, ids)
  volume <- values[
    CALL_KIND %in% c("VOLUMELIBRARY", "VOLUMELIBRARY_MRULE") &
      DBHOB == 12 & HTTOT == 80
  ][, head(.SD, 1L), by = .(VOLEQ, CALL_KIND)]
  diameter <- values[
    grepl("^PROFILE_PROBE", CALL_KIND) & CALCDIA_REQUESTED == "Y" &
      DBHOB == 12 & HTTOT == 80 & HTUP %in% c(1, 4.5, 17.3, 40, 79)
  ][, head(.SD, 1L), by = .(VOLEQ, HTUP)]
  inverse <- values[
    grepl("^PROFILE_PROBE", CALL_KIND) & HT2TOPD_REQUESTED == "Y" &
      DBHOB == 12 & HTTOT == 80
  ][order(STEMDIB), head(.SD, 5L), by = VOLEQ]
  exclusions <- values[
    grepl("^PROFILE_PROBE", CALL_KIND) & HT2TOPD_REQUESTED == "Y" &
      (!is.finite(STEMHT) | STEMHT < 1)
  ][, head(.SD, 1L), by = .(VOLEQ, is.finite(STEMHT))]
  result <- unique(data.table::rbindlist(
    list(volume, diameter, inverse, exclusions), use.names = TRUE, fill = TRUE
  ), by = "ROW_ID")
  data.table::setorder(result, VOLEQ, ROW_ID)
  result
}

destination <- file.path("tests", "testthat", "fixtures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
for (precision in c("single", "double")) {
  for (family in c("clark_r8", "clark_r9")) {
    ids <- if (identical(family, "clark_r8")) {
      region8_ids
    } else {
      c(region8_on_region9, actual_region9)
    }
    input <- file.path(
      fixture_root, "full", paste0(family, ".", precision, ".csv.gz")
    )
    output <- file.path(
      destination, paste0(family, ".", precision, ".csv.gz")
    )
    message("Writing ", basename(output))
    data.table::fwrite(select_rows(input, ids), output, compress = "gzip")
  }
}

clark_files <- list.files(
  destination, pattern = "^clark_.*[.]csv[.]gz$", full.names = TRUE
)
stopifnot(sum(file.info(clark_files)$size) < 400000L)
