# Create the small, committed NSVB fixture sample used in routine tests.  The complete
# protected oracle remains outside the package and is exercised when MERCHANDISER_FIXTURES
# is set.

fixture_root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = file.path(
  "..", "merchandiser", "tools",
  "oracle", "fixtures"
))

nsvb_columns <- c(
  "ROW_ID", "FIXTURE_SET", "CALL_KIND", "REGN", "FORST", "DIST", "FIASPCD", "PROD",
  "CTYPE", "LIVE", "VOLEQ", "DBHOB", "HTTOT", "MTOPP", "MTOPS", "STUMP", "CULL", "DECAYCD",
  "MRULEMOD", "NEWMAXLEN", "NEWMINLEN", "NEWTRIM", "ERRFLAG", "HTUP", "STEMDIB",
  "CALCDIA_REQUESTED",
  "HT2TOPD_REQUESTED", "CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG", "DIB", "STEMHT", paste0(
    "VOL",
    1:15
  ), "NOLOGP", "NOLOGS", paste0("DRYBIO", 1:15), paste0("GRNBIO", 1:15), paste0(
    "BOLHT",
    1:21
  )
)

read_selected <- function(path) {
  available <- names(data.table::fread(path, nrows = 0L))
  values <- data.table::fread(path, select = intersect(nsvb_columns, available))
  for (name in setdiff(nsvb_columns, names(values))) {
    values[[name]] <- NA
  }
  data.table::setcolorder(values, nsvb_columns)
  values
}

select_main <- function(path) {
  values <- read_selected(path)
  values <- values[DBHOB == 12 & HTTOT == 80 & (grepl("^NVBC", CALL_KIND) | grepl(
    "^VOLUMELIBRARY",
    CALL_KIND
  ) | (grepl("^PROFILE", CALL_KIND) & HTUP %in% c(1, 4.5, 40)))]
  data.table::setorder(values, VOLEQ, ROW_ID)
  values
}

destination <- file.path("tests", "testthat", "fixtures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
for (precision in c("single", "double")) {
  input <- file.path(fixture_root, "full", paste0("nsvb.", precision, ".csv.gz"))
  output <- file.path(destination, paste0("nsvb.", precision, ".csv.gz"))
  data.table::fwrite(select_main(input), output, compress = "gzip")
}

anomaly_input <- file.path(
  fixture_root, "anomalies",
  "nsvb_210_230_representative.double.csv"
)
anomaly_output <- file.path(destination, "nsvb_210_230.double.csv.gz")
data.table::fwrite(read_selected(anomaly_input), anomaly_output, compress = "gzip")

outputs <- file.path(destination, c(
  "nsvb.single.csv.gz", "nsvb.double.csv.gz",
  "nsvb_210_230.double.csv.gz"
))
stopifnot(sum(file.info(outputs)$size) < 3e+05)
