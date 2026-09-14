# Create the committed regional small-taper oracle sample. The complete
# fixtures remain outside the package and are tested through
# MERCHANDISER_FIXTURES.

fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)
families <- c(
  "r1_taper", "r2_taper", "r5_taper", "r12_taper", "blm_taper",
  "behre_taper"
)

select_rows <- function(path) {
  values <- data.table::fread(path)
  values <- values[
    DBHOB == 12 & HTTOT == 80 &
      (
        CALL_KIND %in% c("VOLUMELIBRARY", "VOLUMELIBRARY_MRULE") |
          (CALL_KIND == "PROFILE_PROBE" & CALCDIA_REQUESTED == "Y" &
             HTUP %in% c(1, 4.5, 40)) |
          (CALL_KIND == "PROFILE_PROBE" & HT2TOPD_REQUESTED == "Y" &
             STEMDIB %in% c(2, 6, 10))
      )
  ]
  data.table::setorder(values, VOLEQ, ROW_ID)
  values
}

destination <- file.path("tests", "testthat", "fixtures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
written <- character()
for (precision in c("single", "double")) {
  for (family in families) {
    input <- file.path(
      fixture_root, "full", paste0(family, ".", precision, ".csv.gz")
    )
    output <- file.path(destination, paste0(family, ".", precision, ".csv.gz"))
    data.table::fwrite(select_rows(input), output, compress = "gzip")
    written <- c(written, output)
  }
}

stopifnot(sum(file.info(written)$size) < 400 * 1024)
