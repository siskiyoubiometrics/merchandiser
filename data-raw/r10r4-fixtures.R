# Create the committed Region 10 and Region 4 fixture sample. The complete
# corpora remain external and are exercised when MERCHANDISER_FIXTURES is set.

fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)
destination <- file.path("tests", "testthat", "fixtures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)

select_rows <- function(values, family) {
  profile <- values$CALL_KIND == "PROFILE_PROBE" &
    values$DBHOB == 12 & values$HTTOT == 80 &
    (
      values$HTUP %in% c(0, 1, 4.5, 40) |
        (values$HT2TOPD_REQUESTED == "Y" &
           values$STEMDIB %in% c(2, 6, 10))
    )
  volume <- values$CALL_KIND %in% c(
    "VOLUMELIBRARY", "VOLUMELIBRARY_MRULE"
  ) & (
    (values$DBHOB == 12 & values$HTTOT == 80) |
      (values$DBHOB == 2 & values$HTTOT == 15) |
      (values$DBHOB == 4 & values$HTTOT == 30)
  )
  anomaly <- if (identical(family, "r10_taper")) {
    values$VOLEQ == "A16BRUW042" & values$CALL_KIND == "VOLUMELIBRARY" &
      values$DBHOB == 10 & values$HTTOT == 80
  } else {
    values$CALL_KIND == "PROFILE_PROBE" & values$HT2TOPD_REQUESTED == "Y" &
      (values$STEMHT == 0 |
         (values$VOLEQ == "405MATW202" &
            ((values$DBHOB == 4 & values$HTTOT == 100 & values$STEMDIB == 2) |
               (values$DBHOB == 6 & values$HTTOT == 160 &
                  values$STEMDIB == 2))))
  }
  result <- values[profile | volume | anomaly, , drop = FALSE]
  result[order(result$VOLEQ, result$ROW_ID), , drop = FALSE]
}

written <- character()
for (precision in c("single", "double")) {
  for (family in c("r10_taper", "r4_driver")) {
    input <- file.path(
      fixture_root, "full", paste0(family, ".", precision, ".csv.gz")
    )
    values <- utils::read.csv(input, stringsAsFactors = FALSE,
                              check.names = FALSE)
    selected <- select_rows(values, family)
    output <- file.path(
      destination, paste0(family, ".", precision, ".csv.gz")
    )
    connection <- gzfile(output, open = "wt", compression = 9)
    utils::write.csv(selected, connection, row.names = FALSE, na = "")
    close(connection)
    written <- c(written, output)
  }
}

sizes <- file.info(written)$size
stopifnot(sum(sizes) < 300000L)
message("wrote ", length(written), " fixture files (", sum(sizes), " bytes)")
