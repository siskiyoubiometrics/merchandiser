# Create the small, committed Flewelling fixture sample used in routine tests.
# The complete oracle remains outside the package and is exercised separately
# when MERCHANDISER_FIXTURES is set.

fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)

two_point_ids <- c(
  "F00FW2W202", "F00FW2W263", "F00FW2W242",
  "F00FW2W260",
  "I00FW2W202", "I00FW2W073", "I00FW2W017", "I00FW2W122",
  "I00FW2W108", "I00FW2W242", "I00FW2W260", "I00FW2W119",
  "I00FW2W093", "I00FW2W019", "I00FW2W012",
  "203FW2W122", "200FW2W122", "407FW2W093", "200FW2W108",
  "200FW2W202", "200FW2W015", "200FW2W746", "300FW2W122",
  "A00FW2W042", "A00FW2W242", "A00FW2W098", "A00FW2W260",
  "A02FW2W098", "A02FW2W260"
)

three_point_ids <- c(
  "A00FW3W042", "A00F33W242", "A00FW3W098", "A00F33W260",
  "A02FW3W098", "A02F33W260", "A00F32W098"
)

select_rows <- function(path, ids) {
  values <- data.table::fread(path)
  values <- values[
    VOLEQ %in% ids & DBHOB == 12 & HTTOT == 80 &
      (
        CALL_KIND %in% c("VOLUMELIBRARY", "VOLUMELIBRARY_MRULE") |
          (CALL_KIND == "PROFILE_PROBE" & HTUP %in% c(1, 4.5, 8, 40))
      )
  ]
  data.table::setorder(values, VOLEQ, ROW_ID)
  values
}

destination <- file.path("tests", "testthat", "fixtures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
for (precision in c("single", "double")) {
  for (family in c("flewelling_2pt", "flewelling_3pt")) {
    ids <- if (identical(family, "flewelling_2pt")) {
      two_point_ids
    } else {
      three_point_ids
    }
    input <- file.path(fixture_root, "full", paste0(family, ".", precision, ".csv.gz"))
    output <- file.path(destination, paste0(family, ".", precision, ".csv.gz"))
    data.table::fwrite(select_rows(input, ids), output, compress = "gzip")
  }
}

sizes <- file.info(list.files(destination, full.names = TRUE))$size
stopifnot(sum(sizes) < 1e6)
