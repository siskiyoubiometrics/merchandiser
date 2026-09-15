# Regenerate NSVB coefficient, division, regional-weight, and model tables.  Run from the
# package root. Set NVEL_SOURCE and MERCHANDISER_FIXTURES to override the read-only
# reference checkouts. Every stored literal carries its source file, source line range, and
# pinned upstream commit. The generated C++ header is the immutable run-time representation
# used by the compiled kernel.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv("NVEL_SOURCE", unset = file.path(
  "..", "..", "tools",
  "oracle", "nvel"
))
fixture_root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = file.path(
  "..", "merchandiser", "tools",
  "oracle", "fixtures"
))

source_files <- c(paste0("tables", 1:11, ".inc"), "dist_ecoprov.inc", "regndftdata.inc")
reference_files <- c(source_files, "scrib.f")
missing_files <- reference_files[!file.exists(file.path(nvel_source, reference_files))]
if (length(missing_files)) {
  stop("missing NVEL source files: ", paste(missing_files, collapse = ", "))
}

actual_commit <- system2("git", c(
  "-C", normalizePath(nvel_source), "rev-parse",
  "HEAD"
), stdout = TRUE)
if (!identical(actual_commit, nvel_commit)) {
  stop("NVEL checkout is not at the pinned commit: ", actual_commit)
}

is_comment <- function(line) {
  nzchar(line) && substr(line, 1L, 1L) %in% c("c", "C", "*", "!")
}

parse_numeric_data <- function(source_file, targets) {
  lines <- readLines(file.path(nvel_source, source_file), warn = FALSE)
  rows <- list()
  line_number <- 1L
  while (line_number <= length(lines)) {
    line <- lines[[line_number]]
    if (is_comment(line) || !grepl("^[ ]{0,7}DATA[ ]*\\(\\(", line, ignore.case = TRUE)) {
      line_number <- line_number + 1L
      next
    }
    start <- line_number
    statement_lines <- line
    slash_count <- lengths(regmatches(line, gregexpr("/", line, fixed = TRUE)))
    while (slash_count < 2L && line_number < length(lines)) {
      line_number <- line_number + 1L
      continuation <- lines[[line_number]]
      statement_lines <- c(statement_lines, continuation)
      slash_count <- slash_count + lengths(regmatches(continuation, gregexpr("/",
                                             continuation,
                                             fixed = TRUE
                                           )))
    }
    statement <- paste(substring(statement_lines, 7L), collapse = " ")
    matched <- regexec("^[ ]*DATA[ ]*\\(\\(([A-Za-z0-9_]+)\\([^/]*/(.*)/[ ]*$", statement,
      ignore.case = TRUE
    )
    parts <- regmatches(statement, matched)[[1L]]
    if (!length(parts)) {
      stop(source_file, ":", start, ": could not parse DATA statement")
    }
    target <- parts[[2L]]
    if (tolower(target) %in% tolower(targets)) {
      literals <- trimws(strsplit(parts[[3L]], ",", fixed = TRUE)[[1L]])
      literals <- literals[nzchar(literals)]
      values <- suppressWarnings(as.double(gsub("[dD]", "e", literals)))
      if (anyNA(values)) {
        stop(source_file, ":", start, ": non-numeric literal in ", target)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        source_file = source_file, source_line_start = start,
        source_line_end = line_number, target = target,
        statement_ordinal = seq_along(literals),
        literal = literals, value = values, upstream_commit = nvel_commit,
        stringsAsFactors = FALSE
      )
    }
    line_number <- line_number + 1L
  }
  if (!length(rows)) {
    stop("no numeric DATA statements found in ", source_file)
  }
  do.call(rbind, rows)
}

specification <- list(
  tables1.inc = c(SPcoef = 13L, JKcoef = 5L), tables2.inc = c(
    SPcoef = 13L,
    JKcoef = 5L
  ), tables3.inc = c(SPcoef = 13L, JKcoef = 5L), tables4.inc = c(SPcoef = 13L, JKcoef = 5L),
  tables5.inc = c(SPcoef = 13L, JKcoef = 5L), tables6.inc = c(
    SPcoef = 13L,
    JKcoef = 5L
  ), tables7.inc = c(
    SPcoef = 13L,
    JKcoef = 5L
  ), tables8.inc = c(SPcoef = 13L, JKcoef = 5L), tables9.inc = c(
    SPcoef = 13L,
    JKcoef = 5L
  ), tables10.inc = c(SPCF = 2L), tables11.inc = c(DIVCRh = 2L, DIVCRs = 2L),
  dist_ecoprov.inc = c(DistProv = 2L, ForstProv = 2L, RegnProv = 2L),
  regndftdata.inc = c(SPREGNDFTWF = 7L)
)

literal_rows <- lapply(names(specification), function(source_file) {
  values <- parse_numeric_data(source_file, names(specification[[source_file]]))
  target_lower <- tolower(values$target)
  column_count <- unname(specification[[source_file]][match(target_lower, tolower(
    names(specification[[source_file]])
  ))])
  values$column <- ave(values$statement_ordinal, target_lower, FUN = function(x) {
    ((seq_along(x) - 1L) %% column_count[[1L]]) + 1L
  })
  values
})
nsvb_coefficients <- do.call(rbind, literal_rows)
rownames(nsvb_coefficients) <- NULL

expected <- c(
  `tables1.inc:spcoef` = 406L * 13L, `tables1.inc:jkcoef` = 9L * 5L,
  `tables2.inc:spcoef` = 339L *
    13L, `tables2.inc:jkcoef` = 9L * 5L, `tables3.inc:spcoef` = 361L * 13L,
  `tables3.inc:jkcoef` = 9L *
    5L, `tables4.inc:spcoef` = 361L * 13L, `tables4.inc:jkcoef` = 9L * 5L,
  `tables5.inc:spcoef` = 405L *
    13L, `tables5.inc:jkcoef` = 9L * 5L, `tables6.inc:spcoef` = 206L * 13L,
  `tables6.inc:jkcoef` = 9L *
    5L, `tables7.inc:spcoef` = 175L * 13L, `tables7.inc:jkcoef` = 9L * 5L,
  `tables8.inc:spcoef` = 173L *
    13L, `tables8.inc:jkcoef` = 9L * 5L, `tables9.inc:spcoef` = 201L * 13L,
  `tables9.inc:jkcoef` = 9L *
    5L, `tables10.inc:spcf` = 460L * 2L, `tables11.inc:divcrh` = 43L * 2L,
  `tables11.inc:divcrs` = 43L *
    2L, `dist_ecoprov.inc:distprov` = 505L * 2L, `dist_ecoprov.inc:forstprov` = 108L *
    2L, `dist_ecoprov.inc:regnprov` = 9L *
    2L, `regndftdata.inc:spregndftwf` = 149L * 7L
)
keys <- paste(nsvb_coefficients$source_file, tolower(nsvb_coefficients$target), sep = ":")
actual <- table(factor(keys, levels = names(expected)))
if (!identical(as.integer(actual), as.integer(expected))) {
  stop("extracted NSVB table dimensions do not match their declarations")
}

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(nsvb_coefficients, file.path("inst", "extdata", "nsvb_coefficients.csv"),
  row.names = FALSE,
  na = ""
)

# NVBC delegates its two board-foot calculations to SCRIB and INTL14. Preserve SCRIB's
# factor and correction tables separately so their provenance is as explicit as the NSVB
# supplement tables without changing the supplement-row invariant above.
parse_scrib_data <- function(target, expected_count) {
  source_file <- "scrib.f"
  lines <- readLines(file.path(nvel_source, source_file), warn = FALSE)
  starts <- grep(paste0("^[ ]{0,7}DATA[ ]*\\(", target, "\\(I\\)"), lines,
    ignore.case = TRUE
  )
  rows <- lapply(starts, function(start) {
    finish <- start
    statement_lines <- lines[[finish]]
    slash_count <- lengths(regmatches(statement_lines, gregexpr("/",
                             statement_lines,
                             fixed = TRUE
                           )))
    while (slash_count < 2L && finish < length(lines)) {
      finish <- finish + 1L
      statement_lines <- c(statement_lines, lines[[finish]])
      slash_count <- slash_count + lengths(regmatches(lines[[finish]], gregexpr("/",
                                             lines[[finish]],
                                             fixed = TRUE
                                           )))
    }
    statement <- paste(substring(statement_lines, 7L), collapse = " ")
    literal_text <- sub("^[^/]*/", "", statement)
    literal_text <- sub("/[^/]*$", "", literal_text)
    literals <- trimws(strsplit(literal_text, ",", fixed = TRUE)[[1L]])
    literals <- literals[nzchar(literals)]
    values <- suppressWarnings(as.double(gsub("[dD]", "e", literals)))
    if (anyNA(values)) {
      stop(source_file, ":", start, ": non-numeric literal in ", target)
    }
    data.frame(
      source_file = source_file, source_line_start = start, source_line_end = finish,
      target = target, statement_ordinal = seq_along(literals), literal = literals,
      value = values,
      upstream_commit = nvel_commit, stringsAsFactors = FALSE
    )
  })
  values <- do.call(rbind, rows)
  if (nrow(values) != expected_count) {
    stop("unexpected ", target, " count in scrib.f: ", nrow(values))
  }
  values
}

scribner_coefficients <- rbind(parse_scrib_data("FACTOR", 132L), parse_scrib_data(
  "EXCEPT", 149L
))
rownames(scribner_coefficients) <- NULL
utils::write.csv(scribner_coefficients, file.path(
  "inst", "extdata",
  "nsvb_scribner_coefficients.csv"
),
row.names = FALSE, na = ""
)

scribner_rows <- function(target) {
  values <- scribner_coefficients$value[tolower(
    scribner_coefficients$target
  ) == tolower(target)]
  paste0("    ", vapply(values, function(value) sprintf("%.17g", value), character(1)), ",")
}
scribner_header <- c(
  "#ifndef TREEVOLUME_NSVB_SCRIBNER_HPP", "#define TREEVOLUME_NSVB_SCRIBNER_HPP",
  "", "// Generated by data-raw/nsvb.R from NVEL scrib.f at commit", paste0(
    "// ", nvel_commit,
    ". Do not edit by hand."
  ), "namespace treevolume {", "namespace nsvb_data {",
  "inline constexpr double scribner_factor[] = {",
  scribner_rows("FACTOR"), "};", "inline constexpr double scribner_exception[] = {",
  scribner_rows("EXCEPT"),
  "};", "}  // namespace nsvb_data", "}  // namespace treevolume", "", "#endif", ""
)
writeLines(scribner_header, file.path(
  "inst", "include", "merchandiser",
  "nsvb_scribner.hpp"
))

target_matrix <- function(source_file, target, columns) {
  selected <- nsvb_coefficients$source_file == source_file & tolower(
    nsvb_coefficients$target
  ) ==
    tolower(target)
  matrix(nsvb_coefficients$value[selected], ncol = columns, byrow = TRUE)
}

cpp_number <- function(value) sprintf("%.17g", value)
cpp_rows <- function(matrix_value, struct_name) {
  apply(matrix_value, 1L, function(row) {
    paste0(
      "    ", struct_name, "{", paste(vapply(row, cpp_number, character(1)),
        collapse = ", "
      ),
      "},"
    )
  })
}

header <- c(
  "#ifndef TREEVOLUME_NSVB_COEFFICIENTS_HPP", "#define TREEVOLUME_NSVB_COEFFICIENTS_HPP",
  "", "#include <cstddef>", "", "// Generated by data-raw/nsvb.R from NVEL commit", paste0(
    "// ",
    nvel_commit, ". Do not edit by hand."
  ), "namespace treevolume {", "namespace nsvb_data {",
  "", "struct EquationCoefficient {",
  "  double spcd; double division; double stdorg; double equation;",
  "  double a; double a0; double a1; double b; double b0; double b1;",
  "  double b2; double c; double c1;",
  "};",
  "struct GroupCoefficient { double group; double equation; double a; double b; double c; };",
  "struct Pair { double key; double value; };", "struct RegionWeight {",
  "  double region; double forest; double spcd; double primary;",
  "  double secondary; double unused; double dead;", "};", ""
)
for (table_number in 1:9) {
  source_file <- paste0("tables", table_number, ".inc")
  table_name <- paste0("table", table_number)
  spcoef <- target_matrix(source_file, "SPcoef", 13L)
  jkcoef <- target_matrix(source_file, "JKcoef", 5L)
  header <- c(
    header, paste0("inline constexpr EquationCoefficient ", table_name, "[] = {"),
    cpp_rows(spcoef, "EquationCoefficient"), "};", paste0(
      "inline constexpr GroupCoefficient ",
      table_name, "_group[] = {"
    ), cpp_rows(jkcoef, "GroupCoefficient"), "};", ""
  )
}
pair_tables <- list(
  carbon = target_matrix("tables10.inc", "SPCF", 2L),
  crown_hardwood = target_matrix(
    "tables11.inc",
    "DIVCRh", 2L
  ), crown_softwood = target_matrix("tables11.inc", "DIVCRs", 2L),
  district_province = target_matrix(
    "dist_ecoprov.inc",
    "DistProv", 2L
  ), forest_province = target_matrix("dist_ecoprov.inc", "ForstProv", 2L),
  region_province = target_matrix(
    "dist_ecoprov.inc",
    "RegnProv", 2L
  )
)
for (name in names(pair_tables)) {
  header <- c(header, paste0("inline constexpr Pair ", name, "[] = {"), cpp_rows(
    pair_tables[[name]],
    "Pair"
  ), "};", "")
}
region_weight <- target_matrix("regndftdata.inc", "SPREGNDFTWF", 7L)
header <- c(
  header, "inline constexpr RegionWeight region_weight[] = {", cpp_rows(
    region_weight,
    "RegionWeight"
  ), "};", "", "template <typename T, std::size_t N>",
  "constexpr std::size_t countof(const T (&)[N]) { return N; }",
  "", "}  // namespace nsvb_data", "}  // namespace treevolume", "", "#endif", ""
)
writeLines(header, file.path("inst", "include", "merchandiser", "nsvb_coefficients.hpp"))

division_rows <- do.call(rbind, Map(function(name, values) {
  data.frame(
    level = name, key = as.integer(values[, 1L]), division = as.integer(values[, 2L]),
    source_file = "dist_ecoprov.inc", upstream_commit = nvel_commit,
    stringsAsFactors = FALSE
  )
}, c("district", "forest", "region"), pair_tables[c(
  "district_province",
  "forest_province", "region_province"
)]))
utils::write.csv(division_rows, file.path("inst", "extdata", "nsvb_divisions.csv"),
  row.names = FALSE,
  na = ""
)

# SPEC_INTERFACE.md defines the public division helper in terms of State and county FIPS
# codes. NVEL itself contains only the National Forest district crosswalk above, so the
# public lookup is extracted from the Forest Service's county ecological-subregion table.
# The PDF is checksum-pinned and is never fetched during a package build.
ecocode_url <- "https://www.srs.fs.usda.gov/pubs/gtr/gtr_srs036.pdf"
ecocode_sha256 <- "a84f6596f623ad1efc148730e666c748056bb5430b3d925d21b61527feeca1e8"
ecocode_pdf <- Sys.getenv("ECOCODE_PDF", unset = "")
if (!nzchar(ecocode_pdf) || !file.exists(ecocode_pdf)) {
  stop("set ECOCODE_PDF to the official gtr_srs036.pdf downloaded from ", ecocode_url)
}
checksum <- strsplit(system2("sha256sum", ecocode_pdf, stdout = TRUE), " ")[[1L]][1L]
if (!identical(checksum, ecocode_sha256)) {
  stop("ECOCODE_PDF checksum does not match the pinned Forest Service report")
}
ecocode_text <- tempfile(fileext = ".txt")
on.exit(unlink(ecocode_text), add = TRUE)
status <- system2("pdftotext", c("-layout", ecocode_pdf, ecocode_text))
if (!identical(status, 0L)) {
  stop("pdftotext failed while extracting the county division table")
}
ecocode_lines <- readLines(ecocode_text, warn = FALSE)
ecocode_pages <- 1L + cumsum(grepl("\f", ecocode_lines, fixed = TRUE))
county_pattern <- paste0(
  "^\\s*(?:([A-Za-z][A-Za-z .&()'-]+?)\\s+)?([0-9]{2})\\s+",
  "(.+?)\\s+([0-9]{4,5})\\s+([0-9]{6})\\s+([12])\\s+",
  "([0-9]{3})\\s+([0-9]+)\\s+([0-9]+[.]?[0-9]*)\\s*$"
)
matches <- regmatches(ecocode_lines, regexec(county_pattern, ecocode_lines, perl = TRUE))
matched <- lengths(matches) > 0L
county_values <- do.call(rbind, lapply(matches[matched], function(value) value[-1L]))
if (nrow(county_values) != 3104L) {
  stop("unexpected county ecological-subregion row count: ", nrow(county_values))
}
fips <- sprintf("%05d", as.integer(county_values[, 4L]))
nsvb_county_divisions <- data.frame(
  state = as.integer(substr(fips, 1L, 2L)), county = as.integer(substr(
    fips,
    3L, 5L
  )), division = as.integer(county_values[, 7L]) + ifelse(county_values[, 6L] == "2",
    1000L, 0L
  ), mountain = as.integer(county_values[, 6L]), province = as.integer(county_values[
    ,
    7L
  ]), section = as.integer(county_values[, 8L]), subregion_code = as.integer(county_values[
    ,
    5L
  ]), predominant_area_percent = as.double(county_values[, 9L]),
  source_page = ecocode_pages[matched],
  stringsAsFactors = FALSE
)
if (anyDuplicated(paste(nsvb_county_divisions$state, nsvb_county_divisions$county))) {
  stop("county ecological-subregion table contains duplicate FIPS keys")
}
utils::write.csv(nsvb_county_divisions, file.path(
  "inst", "extdata",
  "nsvb_county_divisions.csv"
),
row.names = FALSE, na = ""
)
utils::write.csv(data.frame(
  source_file = "gtr_srs036.pdf", source_url = ecocode_url,
  source_table = "Table 1, PDF pages 21 through 94",
  sha256 = ecocode_sha256, rows = nrow(nsvb_county_divisions), stringsAsFactors = FALSE
), file.path(
  "inst",
  "extdata", "nsvb_county_divisions_provenance.csv"
), row.names = FALSE)

read_columns <- function(path, columns) {
  connection <- if (grepl("[.]gz$", path))
    gzfile(path, open = "rt") else file(path)
  on.exit(close(connection), add = TRUE)
  header_line <- readLines(connection, n = 1L)
  header_names <- strsplit(header_line, ",", fixed = TRUE)[[1L]]
  classes <- rep("NULL", length(header_names))
  classes[header_names %in% columns] <- "character"
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}

full_path <- file.path(fixture_root, "full", "nsvb.double.csv.gz")
anomaly_path <- file.path(
  fixture_root, "anomalies",
  "nsvb_210_230_representative.double.csv"
)
if (!file.exists(full_path) || !file.exists(anomaly_path)) {
  stop("full and division-210/230 double NSVB fixtures are required for model metadata")
}
full_models <- read_columns(full_path, c("VOLEQ", "FIASPCD"))
anomaly_models <- read_columns(anomaly_path, c("VOLEQ", "FIASPCD"))
nsvb_models <- unique(rbind(full_models, anomaly_models))
nsvb_models <- nsvb_models[grepl("^NVB[0M][0-9]{6}$", nsvb_models$VOLEQ), ]
nsvb_models <- nsvb_models[order(nsvb_models$VOLEQ), ]
nsvb_models <- nsvb_models[c("VOLEQ", "FIASPCD")]
names(nsvb_models) <- c("id", "species")
nsvb_models$species <- as.integer(nsvb_models$species)
nsvb_models$family <- "nsvb"
nsvb_models$source_file <- ifelse(substr(nsvb_models$id, 5L, 7L) %in% c(
  "210",
  "230"
), "MERCHANDISER_FIXTURES/anomalies/nsvb_210_230_representative.double.csv",
"MERCHANDISER_FIXTURES/full/nsvb.double.csv.gz"
)
nsvb_models$upstream_commit <- nvel_commit
rownames(nsvb_models) <- NULL
utils::write.csv(nsvb_models, file.path("inst", "extdata", "nsvb_models.csv"),
  row.names = FALSE,
  na = ""
)

message(
  "Wrote ", nrow(nsvb_coefficients), " NSVB numeric literals and metadata for ",
  nrow(nsvb_models),
  " models."
)
