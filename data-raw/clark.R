# Regenerate the Clark coefficient inventory, compile-time tables, model
# metadata, and pattern metadata.
#
# Run from the package root. NVEL_SOURCE and MERCHANDISER_FIXTURES may override
# the read-only upstream checkouts. The installed package uses only the CSV
# metadata and generated C++ header.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv(
  "NVEL_SOURCE",
  unset = file.path("..", "..", "tools", "oracle", "nvel")
)
fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)
identifier_root <- Sys.getenv(
  "TREEVOLUME_IDENTIFIERS",
  unset = file.path("..", "merchandiser", "tools", "identifiers")
)

source_files <- c(
  "r8clkdib.f", "r8dib.f", "r8dib.inc", "r8clkcoef.inc", "r8cfo.inc",
  "r8clist.inc", "r8vlist.f", "r8vlist.inc", "r8init.f", "r8prep.f",
  "r8vol.f", "r8vol1.f", "r8vol2.f", "r9clarkdib.f", "r9clark.f",
  "r9coeff.inc", "r9init.f", "r9vol.f", "r9logs.f", "clkcoef_mod.f",
  "calcdia.f", "ht2topd.f", "profile.f", "volinit.f", "voleqdef.f"
)

missing_files <- source_files[!file.exists(file.path(nvel_source, source_files))]
if (length(missing_files)) {
  stop("missing NVEL source files: ", paste(missing_files, collapse = ", "))
}

actual_commit <- system2(
  "git", c("-C", normalizePath(nvel_source), "rev-parse", "HEAD"),
  stdout = TRUE
)
if (!identical(actual_commit, nvel_commit)) {
  stop("NVEL checkout is not at the pinned commit: ", actual_commit)
}

is_comment <- function(line) {
  nzchar(line) && substr(line, 1L, 1L) %in% c("c", "C", "*", "!")
}

routine_at <- function(lines, line_number) {
  preceding <- lines[seq_len(line_number)]
  declarations <- grep(
    "^[ ]{0,6}(subroutine|([a-z0-9*]+[ ]+)?function)[ ]+",
    preceding,
    ignore.case = TRUE,
    value = TRUE
  )
  if (!length(declarations)) {
    return(NA_character_)
  }
  declaration <- tail(declarations, 1L)
  sub(
    "^.*(?:subroutine|function)[ ]+([a-z0-9_]+).*$", "\\1",
    declaration,
    ignore.case = TRUE
  )
}

split_fortran_values <- function(value_text) {
  characters <- strsplit(value_text, "", fixed = TRUE)[[1L]]
  values <- character()
  current <- character()
  quote <- ""
  for (character in characters) {
    if (character %in% c("'", "\"") &&
          (!nzchar(quote) || quote == character)) {
      quote <- if (nzchar(quote)) "" else character
      current <- c(current, character)
    } else if (character == "," && !nzchar(quote)) {
      values <- c(values, paste0(current, collapse = ""))
      current <- character()
    } else {
      current <- c(current, character)
    }
  }
  values <- c(values, paste0(current, collapse = ""))
  trimws(values[nzchar(trimws(values))])
}

expand_repetition <- function(values) {
  result <- character()
  for (value in values) {
    repeated <- regexec("^([0-9]+)[*](.+)$", value)
    parts <- regmatches(value, repeated)[[1L]]
    if (length(parts)) {
      result <- c(result, rep(trimws(parts[[3L]]), as.integer(parts[[2L]])))
    } else {
      result <- c(result, value)
    }
  }
  result
}

parse_literal <- function(literal) {
  if (grepl("^(['\"]).*\\1$", literal)) {
    return(list(
      type = "character", numeric = NA_real_,
      character = substring(literal, 2L, nchar(literal) - 1L)
    ))
  }
  if (tolower(literal) %in% c(".true.", ".false.")) {
    return(list(
      type = "logical", numeric = NA_real_, character = tolower(literal)
    ))
  }
  candidate <- gsub("[dD]", "e", literal)
  numeric <- suppressWarnings(as.double(candidate))
  if (!is.na(numeric)) {
    return(list(type = "numeric", numeric = numeric, character = NA_character_))
  }
  list(type = "expression", numeric = NA_real_, character = literal)
}

parse_file <- function(file) {
  lines <- readLines(file.path(nvel_source, file), warn = FALSE)
  rows <- list()
  line_number <- 1L
  while (line_number <= length(lines)) {
    line <- lines[[line_number]]
    if (is_comment(line) ||
          !grepl("^[ ]{0,6}data(?:[ ]|[(])", line,
            ignore.case = TRUE, perl = TRUE
          )) {
      line_number <- line_number + 1L
      next
    }
    start <- line_number
    statement_lines <- line
    slash_count <- lengths(regmatches(line, gregexpr("/", line, fixed = TRUE)))
    while (slash_count < 2L && line_number < length(lines)) {
      line_number <- line_number + 1L
      continuation <- lines[[line_number]]
      if (is_comment(continuation)) {
        next
      }
      statement_lines <- c(statement_lines, continuation)
      slash_count <- slash_count +
        lengths(regmatches(continuation, gregexpr("/", continuation, fixed = TRUE)))
    }
    statement <- paste(sub("^.{0,6}", "", statement_lines), collapse = " ")
    matched <- regexec(
      "^[ ]*data[ ]*([^/]+)/(.+)/[ ]*$", statement,
      ignore.case = TRUE
    )
    parts <- regmatches(statement, matched)[[1L]]
    if (!length(parts)) {
      stop(file, ":", start, ": could not parse DATA statement")
    }
    target <- trimws(parts[[2L]])
    literals <- expand_repetition(split_fortran_values(parts[[3L]]))
    parsed <- lapply(literals, parse_literal)
    rows[[length(rows) + 1L]] <- data.frame(
      source_file = file,
      source_line_start = start,
      source_line_end = line_number,
      routine = routine_at(lines, start),
      target = target,
      ordinal = seq_along(literals),
      literal = literals,
      value_type = vapply(parsed, `[[`, character(1), "type"),
      value_numeric = vapply(parsed, `[[`, double(1), "numeric"),
      value_character = vapply(parsed, `[[`, character(1), "character"),
      upstream_commit = nvel_commit,
      stringsAsFactors = FALSE
    )
    line_number <- line_number + 1L
  }
  if (!length(rows)) {
    return(NULL)
  }
  do.call(rbind, rows)
}

clark_coefficients <- do.call(rbind, lapply(source_files, parse_file))
rownames(clark_coefficients) <- NULL

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  clark_coefficients,
  file.path("inst", "extdata", "clark_coefficients.csv"),
  row.names = FALSE,
  na = ""
)
coefficient_round_trip <- utils::read.csv(
  file.path("inst", "extdata", "clark_coefficients.csv"),
  stringsAsFactors = FALSE, na.strings = ""
)
if (!isTRUE(all.equal(
  clark_coefficients, coefficient_round_trip,
  check.attributes = FALSE, tolerance = 0
))) {
  stop("Clark coefficient inventory did not round trip through CSV")
}

cpp_number <- function(value) {
  if (is.infinite(value)) {
    return(if (value > 0) "INFINITY" else "-INFINITY")
  }
  if (is.nan(value)) {
    return("NAN")
  }
  sprintf("%.17g", value)
}

table_values <- function(source_file, target_pattern) {
  selected <- clark_coefficients$source_file == source_file &
    grepl(target_pattern, clark_coefficients$target, ignore.case = TRUE)
  values <- clark_coefficients[selected, , drop = FALSE]
  values <- values[order(values$source_line_start, values$ordinal), , drop = FALSE]
  if (!nrow(values) || any(values$value_type != "numeric")) {
    stop("numeric coefficient table not found: ", source_file, " / ", target_pattern)
  }
  values$value_numeric
}

compiled_tables <- list(
  r8_cf = table_values("r8dib.inc", "R8CF\\("),
  r8_cfo = table_values("r8cfo.inc", "R8CFO\\("),
  r8_dibmen = table_values("r8clkcoef.inc", "DIBMEN\\("),
  r8_total = table_values("r8clkcoef.inc", "^[(][(]TOTAL\\("),
  r8_ototal = table_values("r8clkcoef.inc", "^[(][(]OTOTAL\\("),
  r8_four = table_values("r8clkcoef.inc", "FOUR\\("),
  r8_seven = table_values("r8clkcoef.inc", "SEVEN\\("),
  r8_nine = table_values("r8clkcoef.inc", "NINE\\("),
  r9_a = table_values("r9coeff.inc", "coefA\\("),
  r9_0 = table_values("r9coeff.inc", "coef0\\("),
  r9_4 = table_values("r9coeff.inc", "coef4\\("),
  r9_79 = table_values("r9coeff.inc", "coef79\\(")
)

expected_lengths <- c(
  r8_cf = 182L * 18L,
  r8_cfo = 182L * 9L,
  r8_dibmen = 49L * 3L,
  r8_total = 49L * 7L,
  r8_ototal = 49L * 7L,
  r8_four = 49L * 6L,
  r8_seven = 15L * 6L,
  r8_nine = 34L * 6L,
  r9_a = 47L * 4L,
  r9_0 = 47L * 9L,
  r9_4 = 47L * 8L,
  r9_79 = 47L * 8L
)
actual_lengths <- vapply(compiled_tables, length, integer(1))
if (!identical(actual_lengths, expected_lengths)) {
  stop("compiled Clark coefficient table lengths do not match declarations")
}

header <- c(
  "#ifndef TREEVOLUME_CLARK_COEFFICIENTS_HPP",
  "#define TREEVOLUME_CLARK_COEFFICIENTS_HPP",
  "",
  "// Generated by data-raw/clark.R from NVEL commit",
  paste0("// ", nvel_commit, ". Do not edit by hand."),
  "namespace treevolume {",
  "namespace clark_coefficients {",
  ""
)
for (name in names(compiled_tables)) {
  header <- c(
    header,
    paste0("inline constexpr double ", name, "[] = {"),
    paste0(
      "    ",
      paste(vapply(compiled_tables[[name]], cpp_number, character(1)), collapse = ", "),
      "};"
    ),
    ""
  )
}
header <- c(
  header,
  "}  // namespace clark_coefficients",
  "}  // namespace treevolume",
  "",
  "#endif",
  ""
)
writeLines(
  header,
  file.path("inst", "include", "merchandiser", "clark_coefficients.hpp")
)

identifier_path <- file.path(fixture_root, "clark_identifiers.csv")
if (!file.exists(identifier_path)) {
  stop("Clark identifier metadata is missing: ", identifier_path)
}
clark_identifiers <- utils::read.csv(
  identifier_path,
  stringsAsFactors = FALSE,
  colClasses = c(identifier = "character")
)
if (nrow(clark_identifiers) != 2824L || anyDuplicated(clark_identifiers$identifier)) {
  stop("Clark identifier metadata must contain 2,824 unique identifiers")
}
clark_models <- data.frame(
  id = clark_identifiers$identifier,
  family = clark_identifiers$family,
  species = as.integer(clark_identifiers$table_species),
  region = as.integer(clark_identifiers$REGN),
  forest = sprintf("%02d", as.integer(clark_identifiers$FORST)),
  district = sprintf("%02d", as.integer(clark_identifiers$DIST)),
  variant = clark_identifiers$VAR,
  product = sprintf("%02d", as.integer(clark_identifiers$PROD)),
  top_code = ifelse(
    clark_identifiers$REGN == 8L,
    substr(clark_identifiers$identifier, 3L, 3L),
    ""
  ),
  table_variant = clark_identifiers$table_variant,
  discovery_mode = clark_identifiers$discovery_mode,
  source_file = "MERCHANDISER_FIXTURES/clark_identifiers.csv",
  upstream_commit = nvel_commit,
  stringsAsFactors = FALSE
)
clark_models <- clark_models[order(clark_models$family, clark_models$id), ]
rownames(clark_models) <- NULL
utils::write.csv(
  clark_models,
  file.path("inst", "extdata", "clark_models.csv"),
  row.names = FALSE,
  na = ""
)

pattern_path <- file.path(identifier_root, "identifiers.csv")
if (!file.exists(pattern_path)) {
  stop("identifier inventory is missing: ", pattern_path)
}
patterns <- utils::read.csv(pattern_path, stringsAsFactors = FALSE)
clark_patterns <- patterns[
  patterns$family %in% c("clark_r8", "clark_r9") &
    grepl("[?]", patterns$voleq),
  c("voleq", "family", "dispatch_routine", "source", "notes"),
  drop = FALSE
]
names(clark_patterns)[[1L]] <- "id"
if (nrow(clark_patterns) != 3L) {
  stop("expected three Clark profile pattern rows")
}
clark_patterns$source_file <- "TREEVOLUME_IDENTIFIERS/identifiers.csv"
clark_patterns$upstream_commit <- nvel_commit
utils::write.csv(
  clark_patterns,
  file.path("inst", "extdata", "clark_patterns.csv"),
  row.names = FALSE,
  na = ""
)

message(
  "Wrote ", nrow(clark_coefficients), " Clark DATA literal rows, ",
  nrow(clark_models), " models, and ", nrow(clark_patterns), " patterns."
)
