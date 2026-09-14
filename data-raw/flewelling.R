# Regenerate the Flewelling coefficient inventory and the compile-time table.
#
# Run from the package root. Set NVEL_SOURCE to override the read-only NVEL
# checkout. The package does not require this script or an R data object at
# run time. CSV is used so every source literal and its provenance remain
# directly inspectable, while the generated header supplies immutable values
# to the compiled kernel.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv(
  "NVEL_SOURCE",
  unset = file.path("..", "..", "tools", "oracle", "nvel")
)

source_files <- c(
  "f_west.f", "f_ingy.f", "f_alaska.f", "f_other.f",
  "sf_2pt.f", "sf_2pth.f", "sf_3pt.f", "sf_3z.f", "sf_corr.f",
  "sf_dfz.f", "sf_ds.f", "sf_hs.f", "sf_shp.f", "sf_taper.f",
  "sf_yhat.f", "sf_yhat3.f", "sf_zero.f"
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

split_fortran_values <- function(text) {
  characters <- strsplit(text, "", fixed = TRUE)[[1L]]
  values <- character()
  current <- character()
  quote <- ""
  for (character in characters) {
    if (character %in% c("'", "\"") && (!nzchar(quote) || quote == character)) {
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
    return(list(type = "character", numeric = NA_real_,
                character = substring(literal, 2L, nchar(literal) - 1L)))
  }
  if (tolower(literal) %in% c(".true.", ".false.")) {
    return(list(type = "logical", numeric = NA_real_,
                character = tolower(literal)))
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
    if (is_comment(line) || !grepl("^[ ]{0,6}data(?:[ ]|[(])", line,
                                   ignore.case = TRUE, perl = TRUE)) {
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
      slash_count <- slash_count +
        lengths(regmatches(continuation, gregexpr("/", continuation, fixed = TRUE)))
    }
    statement <- paste(sub("^.{0,6}", "", statement_lines), collapse = " ")
    matched <- regexec("^[ ]*data[ ]*([^/]+)/(.+)/[ ]*$", statement,
                       ignore.case = TRUE)
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

flewelling_coefficients <- do.call(rbind, lapply(source_files, parse_file))
rownames(flewelling_coefficients) <- NULL

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  flewelling_coefficients,
  file.path("inst", "extdata", "flewelling_coefficients.csv"),
  row.names = FALSE,
  na = ""
)

cpp_number <- function(value) {
  if (is.infinite(value)) {
    return(if (value > 0) "INFINITY" else "-INFINITY")
  }
  if (is.nan(value)) {
    return("NAN")
  }
  sprintf("%.17g", value)
}

statement_key <- interaction(
  flewelling_coefficients$source_file,
  flewelling_coefficients$source_line_start,
  drop = TRUE,
  lex.order = TRUE
)
statements <- split(flewelling_coefficients, statement_key)
header <- c(
  "#ifndef TREEVOLUME_FLEWELLING_COEFFICIENTS_HPP",
  "#define TREEVOLUME_FLEWELLING_COEFFICIENTS_HPP",
  "",
  "// Generated by data-raw/flewelling.R from NVEL commit",
  paste0("// ", nvel_commit, ". Do not edit by hand."),
  "namespace treevolume {",
  "namespace flewelling_coefficients {",
  ""
)
for (statement in statements) {
  numeric <- statement$value_type == "numeric"
  if (!all(numeric)) {
    next
  }
  name <- paste0(
    sub("[.]f$", "", statement$source_file[[1L]]), "_",
    statement$source_line_start[[1L]]
  )
  header <- c(
    header,
    paste0("// ", statement$routine[[1L]], ": ", statement$target[[1L]]),
    paste0("inline constexpr double ", name, "[] = {"),
    paste0("    ", paste(vapply(statement$value_numeric, cpp_number, character(1)),
                         collapse = ", "), "};"),
    ""
  )
}
header <- c(
  header,
  "}  // namespace flewelling_coefficients",
  "}  // namespace treevolume",
  "",
  "#endif",
  ""
)
writeLines(header, file.path(
  "inst", "include", "merchandiser", "flewelling_coefficients.hpp"
))

message(
  "Wrote ", nrow(flewelling_coefficients), " coefficient literal rows from ",
  length(unique(interaction(flewelling_coefficients$source_file,
                            flewelling_coefficients$source_line_start))),
  " DATA statements."
)

fixture_root <- Sys.getenv(
  "MERCHANDISER_FIXTURES",
  unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
)
fixture_full <- file.path(fixture_root, "full")

read_model_columns <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  classes <- rep("NULL", length(header))
  classes[header == "VOLEQ"] <- "character"
  classes[header == "FIASPCD"] <- "integer"
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}

model_paths <- file.path(
  fixture_full,
  c("flewelling_2pt.double.csv.gz", "flewelling_3pt.double.csv.gz")
)
if (!all(file.exists(model_paths))) {
  stop("full Flewelling fixtures are required to regenerate model metadata")
}
model_rows <- Map(function(path, family) {
  values <- unique(read_model_columns(path))
  values$family <- family
  values
}, model_paths, c("flewelling_2pt", "flewelling_3pt"))
flewelling_models <- unique(do.call(rbind, model_rows))
names(flewelling_models)[names(flewelling_models) == "VOLEQ"] <- "id"
names(flewelling_models)[names(flewelling_models) == "FIASPCD"] <- "species"
flewelling_models <- flewelling_models[order(flewelling_models$family,
                                             flewelling_models$id), ]
flewelling_models$source_file <- sub(fixture_root, "MERCHANDISER_FIXTURES",
                                     model_paths[match(
                                       flewelling_models$family,
                                       c("flewelling_2pt", "flewelling_3pt")
                                     )], fixed = TRUE)
flewelling_models$upstream_commit <- nvel_commit
rownames(flewelling_models) <- NULL
utils::write.csv(
  flewelling_models,
  file.path("inst", "extdata", "flewelling_models.csv"),
  row.names = FALSE,
  na = ""
)
message("Wrote metadata for ", nrow(flewelling_models), " Flewelling models.")
