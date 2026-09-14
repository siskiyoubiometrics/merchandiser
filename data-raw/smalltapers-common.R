# Shared source extractor for the regional small-taper families.
#
# Run a family script from the package root. NVEL_SOURCE may override the
# read-only checkout, and MERCHANDISER_FIXTURES may override the oracle root.

.smalltaper_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
.smalltaper_source <- Sys.getenv(
  "NVEL_SOURCE", unset = file.path("..", "..", "tools", "oracle", "nvel")
)

.smalltaper_files <- list(
  r1_taper = "r1tap.f",
  r2_taper = "r2tap.f",
  r5_taper = "r5tap.f",
  r12_taper = c("r12tap.f", "stump.f"),
  blm_taper = c("blmtap.f", "blmvol.f", "formclas.f"),
  behre_taper = c("blmtap.f", "formclas.f", "r6vol3.f")
)

.smalltaper_comment <- function(line) {
  nzchar(line) && substr(line, 1L, 1L) %in% c("c", "C", "*", "!")
}

.smalltaper_routine <- function(lines, line_number) {
  declarations <- grep(
    "^[ ]{0,6}(subroutine|([a-z0-9*]+[ ]+)?function)[ ]+",
    lines[seq_len(line_number)], ignore.case = TRUE, value = TRUE
  )
  if (!length(declarations)) return(NA_character_)
  sub(
    "^.*(?:subroutine|function)[ ]+([a-z0-9_]+).*$", "\\1",
    tail(declarations, 1L), ignore.case = TRUE
  )
}

.smalltaper_split <- function(text) {
  characters <- strsplit(text, "", fixed = TRUE)[[1L]]
  values <- current <- character()
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
  trimws(c(values, paste0(current, collapse = "")))
}

.smalltaper_expand <- function(values) {
  unlist(lapply(values[nzchar(values)], function(value) {
    parts <- regmatches(value, regexec("^([0-9]+)[*](.+)$", value))[[1L]]
    if (length(parts)) rep(trimws(parts[[3L]]), as.integer(parts[[2L]])) else value
  }), use.names = FALSE)
}

.smalltaper_literal <- function(literal) {
  if (grepl("^(['\"]).*\\1$", literal)) {
    return(list(type = "character", numeric = NA_real_,
                character = substring(literal, 2L, nchar(literal) - 1L)))
  }
  candidate <- gsub("[dD]", "e", literal)
  numeric <- suppressWarnings(as.double(candidate))
  if (!is.na(numeric)) {
    return(list(type = "numeric", numeric = numeric, character = NA_character_))
  }
  list(type = "expression", numeric = NA_real_, character = literal)
}

.smalltaper_parse_file <- function(source_file) {
  lines <- readLines(file.path(.smalltaper_source, source_file), warn = FALSE)
  rows <- list()
  line_number <- 1L
  while (line_number <= length(lines)) {
    line <- lines[[line_number]]
    if (.smalltaper_comment(line) ||
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
      statement_lines <- c(statement_lines, continuation)
      slash_count <- slash_count +
        lengths(regmatches(continuation, gregexpr("/", continuation, fixed = TRUE)))
    }
    statement <- paste(sub("^.{0,6}", "", statement_lines), collapse = " ")
    parts <- regmatches(
      statement,
      regexec("^[ ]*data[ ]*([^/]+)/(.+)/[ ]*$", statement, ignore.case = TRUE)
    )[[1L]]
    if (!length(parts)) stop(source_file, ":", start, ": could not parse DATA")
    literals <- .smalltaper_expand(.smalltaper_split(parts[[3L]]))
    parsed <- lapply(literals, .smalltaper_literal)
    rows[[length(rows) + 1L]] <- data.frame(
      source_file = source_file,
      source_line_start = start,
      source_line_end = line_number,
      routine = .smalltaper_routine(lines, start),
      target = trimws(parts[[2L]]),
      ordinal = seq_along(literals),
      literal = literals,
      value_type = vapply(parsed, `[[`, character(1), "type"),
      value_numeric = vapply(parsed, `[[`, double(1), "numeric"),
      value_character = vapply(parsed, `[[`, character(1), "character"),
      upstream_commit = .smalltaper_commit,
      stringsAsFactors = FALSE
    )
    line_number <- line_number + 1L
  }
  do.call(rbind, rows)
}

.write_smalltaper_metadata <- function() {
  fixture_root <- Sys.getenv(
    "MERCHANDISER_FIXTURES",
    unset = file.path("..", "merchandiser", "tools", "oracle", "fixtures")
  )
  families <- names(.smalltaper_files)
  rows <- lapply(families, function(family) {
    path <- file.path(fixture_root, "full", paste0(family, ".double.csv.gz"))
    connection <- gzfile(path, open = "rt")
    on.exit(close(connection), add = TRUE)
    header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
    classes <- rep("NULL", length(header))
    classes[header %in% c("VOLEQ", "FIASPCD")] <- NA
    values <- unique(utils::read.csv(
      path, colClasses = classes, stringsAsFactors = FALSE
    ))
    names(values) <- c("species", "id")[match(names(values), c("FIASPCD", "VOLEQ"))]
    values$family <- family
    values$source_file <- paste0(
      "MERCHANDISER_FIXTURES/full/", family, ".double.csv.gz"
    )
    values$upstream_commit <- .smalltaper_commit
    values
  })
  metadata <- unique(do.call(rbind, rows))
  metadata <- metadata[order(metadata$family, metadata$id), ]
  rownames(metadata) <- NULL
  utils::write.csv(
    metadata, file.path("inst", "extdata", "smalltaper_models.csv"),
    row.names = FALSE, na = ""
  )
}

extract_smalltaper_family <- function(family) {
  if (!family %in% names(.smalltaper_files)) stop("unknown family: ", family)
  missing <- .smalltaper_files[[family]][
    !file.exists(file.path(.smalltaper_source, .smalltaper_files[[family]]))
  ]
  if (length(missing)) stop("missing NVEL source files: ", paste(missing, collapse = ", "))
  actual_commit <- system2(
    "git", c("-C", normalizePath(.smalltaper_source), "rev-parse", "HEAD"),
    stdout = TRUE
  )
  if (!identical(actual_commit, .smalltaper_commit)) {
    stop("NVEL checkout is not at the pinned commit: ", actual_commit)
  }
  coefficients <- do.call(
    rbind, lapply(.smalltaper_files[[family]], .smalltaper_parse_file)
  )
  rownames(coefficients) <- NULL
  dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    coefficients,
    file.path("inst", "extdata", paste0(family, "_coefficients.csv")),
    row.names = FALSE, na = ""
  )
  .write_smalltaper_metadata()
  message("Wrote ", nrow(coefficients), " literal rows for ", family, ".")
  invisible(coefficients)
}
