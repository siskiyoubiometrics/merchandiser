# Regenerate the NVEL default-equation tables, FIA crosswalk, identifier
# catalog, and regional merchant-rule defaults.
#
# Run from the package root. The installed package reads only the generated
# CSV files under inst/extdata. The NVEL and treevolume oracle trees are
# read-only inputs.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv(
  "NVEL_SOURCE",
  unset = file.path("..", "..", "tools", "oracle", "nvel")
)
identifier_source <- Sys.getenv(
  "TREEVOLUME_IDENTIFIERS",
  unset = file.path("..", "merchandiser", "tools", "identifiers")
)

source_files <- c("voleqdef.f", "fiaeq2nveleq.for", "mrules.f")
missing <- source_files[!file.exists(file.path(nvel_source, source_files))]
if (length(missing)) {
  stop("missing NVEL source files: ", paste(missing, collapse = ", "))
}
actual_commit <- system2(
  "git", c("-C", normalizePath(nvel_source), "rev-parse", "HEAD"),
  stdout = TRUE
)
if (!identical(actual_commit, nvel_commit)) {
  stop("NVEL checkout is not at the pinned commit: ", actual_commit)
}

.defaults_is_comment <- function(line) {
  nzchar(line) && substr(line, 1L, 1L) %in% c("c", "C", "*", "!")
}

.defaults_routine_at <- function(lines, at) {
  declaration <- grep(
    "^[ ]{0,6}subroutine[ ]+", lines[seq_len(at)],
    ignore.case = TRUE, value = TRUE
  )
  if (!length(declaration)) return(NA_character_)
  sub(
    "^.*subroutine[ ]+([a-z0-9_]+).*$", "\\1", tail(declaration, 1L),
    ignore.case = TRUE
  )
}

.defaults_split <- function(text) {
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

.defaults_expand <- function(values) {
  result <- character()
  for (value in values[nzchar(values)]) {
    match <- regmatches(value, regexec("^([0-9]+)[*](.+)$", value))[[1L]]
    if (length(match)) {
      result <- c(result, rep(trimws(match[[3L]]), as.integer(match[[2L]])))
    } else {
      result <- c(result, value)
    }
  }
  result
}

.defaults_literal <- function(value) {
  if (grepl("^(['\"]).*\\1$", value)) {
    return(c(
      value_type = "character", value_numeric = NA_character_,
      value_character = substring(value, 2L, nchar(value) - 1L)
    ))
  }
  numeric <- suppressWarnings(as.double(gsub("[dD]", "e", value)))
  if (!is.na(numeric)) {
    return(c(
      value_type = "numeric", value_numeric = format(numeric, digits = 17),
      value_character = NA_character_
    ))
  }
  c(
    value_type = "expression", value_numeric = NA_character_,
    value_character = value
  )
}

.defaults_parse_file <- function(source_file) {
  lines <- readLines(file.path(nvel_source, source_file), warn = FALSE)
  output <- list()
  at <- 1L
  while (at <= length(lines)) {
    line <- lines[[at]]
    if (.defaults_is_comment(line) ||
          !grepl("^[ ]{0,6}data(?:[ ]|[(])", line, ignore.case = TRUE)) {
      at <- at + 1L
      next
    }
    first <- at
    statement <- line
    closed <- lengths(regmatches(line, gregexpr("/", line, fixed = TRUE))) >= 2L
    while (!closed && at < length(lines)) {
      at <- at + 1L
      continuation <- lines[[at]]
      if (.defaults_is_comment(continuation)) next
      statement <- c(statement, continuation)
      closed <- grepl("/[ ]*$", continuation)
    }
    collapsed <- paste(sub("^.{0,6}", "", statement), collapse = " ")
    parts <- regmatches(
      collapsed,
      regexec("^[ ]*data[ ]*([^/]+)/(.+)/[ ]*$", collapsed, ignore.case = TRUE)
    )[[1L]]
    if (!length(parts)) stop(source_file, ":", first, ": invalid DATA statement")
    values <- .defaults_expand(.defaults_split(parts[[3L]]))
    parsed <- t(vapply(values, .defaults_literal, character(3L)))
    output[[length(output) + 1L]] <- data.frame(
      source_file = source_file,
      source_line_start = first,
      source_line_end = at,
      routine = toupper(.defaults_routine_at(lines, first)),
      target = toupper(trimws(parts[[2L]])),
      ordinal = seq_along(values),
      literal = values,
      value_type = parsed[, "value_type"],
      value_numeric = as.double(parsed[, "value_numeric"]),
      value_character = parsed[, "value_character"],
      upstream_commit = nvel_commit,
      stringsAsFactors = FALSE
    )
    at <- at + 1L
  }
  do.call(rbind, output)
}

defaults_data <- do.call(rbind, lapply(source_files, .defaults_parse_file))
rownames(defaults_data) <- NULL
keep_routines <- c(
  "R1_EQN", "R2_EQN", "R3_EQN", "R4_EQN", "R5_EQN", "R6_EQN",
  "R7_EQN", "R8_BEQN", "R8_CEQN", "R9_EQN", "R10_EQN",
  "R5_PNWEQN", "R6_PNWEQN", "FIAEQ2NVELEQ"
)
default_arrays <- defaults_data[defaults_data$routine %in% keep_routines, ]

.defaults_values <- function(routine, target_pattern) {
  selected <- default_arrays$routine == routine &
    grepl(target_pattern, default_arrays$target, ignore.case = TRUE)
  values <- default_arrays[selected, ]
  values[order(values$source_line_start, values$ordinal), ]
}

.crosswalk <- function(target, count, volume_class) {
  values <- .defaults_values("FIAEQ2NVELEQ", paste0(target, "[(]"))
  if (nrow(values) != count * 3L || any(values$value_type != "character")) {
    stop(target, " crosswalk has an unexpected shape")
  }
  matrix_values <- matrix(values$value_character, ncol = 3L, byrow = TRUE)
  data.frame(
    fia_code = matrix_values[, 1L],
    nvel_template = matrix_values[, 2L],
    volume_type = trimws(matrix_values[, 3L]),
    volume_class = volume_class,
    source_file = values$source_file[seq(1L, nrow(values), by = 3L)],
    source_line_start = values$source_line_start[seq(1L, nrow(values), by = 3L)],
    source_line_end = values$source_line_end[seq(1L, nrow(values), by = 3L)],
    upstream_commit = nvel_commit,
    stringsAsFactors = FALSE
  )
}

fia_crosswalk <- rbind(
  .crosswalk("BDLIST", 231L, "board_foot"),
  .crosswalk("CULIST", 361L, "cubic")
)

identifier_file <- file.path(identifier_source, "identifiers.csv")
if (!file.exists(identifier_file)) stop("missing identifier catalog: ", identifier_file)
identifier_catalog <- utils::read.csv(
  identifier_file, stringsAsFactors = FALSE,
  colClasses = c(voleq = "character")
)

# Baseline MRULES assignments. Product-, species-, and equation-dependent
# branches are applied in R. These rows make the regional constants auditable.
merchant_defaults <- data.frame(
  region = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L),
  even_or_odd = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
  option = c(22L, 22L, 22L, 22L, 22L, 23L, 23L, 22L, 22L, 23L, 23L),
  maximum_length = c(16, 16, 16, 16, 16, 16, 16, 8, 8, 16, 16),
  minimum_length = c(2, 2, 10, 2, 2, 2, 2, 2, 2, 8, 2),
  minimum_top_length = c(16, 2, 10, 2, 2, 2, 2, 2, 4, 8, 2),
  merchantable_length = c(8, 8, 10, 8, 8, 8, 8, 8, 8, 8, 8),
  primary_top = c(5.6, 6, 6, 6, 6, 2, NA, NA, NA, 6, 2),
  secondary_top = c(4, 4, 4, 4, 4, 2, 2, 4, 4, 4, 2),
  stump = c(1, 1, 1, 1, 1, 0, 1, NA, NA, 1, 0),
  trim = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.3, 0.5, 0.5),
  minimum_board_foot_dbh = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  scribner = c("table", "table", "table", "table", "table", "factor",
               "factor", "table", "table", "factor", "factor"),
  source_file = "mrules.f",
  source_line_start = c(46L, 87L, 105L, 262L, 280L, 302L, 321L, 337L,
                        370L, 396L, 302L),
  source_line_end = c(86L, 104L, 261L, 279L, 301L, 320L, 336L, 369L,
                      395L, 413L, 320L),
  upstream_commit = nvel_commit,
  stringsAsFactors = FALSE
)

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
write_generated <- function(value, name) {
  path <- file.path("inst", "extdata", name)
  utils::write.csv(value, path, row.names = FALSE, na = "")
  if (!file.exists(path)) stop("failed to write ", path)
}
write_generated(default_arrays, "nvel_default_arrays.csv")
write_generated(fia_crosswalk, "nvel_fia_crosswalk.csv")
write_generated(identifier_catalog, "nvel_identifier_catalog.csv")
write_generated(merchant_defaults, "nvel_merchant_defaults.csv")

message(
  "wrote ", nrow(default_arrays), " default-array values, ",
  nrow(fia_crosswalk), " FIA mappings, ", nrow(identifier_catalog),
  " identifiers, and ", nrow(merchant_defaults), " merchant defaults"
)
