# Regenerate the Region 10 and Region 4 coefficient inventories, compile-time table, and
# exact model metadata.  Run from the package root. NVEL_SOURCE and MERCHANDISER_FIXTURES
# may override the read-only source and oracle fixture locations. Runtime package code does
# not depend on either external tree.

nvel_commit <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
nvel_source <- Sys.getenv("NVEL_SOURCE", unset = file.path(
  "..", "..", "tools",
  "oracle", "nvel"
))
fixture_root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = file.path(
  "..", "merchandiser", "tools",
  "oracle", "fixtures"
))

source_files <- c(
  "r10tap.f", "r10tapo.f", "r10d2h.f", "r10vol.f", "r10vol1.f", "r10volo.f",
  "r4d2h.f", "r4vol.f", "profile.f", "calcdia.f", "ht2topd.f"
)
missing_files <- source_files[!file.exists(file.path(nvel_source, source_files))]
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

# Each named Region 10 constant is tied to the source span that defines it.  This makes
# accidental transcription or source drift fail regeneration.
r10_spec <- data.frame(name = c(
  "spruce_shape", "spruce_h1", "spruce_h2", "spruce_hd", "spruce_d2",
  "spruce_h2_tail", "spruce_hd_tail", "spruce_bark_0", "spruce_bark_d",
  "spruce_bark_h", "redcedar_d2",
  "redcedar_d", "redcedar_h", "redcedar_h2", "redcedar_d_sq", "redcedar_h2_tail",
  "redcedar_d2_tail",
  "redcedar_bark_0", "redcedar_bark_h", "redcedar_bark_d_inv", "alaska_cedar_d",
  "alaska_cedar_h2",
  "alaska_cedar_hd", "alaska_cedar_d_tail", "alaska_cedar_h2_tail", "alaska_cedar_d2_tail",
  "alaska_cedar_hd_tail", "alaska_cedar_bark_0", "alaska_cedar_bark_d",
  "alaska_cedar_bark_h_inv",
  "alder_0", "alder_d", "alder_h", "alder_hd", "alder_sqrt_h", "alder_h2", "fstgro_short_0",
  "fstgro_short_d", "fstgro_short_dh", "fstgro_tall_0", "fstgro_tall_h2", "fstgro_tall_dh2",
  "fstgro_tall_dh", "fstgro_tall_d", "fstgro_scale", "secgro_0", "secgro_d",
  "secgro_h", "r10tc_pi",
  "r10tc_smalian"
), source_file = c(rep("r10tap.f", 36L), rep("r10vol.f", 12L), rep(
  "r10vol1.f",
  2L
)), source_line_start = c(
  rep(36L, 7L), rep(43L, 3L), rep(51L, 7L), rep(58L, 3L), rep(
    67L,
    7L
  ), rep(74L, 3L), rep(79L, 6L), rep(109L, 3L), rep(115L, 5L), 118L, rep(143L, 3L), 1005L,
  1012L
), source_line_end = c(
  rep(38L, 7L), rep(43L, 3L), rep(54L, 7L), rep(58L, 3L), rep(
    70L,
    7L
  ), rep(74L, 3L), rep(84L, 6L), rep(113L, 3L), rep(116L, 5L), 118L, rep(143L, 3L), 1005L,
  1012L
), literal = c(
  "1.5", "-.0052554", "0.000034947", "0.104477", "7.76807", "- 0.0000094852",
  "- 0.011351", "0.8467", "0.0009144", "0.0003568", "5.17703194", "- 0.12516819",
  "0.02537037",
  "- 0.00004193", "0.00155481", "-0.00002070", "0.24125235", "0.86031485",
  "0.00059638", "- 0.18335961",
  "-0.02834001", "0.00007123", "0.06709114", ".00282021", "- 0.00002277",
  "1.06064717", "- 0.00528349",
  "0.95866817", "0.00064402", "- 3.1299972", "0.91274", "1.9758", "8.2375", "4.964",
  "3.773",
  "7.417", "0.406098", "- 0.0762998", "0.00262615", "0.480961", "42.46542",
  "- 10.99643", "- 0.107809",
  "- 0.00409083", "0.005454154", "-5.577", "1.9067", "0.9416", "3.1416", ".00272708"
), stringsAsFactors = FALSE)

read_span <- function(file, first, last) {
  lines <- readLines(file.path(nvel_source, file), warn = FALSE)
  paste(sub("^.{0,6}", "", lines[seq.int(first, last)]), collapse = "\n")
}
for (row in seq_len(nrow(r10_spec))) {
  span <- read_span(
    r10_spec$source_file[[row]], r10_spec$source_line_start[[row]],
    r10_spec$source_line_end[[row]]
  )
  normalized_span <- gsub("[[:space:]]", "", span)
  normalized_literal <- gsub("[[:space:]]", "", r10_spec$literal[[row]])
  if (!grepl(normalized_literal, normalized_span, fixed = TRUE)) {
    stop("missing literal ", r10_spec$literal[[row]], " for ", r10_spec$name[[row]])
  }
}
r10_spec$value_numeric <- as.double(gsub("[[:space:]]", "", gsub(
  "[dD]", "e",
  r10_spec$literal
)))
r10_spec$family <- "r10"
r10_spec$target <- r10_spec$name
r10_spec$ordinal <- 1L

# CFCOEF is one fixed-form DATA statement. Its storage order is the Fortran column-major
# order used by CFCOEF(20, 7).
r4_lines <- readLines(file.path(nvel_source, "r4vol.f"), warn = FALSE)
r4_text <- paste(sub("^.{0,6}", "", r4_lines[559:576]), collapse = " ")
r4_body <- sub("^.*CFCOEF[ ]*/", "", r4_text, ignore.case = TRUE)
r4_body <- sub("/[ ]*$", "", r4_body)
r4_literals <- trimws(strsplit(r4_body, ",", fixed = TRUE)[[1L]])
if (length(r4_literals) != 140L) {
  stop("expected 140 R4 CFCOEF literals, found ", length(r4_literals))
}
r4_spec <- data.frame(
  name = paste0("cfcoef_", seq_along(r4_literals)), source_file = "r4vol.f",
  source_line_start = 559L, source_line_end = 576L, literal = r4_literals,
  value_numeric = as.double(gsub(
    "[dD]",
    "e", r4_literals
  )), family = "r4_driver", target = "CFCOEF(20,7)", ordinal = seq_along(r4_literals),
  stringsAsFactors = FALSE
)

coefficient_columns <- c(
  "family", "source_file", "source_line_start", "source_line_end", "target",
  "ordinal", "literal", "value_numeric"
)
r10r4_coefficients <- rbind(r10_spec[, coefficient_columns], r4_spec[, coefficient_columns])
r10r4_coefficients$upstream_commit <- nvel_commit

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(r10r4_coefficients, file.path("inst", "extdata", "r10r4_coefficients.csv"),
  row.names = FALSE, na = ""
)

cpp_number <- function(value) sprintf("%.17g", value)
header <- c(
  "#ifndef TREEVOLUME_R10R4_COEFFICIENTS_HPP", "#define TREEVOLUME_R10R4_COEFFICIENTS_HPP",
  "", "// Generated by data-raw/r10r4.R from NVEL commit", paste0(
    "// ",
    nvel_commit, ". Do not edit by hand."
  ),
  "namespace treevolume {", "namespace r10r4_coefficients {", ""
)
for (row in seq_len(nrow(r10_spec))) {
  header <- c(header, paste0(
    "inline constexpr double ", r10_spec$name[[row]], " = ", cpp_number(
      r10_spec$value_numeric[[row]]
    ),
    ";"
  ))
}
header <- c(
  header, "", "inline constexpr double r4_cfcoef[140] = {", paste0("    ", paste(vapply(
    r4_spec$value_numeric,
    cpp_number, character(1L)
  ), collapse = ", ")), "};", "", "}  // namespace r10r4_coefficients",
  "}  // namespace treevolume", "", "#endif"
)
dir.create(file.path("inst", "include", "merchandiser"),
  recursive = TRUE,
  showWarnings = FALSE
)
writeLines(header, file.path("inst", "include", "merchandiser", "r10r4_coefficients.hpp"))

model_rows <- list()
for (family in c("r10_taper", "r4_driver")) {
  path <- file.path(fixture_root, "full", paste0(family, ".double.csv.gz"))
  fixture <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(
    VOLEQ = "character"
  ))
  ids <- sort(unique(fixture$VOLEQ))
  model_rows[[family]] <- data.frame(
    id = ids, family = family,
    species = suppressWarnings(as.integer(substr(
      ids,
      8L, 10L
    ))), source_file = if (family == "r10_taper")
      "r10tap.f" else "r4vol.f", upstream_commit = nvel_commit, oracle_tested = TRUE,
    stringsAsFactors = FALSE
  )
}
r10r4_models <- do.call(rbind, model_rows)
rownames(r10r4_models) <- NULL
utils::write.csv(r10r4_models, file.path("inst", "extdata", "r10r4_models.csv"),
  row.names = FALSE,
  na = ""
)

r10r4_sources <- data.frame(
  source_file = source_files, role = c(
    "active Region 10 taper", "older Region 10 taper reference",
    "Region 10 direct-volume reference", "Region 10 driver and small trees",
    "Region 10 total cubic integration",
    "older Region 10 volume driver", "Region 4 direct-volume reference",
    "Region 4 driver, Mathis taper, and CFCOEF",
    "profile dispatch and R10HTS", "diameter dispatch", "height-at-diameter dispatch"
  ), evidence = c(
    "r10tap.f:1-202",
    "r10tapo.f", "r10d2h.f", "r10vol.f:1-153", "r10vol1.f:980-1021",
    "r10volo.f:1-11", "r4d2h.f",
    paste(
      "r4vol.f:1-6 revision history; r4vol.f:28 says the former CFCOEF",
      "statement was commented out 03/21/2017 YW; r4vol.f:559-576 is active"
    ),
    "profile.f:144-146, 1643-2056", "calcdia.f Region 10 and Region 4",
    "ht2topd.f:247-260 Region 10; Region 4 R4MATTAPER"
  ),
  upstream_commit = nvel_commit, stringsAsFactors = FALSE
)
utils::write.csv(r10r4_sources, file.path("inst", "extdata", "r10r4_sources.csv"),
  row.names = FALSE,
  na = ""
)

stopifnot(nrow(r10_spec) == 50L, nrow(r4_spec) == 140L, sum(
  r10r4_models$family == "r10_taper"
) ==
  38L, sum(r10r4_models$family == "r4_driver") == 20L)
message("wrote 190 coefficient literals, 58 exact models, and 11 source records")
