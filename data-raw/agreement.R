# Regenerate inst/extdata/oracle-agreement.csv from one fixture-enabled test run.
#
# Usage:
# Rscript data-raw/agreement.R /tmp/oracle-agreement-raw.csv

arguments <- commandArgs(trailingOnly = TRUE)
input <- if (length(arguments)) {
  arguments[[1L]]
} else {
  Sys.getenv("TREEVOLUME_AGREEMENT_INPUT", unset = "")
}
if (!nzchar(input) || !file.exists(input)) {
  stop(
    "Supply the raw TREEVOLUME_GATE1_RESULTS CSV as the first argument or ",
    "TREEVOLUME_AGREEMENT_INPUT.",
    call. = FALSE
  )
}

required <- c(
  "family", "output", "precision", "compat", "rows_compared",
  "rows_within_tolerance", "max_relative_difference", "tolerance",
  "exclusions"
)
agreement <- utils::read.csv(input, stringsAsFactors = FALSE)
if (!identical(names(agreement), required)) {
  stop("The raw agreement CSV has unexpected columns.", call. = FALSE)
}
if (!nrow(agreement)) {
  stop("The raw agreement CSV has no rows.", call. = FALSE)
}
if (!all(agreement$precision %in% c("double", "single"))) {
  stop("The raw agreement CSV has an invalid precision.", call. = FALSE)
}
if (!all(agreement$compat %in% c("port", "nvel"))) {
  stop("The raw agreement CSV has an invalid compatibility mode.", call. = FALSE)
}
if (any(agreement$rows_within_tolerance > agreement$rows_compared)) {
  stop("Rows within tolerance exceed rows compared.", call. = FALSE)
}

ordering <- order(
  agreement$family, agreement$output, agreement$compat,
  match(agreement$precision, c("double", "single"))
)
agreement <- agreement[ordering, required, drop = FALSE]
rownames(agreement) <- NULL

output <- file.path("inst", "extdata", "oracle-agreement.csv")
utils::write.csv(agreement, output, row.names = FALSE, na = "")
message("Wrote ", output, " from ", input)
