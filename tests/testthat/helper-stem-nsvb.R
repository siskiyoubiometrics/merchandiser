.nsvb_biomass_columns <- c(
  paste0("dry_", c(
    "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood", "stump_bark",
    "saw_wood", "saw_bark", "topwood_wood", "topwood_bark", "tip_wood",
    "tip_bark", "branches", "foliage", "top_and_limb"
  )),
  "carbon",
  paste0("green_", c(
    "agb_no_foliage", "stem_wood", "stem_bark", "stump_wood", "stump_bark",
    "saw_wood", "saw_bark", "topwood_wood", "topwood_bark", "tip_wood",
    "tip_bark", "branches", "foliage", "top_and_limb"
  ))
)

.nsvb_fixture_path <- function(root, precision) {
  name <- paste0("nsvb.", precision, ".csv.gz")
  paths <- c(file.path(root, "full", name), file.path(root, name))
  found <- paths[file.exists(paths)]
  if (length(found)) found[[1L]] else paths[[1L]]
}

.nsvb_read_fixture <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE)
}

.nsvb_division_from_id <- function(id) {
  as.integer(substr(id, 5L, 7L)) + ifelse(substr(id, 4L, 4L) == "M", 1000L, 0L)
}

.nsvb_expect_relative <- function(actual, expected, tolerance) {
  relative <- abs(actual - expected) / pmax(abs(expected), 1)
  expect_true(
    all(relative <= tolerance),
    info = paste("maximum relative difference", max(relative, na.rm = TRUE))
  )
}

.nsvb_biomass_tolerance <- function(precision) {
  # Proposed values. helper-tolerance.R remains maintainer-owned.
  if (identical(precision, "single")) 1e-3 else 2e-12
}

.nsvb_evaluate_biomass <- function(data) {
  override <- data$MRULEMOD == "Y"
  biomass(
    data$DBHOB, data$HTTOT, data$FIASPCD,
    .nsvb_division_from_id(data$VOLEQ),
    region = data$REGN, forest = data$FORST,
    decay_class = data$DECAYCD, cull = data$CULL,
    primary_top = data$MTOPP, secondary_top = data$MTOPS,
    stump_ht = data$STUMP,
    max_log_length = ifelse(override, data$NEWMAXLEN, NA_real_),
    min_log_length = ifelse(override, data$NEWMINLEN, NA_real_),
    trim = ifelse(override, data$NEWTRIM, NA_real_),
    status = TRUE
  )
}

.nsvb_compiled_inverse <- function(data) {
  result <- data.frame(
    value = rep(NA_real_, nrow(data)), status = integer(nrow(data))
  )
  for (id in unique(data$VOLEQ)) {
    rows <- which(data$VOLEQ == id)
    evaluated <- merchandiser:::tv_cpp_kernel_eval(
      paste0("nsvb:", id), 2L,
      data$DBHOB[rows], data$HTTOT[rows], data$STEMDIB[rows],
      data$STEMDIB[rows], rep(NA_real_, length(rows)), 1L
    )
    result$value[rows] <- evaluated$value
    result$status[rows] <- evaluated$status
  }
  result
}

.check_nsvb_fixture <- function(path, precision, full = FALSE,
                                record = FALSE) {
  fixture <- .nsvb_read_fixture(path)

  profile_rows <- grepl("^PROFILE", fixture$CALL_KIND)
  profile <- fixture[profile_rows, , drop = FALSE]
  inside <- dib(
    profile$DBHOB, profile$HTTOT, profile$HTUP, profile$VOLEQ,
    status = TRUE
  )
  expect_identical(inside$status, integer(nrow(profile)))
  .nsvb_expect_relative(
    inside$value, profile$DIB,
    tv_tolerance[[paste0("diameter_", precision, "_rel")]]
  )
  inverse <- height_at_dib(
    profile$DBHOB, profile$HTTOT, profile$STEMDIB, profile$VOLEQ,
    status = TRUE
  )
  stump_diameter <- dib(
    profile$DBHOB, profile$HTTOT, 1, profile$VOLEQ, status = TRUE
  )
  expect_identical(stump_diameter$status, integer(nrow(profile)))
  outside_contract <- profile$STEMDIB > stump_diameter$value
  if (full) {
    expect_identical(sum(outside_contract), 279L)
  }
  expect_identical(
    inverse$status[!outside_contract], integer(sum(!outside_contract))
  )
  expect_true(all(inverse$status[outside_contract] == 101L))
  expect_true(all(is.na(inverse$value[outside_contract])))
  .nsvb_expect_relative(
    inverse$value[!outside_contract], profile$STEMHT[!outside_contract],
    tv_tolerance[[paste0("height_", precision, "_rel")]]
  )
  source_dob <- .with_treevolume_compat("nvel", dob(
    profile$DBHOB, profile$HTTOT, profile$HTUP, profile$VOLEQ,
    status = TRUE
  ))
  expected_dob <- if ("DOB" %in% names(profile)) {
    profile$DOB
  } else {
    rep(0, nrow(profile))
  }
  expect_identical(source_dob$status, integer(nrow(profile)))
  expect_identical(source_dob$value, as.double(expected_dob))
  if (record) {
    .gate1_record_values(
      "nsvb", "DIB", precision, inside$value, profile$DIB,
      rep(TRUE, nrow(profile)),
      tv_tolerance[[paste0("diameter_", precision, "_rel")]],
      denominator_floor = 1
    )
    .gate1_record_values(
      "nsvb", "height at DIB", precision,
      inverse$value, profile$STEMHT, !outside_contract,
      tv_tolerance[[paste0("height_", precision, "_rel")]],
      list(above_contract_stump_diameter = outside_contract),
      denominator_floor = 1
    )
    .gate1_record_values(
      "nsvb", "DOB", precision,
      source_dob$value, expected_dob, rep(TRUE, nrow(profile)), 0,
      denominator_floor = 1, compat = "nvel"
    )
  }

  biomass_rows <- grepl("^NVBC", fixture$CALL_KIND)
  biomass_fixture <- fixture[biomass_rows, , drop = FALSE]
  observed <- .nsvb_evaluate_biomass(biomass_fixture)
  status_names <- names(observed)[grepl("_status$", names(observed))]
  expect_true(all(as.matrix(observed[status_names]) == 0L))
  actual <- as.matrix(observed[.nsvb_biomass_columns])
  green_carbon <- observed$carbon * observed$green_agb_no_foliage /
    observed$dry_agb_no_foliage
  actual <- cbind(actual, green_carbon)
  expected <- as.matrix(biomass_fixture[c(
    paste0("DRYBIO", 1:15), paste0("GRNBIO", 1:15)
  )])
  .nsvb_expect_relative(
    actual, expected, .nsvb_biomass_tolerance(precision)
  )
  expect_equal(observed$co2e, observed$carbon * 44 / 12, tolerance = 0)
  if (record) {
    tolerance <- .nsvb_biomass_tolerance(precision)
    relative <- abs(actual - expected) / pmax(abs(expected), 1)
    row_within <- apply(
      is.finite(relative) & relative <= tolerance, 1L, all
    )
    .gate1_record_summary(
      "nsvb", "biomass, 30 values per row", precision,
      nrow(relative), sum(row_within), max(relative, na.rm = TRUE), tolerance
    )
  }

  invisible(fixture)
}
