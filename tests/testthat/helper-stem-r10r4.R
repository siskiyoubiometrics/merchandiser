.r10r4_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT", "HTUP", "STEMDIB",
  "CALCDIA_REQUESTED", "HT2TOPD_REQUESTED", "ERRFLAG", "CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG",
  "DIB", "STEMHT"
)

.read_r10r4_fixture <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  header <- gsub("^\"|\"$", "", header)
  classes <- rep("NULL", length(header))
  classes[header %in% .r10r4_columns] <- NA
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}

.r10r4_fixture_path <- function(root, family, precision) {
  name <- paste0(family, ".", precision, ".csv.gz")
  paths <- c(file.path(root, "full", name), file.path(root, name))
  found <- paths[file.exists(paths)]
  if (length(found))
    found[[1L]] else paths[[1L]]
}

# Measured values proposed for adoption as maintainer tolerances.  They are deliberately
# rounded upward from the maxima in REPORT_R10R4.md.
.r10r4_tolerance <- list(
  diameter_double_rel = 1e-12, height_double_rel = 1e-10, diameter_single_rel = 5e-06,
  height_single_rel = 0.001
)

.expect_r10r4_relative <- function(actual, expected, tolerance) {
  relative <- abs(actual - expected) / pmax(abs(expected), 1e-12)
  expect_true(all(relative <= tolerance), info = paste(
    "maximum relative difference", max(relative,
      na.rm = TRUE
    )
  ))
}

.expect_r10r4_exclusion_counts <- function(masks, expected) {
  expect_named(masks, names(expected))
  expect_identical(
    vapply(masks, function(mask) as.integer(sum(mask)), integer(1L)),
    expected
  )
  membership <- Reduce(`+`, masks)
  expect_true(all(membership <= 1L))
  membership > 0L
}

.r10r4_evaluate_height <- function(data, include) {
  value <- rep(NA_real_, nrow(data))
  status <- integer(nrow(data))
  selected <- which(include)
  if (!length(selected))
    return(data.frame(value = value, status = status))
  # Fixed-grid crossing discovery is intentionally memory bounded here.
  starts <- seq.int(1L, length(selected), by = 128L)
  for (start in starts) {
    rows <- selected[seq.int(start, min(start + 127L, length(selected)))]
    result <- .oracle_height_at_dib(
      dbh = data$DBHOB[rows], ht = data$HTTOT[rows],
      dib = data$STEMDIB[rows], model = data$VOLEQ[rows], spcd = .interface_spcd(
        data$VOLEQ[rows]
      )
    )
    value[rows] <- result$value
    status[rows] <- result$status
  }
  data.frame(value = value, status = status)
}

.check_r10r4_fixture <- function(
  path, family, precision, expected_exclusions, actual = NULL,
  record = FALSE
) {
  fixture <- .read_r10r4_fixture(path)
  input_columns <- c(
    "ROW_ID", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT", "HTUP", "STEMDIB",
    "CALCDIA_REQUESTED",
    "HT2TOPD_REQUESTED"
  )
  inputs <- fixture[, input_columns, drop = FALSE]
  if (!is.null(actual))
    expect_identical(inputs, actual$inputs)

  diameter <- fixture[fixture$CALL_KIND == "PROFILE_PROBE" &
                        fixture$CALCDIA_REQUESTED == "Y" &
                        fixture$ERRFLAG == 0L & fixture$CALCDIA_ERRFLAG == 0L, , drop = FALSE]
  if (identical(family, "r10_taper")) {
    diameter_masks <- list(missing_bru_dispatch = substr(diameter$VOLEQ, 4L, 6L) == "BRU")
  } else {
    diameter_masks <- list(nonzero_errflag = rep(FALSE, nrow(diameter)))
  }
  diameter_excluded <- .expect_r10r4_exclusion_counts(
    diameter_masks,
    expected_exclusions$diameter
  )
  inside <- .oracle_dib(
    dbh = diameter$DBHOB, ht = diameter$HTTOT, h = diameter$HTUP,
    model = diameter$VOLEQ, spcd = .interface_spcd(diameter$VOLEQ)
  )
  expect_true(all(inside$status == 0L))
  .expect_r10r4_relative(
    inside$value[!diameter_excluded], diameter$DIB[!diameter_excluded],
    .r10r4_tolerance[[paste0("diameter_", precision, "_rel")]]
  )
  if (record) {
    diameter_requested <- fixture$CALL_KIND == "PROFILE_PROBE" &
      fixture$CALCDIA_REQUESTED ==
        "Y"
    masks <- c(
      list(nonzero_errflag = sum(diameter_requested) - nrow(diameter)),
      diameter_masks
    )
    .gate1_record_values(
      family, "DIB", precision, inside$value, diameter$DIB, !diameter_excluded,
      .r10r4_tolerance[[paste0("diameter_", precision, "_rel")]], masks
    )
  }

  inverse <- fixture[fixture$CALL_KIND == "PROFILE_PROBE" &
                       fixture$HT2TOPD_REQUESTED == "Y" &
                       fixture$ERRFLAG == 0L & fixture$HT2TOPD_ERRFLAG == 0L, , drop = FALSE]
  if (identical(family, "r10_taper")) {
    bru <- substr(inverse$VOLEQ, 4L, 6L) == "BRU"
    inverse_masks <- list(missing_bru_dispatch = bru, unsupported_dispatch_species = !bru &
                            substr(inverse$VOLEQ, 8L, 10L) %in% c("202", "260"))
  } else {
    inverse_masks <- list(zero_outside_contract = is.finite(inverse$STEMHT) &
                            inverse$STEMHT ==
                              0, nonfinite_oracle = !is.finite(inverse$STEMHT))
  }
  inverse_excluded <- .expect_r10r4_exclusion_counts(
    inverse_masks,
    expected_exclusions$height
  )
  height <- if (is.null(actual)) {
    .r10r4_evaluate_height(inverse, !inverse_excluded)
  } else {
    actual$height
  }
  good_height <- !inverse_excluded
  expect_true(all(height$status[good_height] %in% c(0L, 102L)))
  .expect_r10r4_relative(
    height$value[good_height], inverse$STEMHT[good_height],
    .r10r4_tolerance[[paste0(
      "height_",
      precision, "_rel"
    )]]
  )
  if (identical(family, "r10_taper")) {
    source_omission <- inverse_masks$unsupported_dispatch_species
    compat_height <- .with_treevolume_compat("nvel", .r10r4_evaluate_height(
      inverse,
      source_omission
    ))
    expect_true(all(compat_height$status[source_omission] == 0L))
    expect_identical(compat_height$value[source_omission], inverse$STEMHT[source_omission])
  }
  if (record) {
    inverse_requested <- fixture$CALL_KIND == "PROFILE_PROBE" & fixture$HT2TOPD_REQUESTED ==
      "Y"
    masks <- c(
      list(nonzero_errflag = sum(inverse_requested) - nrow(inverse)),
      inverse_masks
    )
    .gate1_record_values(
      family, "height at DIB", precision, height$value, inverse$STEMHT,
      good_height, .r10r4_tolerance[[paste0("height_", precision, "_rel")]], masks
    )
    if (identical(family, "r10_taper")) {
      .gate1_record_values(family, "height at DIB", precision, compat_height$value,
        inverse$STEMHT,
        source_omission, 0, list(
          nonzero_errflag = sum(inverse_requested) - nrow(inverse),
          outside_omitted_species = !source_omission
        ),
        compat = "nvel"
      )
    }
  }
  if (identical(family, "r4_driver") && any(inverse_masks[[1L]])) {
    zero_result <- .r10r4_evaluate_height(inverse, inverse_masks[[1L]])
    expect_true(all(zero_result$status[inverse_masks[[1L]]] == 101L))
  }

  invisible(list(inputs = inputs, height = height))
}
