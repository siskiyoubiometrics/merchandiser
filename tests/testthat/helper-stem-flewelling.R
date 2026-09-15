.flewelling_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT", "MTOPP",
  "STUMP", "UPSHT1", "UPSHT2", "UPSD1", "UPSD2", "HTUP", "STEMDIB",
  "CALCDIA_REQUESTED", "HT2TOPD_REQUESTED",
  "MRULEMOD", "ERRFLAG", "CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG", "DIB", "DOB", "STEMHT"
)

.read_flewelling_fixture <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  classes <- rep("NULL", length(header))
  classes[header %in% .flewelling_columns] <- NA
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}
.flewelling_fixture_path <- function(root, family, precision) {
  name <- paste0(family, ".", precision, ".csv.gz")
  paths <- c(file.path(root, "full", name), file.path(root, name))
  found <- paths[file.exists(paths)]
  if (length(found))
    found[[1L]] else paths[[1L]]
}

.flewelling_expected_status <- function(primary, operation) {
  flag <- operation
  missing <- is.na(flag) | flag == 0L
  flag[missing] <- primary[missing]
  ifelse(is.na(flag) | flag == 0L, 0L, 300L + as.integer(flag))
}

.fw_invalid_conditioning <- function(data) {
  form <- substr(data$VOLEQ, 4L, 6L)
  invalid <- form %in% c("FW3", "F33") & (data$UPSHT1 >= data$HTTOT |
                                            (data$UPSHT2 > 0 & data$UPSHT2 >=
                                               data$HTTOT))
  invalid[is.na(invalid)] <- FALSE
  invalid
}

.fw_other_group <- function(data) {
  region <- substr(data$VOLEQ, 1L, 1L)
  geosub <- substr(data$VOLEQ, 2L, 3L)
  species <- suppressWarnings(as.integer(substr(data$VOLEQ, 8L, 10L)))
  (region == "2" & species %in% c(122L, 108L, 202L, 15L, 746L)) | (region == "4" & geosub ==
    "07" &
    species %in% c(
      93L,
      122L
    )) |
    (
     region == "3" &
       geosub == "00" &
       species %in% c(
         122L,
         202L
       )) | (region == "3" & geosub == "01" & species %in% c(122L, 108L, 202L, 15L))
}

.expect_fw_exclusion_counts <- function(masks, expected) {
  expect_named(masks, names(expected))
  expect_identical(
    vapply(masks, function(mask) as.integer(sum(mask)), integer(1L)),
    expected
  )
  membership <- Reduce(`+`, masks)
  expect_true(all(membership <= 1L))
  membership > 0L
}

.expect_flewelling_status <- function(actual, expected, allow_not_unique = FALSE) {
  failed <- expected != 0L
  expect_identical(actual[failed], expected[failed])
  successful <- !failed
  allowed <- if (allow_not_unique)
    c(0L, 102L) else 0L
  expect_true(all(actual[successful] %in% allowed))
}

.flewelling_evaluate <- function(data, operation) {
  result <- data.frame(value = rep(NA_real_, nrow(data)), status = integer(nrow(data)))
  if (!nrow(data)) {
    return(result)
  }
  form <- substr(data$VOLEQ, 4L, 6L)
  three_point <- form %in% c("FW3", "F32", "F33")
  second <- three_point & !is.na(data$UPSHT2) & data$UPSHT2 > 0 & !is.na(
    data$UPSD2
  ) & data$UPSD2 >
    0
  groups <- split(seq_len(nrow(data)), interaction(three_point, second))
  batch_size <- if (identical(operation, "height"))
    128L else 4096L
  for (group_rows in groups) {
    starts <- seq.int(1L, length(group_rows), by = batch_size)
    for (start in starts) {
      rows <- group_rows[seq.int(start, min(start + batch_size - 1L, length(group_rows)))]
      arguments <- list(
        dbh = data$DBHOB[rows], ht = data$HTTOT[rows], model = data$VOLEQ[rows],
        spcd = .interface_spcd(data$VOLEQ[rows])
      )
      if (identical(operation, "dib") || identical(operation, "dob")) {
        arguments$h <- data$HTUP[rows]
      } else if (identical(operation, "height")) {
        arguments$dib <- data$STEMDIB[rows]
      }
      if (any(three_point[rows])) {
        f32 <- form[rows] == "F32"
        arguments$upper_ht1 <- ifelse(f32 & (is.na(data$UPSHT1[rows]) | data$UPSHT1[rows] <=
                                               0), 1, data$UPSHT1[rows])
        arguments$upper_d1 <- ifelse(f32 & (is.na(data$UPSD1[rows]) | data$UPSD1[rows] <=
                                              0), 1, data$UPSD1[rows])
        if (any(second[rows])) {
          arguments$upper_ht2 <- data$UPSHT2[rows]
          arguments$upper_d2 <- data$UPSD2[rows]
        }
      }
      evaluated <- do.call(switch(operation,
                             dib = .oracle_dib,
                             dob = .oracle_dob,
                             height = .oracle_height_at_dib
                           ), arguments)
      result[rows, ] <- evaluated
    }
  }
  result
}

.expect_flewelling_relative <- function(actual, expected, tolerance) {
  relative <- abs(actual - expected) / pmax(abs(expected), 1e-12)
  expect_true(all(relative <= tolerance), info = paste(
    "maximum relative difference", max(relative,
      na.rm = TRUE
    )
  ))
}

.check_flewelling_fixture <- function(
  path, precision, expected_exclusions, actual = NULL, record = FALSE,
  expected_compat_precision = 0L
) {
  fixture <- .read_flewelling_fixture(path)
  input_columns <- c(
    "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT", "UPSHT1",
    "UPSHT2", "UPSD1", "UPSD2", "HTUP", "STEMDIB", "CALCDIA_REQUESTED", "HT2TOPD_REQUESTED"
  )
  inputs <- fixture[, input_columns, drop = FALSE]
  if (!is.null(actual)) {
    # The full single and double corpora use identical requests. Reusing the package
    # evaluation avoids repeating the expensive inverse grid while still comparing every
    # row with both oracle builds.
    expect_identical(inputs, actual$inputs)
  }

  diameter_rows <- fixture$CALL_KIND == "PROFILE_PROBE" & fixture$CALCDIA_REQUESTED == "Y"
  diameter <- fixture[diameter_rows, , drop = FALSE]
  expected_status <- .flewelling_expected_status(diameter$ERRFLAG, diameter$CALCDIA_ERRFLAG)
  inside <- if (is.null(actual)) {
    .flewelling_evaluate(diameter, "dib")
  } else {
    actual$inside
  }
  outside <- if (is.null(actual)) {
    .flewelling_evaluate(diameter, "dob")
  } else {
    actual$outside
  }
  failed <- expected_status != 0L
  invalid <- !failed & .fw_invalid_conditioning(diameter)
  excluded <- .expect_fw_exclusion_counts(
    list(nonzero_errflag = failed, upper_at_or_above_total = invalid),
    expected_exclusions$diameter
  )
  for (diameter_result in list(inside, outside)) {
    expect_identical(diameter_result$status[failed], expected_status[failed])
    expect_true(all(is.na(diameter_result$value[failed])))
    expect_identical(diameter_result$status[invalid], rep(54L, sum(invalid)))
    expect_true(all(is.na(diameter_result$value[invalid])))
    .expect_flewelling_status(diameter_result$status[!excluded], expected_status[!excluded])
  }
  good <- !excluded
  .expect_flewelling_relative(inside$value[good], diameter$DIB[good], tv_tolerance[[paste0(
    "diameter_",
    precision, "_rel"
  )]])
  .expect_flewelling_relative(outside$value[good], diameter$DOB[good], tv_tolerance[[paste0(
    "diameter_",
    precision, "_rel"
  )]])
  if (record) {
    family <- unique(fixture$FAMILY)
    masks <- list(nonzero_errflag = failed, upper_at_or_above_total = invalid)
    tolerance <- tv_tolerance[[paste0("diameter_", precision, "_rel")]]
    .gate1_record_values(
      family, "DIB", precision, inside$value, diameter$DIB, good, tolerance,
      masks
    )
    .gate1_record_values(
      family, "DOB", precision, outside$value, diameter$DOB, good, tolerance,
      masks
    )
  }

  height_rows <- fixture$CALL_KIND == "PROFILE_PROBE" & fixture$HT2TOPD_REQUESTED == "Y"
  inverse <- fixture[height_rows, , drop = FALSE]
  expected_status <- .flewelling_expected_status(inverse$ERRFLAG, inverse$HT2TOPD_ERRFLAG)
  height <- if (is.null(actual)) {
    .flewelling_evaluate(inverse, "height")
  } else {
    actual$height
  }
  compat_height <- if (is.null(actual)) {
    .with_treevolume_compat("nvel", .flewelling_evaluate(inverse, "height"))
  } else {
    actual$compat_height
  }
  failed <- expected_status != 0L
  form <- substr(inverse$VOLEQ, 4L, 6L)
  invalid <- !failed & .fw_invalid_conditioning(inverse)
  # sf_hs.f lines 109 through 121 pass HI2, rather than the trial height H, to BRK_UP for
  # the JSP 22 to 29 inside-bark conversion.
  fixed_bark_height <- !failed & !invalid & form == "FW2" & .fw_other_group(inverse)
  # sf_hs.f uses a Newton search that can select a lower or approximate root for
  # conditioned profiles. The package contract requires the highest root.
  conditioned_sf_hs <- !failed & !invalid & !fixed_bark_height & form %in% c("FW3", "F33")
  # The package inverse interval starts at the model's 1-foot stump.
  outside_contract_interval <- !failed & !invalid & !fixed_bark_height &
    !conditioned_sf_hs &
    is.finite(inverse$STEMHT) & inverse$STEMHT <= 1
  excluded <- .expect_fw_exclusion_counts(
    list(
      nonzero_errflag = failed, upper_at_or_above_total = invalid,
      sf_hs_bark_height = fixed_bark_height,
      sf_hs_conditioned_root = conditioned_sf_hs,
      at_or_below_contract_stump = outside_contract_interval
    ),
    expected_exclusions$height
  )
  expect_identical(height$status[failed], expected_status[failed])
  expect_true(all(is.na(height$value[failed])))
  expect_identical(height$status[invalid], rep(54L, sum(invalid)))
  expect_true(all(is.na(height$value[invalid])))
  expect_identical(height$status[outside_contract_interval], rep(101L, sum(
    outside_contract_interval
  )))
  expect_true(all(is.na(height$value[outside_contract_interval])))
  expect_true(all(height$status[fixed_bark_height] %in% c(0L, 101L, 102L)))
  fixed_out_of_range <- fixed_bark_height & height$status == 101L
  expect_true(all(is.na(height$value[fixed_out_of_range])))
  if (any(fixed_out_of_range)) {
    stump_rows <- inverse[fixed_out_of_range, , drop = FALSE]
    stump_rows$HTUP <- 1
    stump_diameter <- .flewelling_evaluate(stump_rows, "dib")
    expect_identical(stump_diameter$status, integer(nrow(stump_rows)))
    expect_true(all(stump_rows$STEMDIB > stump_diameter$value))
  }
  expect_true(all(height$status[conditioned_sf_hs] %in% c(0L, 102L)))
  root <- (fixed_bark_height | conditioned_sf_hs) & height$status %in% c(0L, 102L)
  expect_true(all(is.finite(height$value[root])))
  if (any(root)) {
    roundtrip <- inverse[root, , drop = FALSE]
    roundtrip$HTUP <- height$value[root]
    diameter_at_height <- .flewelling_evaluate(roundtrip, "dib")
    expect_identical(diameter_at_height$status, integer(nrow(diameter_at_height)))
    .expect_flewelling_relative(
      diameter_at_height$value, roundtrip$STEMDIB,
      tv_tolerance[[paste0(
        "diameter_",
        precision, "_rel"
      )]]
    )
  }
  .expect_flewelling_status(height$status[!excluded], expected_status[!excluded],
    allow_not_unique = TRUE
  )
  good <- !excluded & is.finite(inverse$STEMHT)
  .expect_flewelling_relative(
    height$value[good], inverse$STEMHT[good],
    tv_tolerance[[paste0(
      "height_",
      precision, "_rel"
    )]]
  )
  compat_affected <- fixed_bark_height | conditioned_sf_hs
  reference_compat <- if (is.null(actual)) {
    inverse$STEMHT
  } else {
    actual$compat_oracle
  }
  compatibility_tolerance <- tv_tolerance[[paste0("height_", precision, "_rel")]]
  oracle_precision_difference <- abs(reference_compat - inverse$STEMHT) / pmax(
    abs(inverse$STEMHT),
    1e-12
  )
  compat_precision_boundary <- compat_affected & precision == "single" & is.finite(
    oracle_precision_difference
  ) &
    oracle_precision_difference > compatibility_tolerance
  expect_identical(sum(compat_precision_boundary), expected_compat_precision)
  compat_good <- compat_affected & is.finite(inverse$STEMHT) & !compat_precision_boundary
  expect_true(all(compat_height$status[compat_good] == 0L))
  .expect_flewelling_relative(
    compat_height$value[compat_good], inverse$STEMHT[compat_good],
    compatibility_tolerance
  )
  if (record) {
    masks <- list(
      nonzero_errflag = failed, upper_at_or_above_total = invalid,
      sf_hs_bark_height = fixed_bark_height,
      sf_hs_conditioned_root = conditioned_sf_hs,
      at_or_below_contract_stump = outside_contract_interval
    )
    .gate1_record_values(
      unique(fixture$FAMILY), "height at DIB", precision, height$value,
      inverse$STEMHT, good, tv_tolerance[[paste0("height_", precision, "_rel")]], masks
    )
    .gate1_record_values(unique(fixture$FAMILY), "height at DIB", precision,
      compat_height$value,
      inverse$STEMHT, compat_good, tv_tolerance[[paste0("height_", precision, "_rel")]],
      list(
        outside_compatibility_ruling = !fixed_bark_height & !conditioned_sf_hs,
        nonfinite_oracle = (fixed_bark_height |
                              conditioned_sf_hs) & !is.finite(inverse$STEMHT),
        single_precision_boundary = compat_precision_boundary
      ),
      compat = "nvel"
    )
  }

  invisible(list(
    inputs = inputs, inside = inside, outside = outside, height = height,
    compat_height = compat_height,
    compat_oracle = inverse$STEMHT
  ))
}
