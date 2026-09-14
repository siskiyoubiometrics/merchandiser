.nvel_volume_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "REGN", "FORST", "DIST", "FIASPCD",
  "PROD", "VOLEQ", "DBHOB", "HTTOT", "MTOPP", "MTOPS", "STUMP",
  "UPSHT1", "UPSHT2", "UPSD1", "UPSD2", "FCLASS", "CTYPE", "LIVE",
  "CULL", "MRULEMOD", "NEWMAXLEN", "NEWMINLEN", "NEWTRIM", "ERRFLAG",
  paste0("VOL", 1:15), "NOLOGP", "NOLOGS"
)

.read_nvel_volume_fixture <- function(path, family = NULL) {
  connection <- if (grepl("[.]gz$", path)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  header <- gsub('^"|"$', "", header)
  classes <- rep("NULL", length(header))
  classes[header %in% .nvel_volume_columns] <- NA
  fixture <- utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
  if (!"FAMILY" %in% names(fixture)) {
    fixture$FAMILY <- if (is.null(family)) {
      sub("[.].*$", "", basename(path))
    } else {
      family
    }
  }
  for (name in c("UPSHT1", "UPSHT2", "UPSD1", "UPSD2", "FCLASS")) {
    if (!name %in% names(fixture)) fixture[[name]] <- 0
  }
  size <- nrow(fixture)
  inferred_region <- suppressWarnings(as.integer(substr(fixture$VOLEQ, 1L, 1L)))
  inferred_region[substr(fixture$VOLEQ, 1L, 1L) == "A"] <- 10L
  inferred_region[is.na(inferred_region)] <- 0L
  defaults <- list(
    REGN = inferred_region, FORST = rep(0, size), DIST = rep(0, size),
    FIASPCD = suppressWarnings(as.integer(substr(fixture$VOLEQ, 8L, 10L))),
    PROD = rep(1, size), MTOPP = rep(6, size), MTOPS = rep(4, size),
    STUMP = rep(1, size), CTYPE = rep("C", size), LIVE = rep("L", size),
    CULL = rep(0, size), MRULEMOD = rep("N", size),
    NEWMAXLEN = rep(NA_real_, size), NEWMINLEN = rep(NA_real_, size),
    NEWTRIM = rep(NA_real_, size), NOLOGP = rep(NA_real_, size),
    NOLOGS = rep(NA_real_, size)
  )
  for (name in paste0("VOL", 1:15)) defaults[[name]] <- rep(NA_real_, size)
  for (name in names(defaults)) {
    if (!name %in% names(fixture)) fixture[[name]] <- defaults[[name]]
  }
  volume_rows <- fixture$CALL_KIND %in% c(
    "VOLUMELIBRARY", "VOLUMELIBRARY_MRULE"
  )
  if (!any(volume_rows)) volume_rows <- is.finite(fixture$VOL1)
  fixture[volume_rows, , drop = FALSE]
}

.nvel_fixture_division <- function(id) {
  as.integer(substr(id, 5L, 7L)) +
    ifelse(substr(id, 4L, 4L) == "M", 1000L, 0L)
}

.nvel_fixture_rules <- function(data) {
  modified <- !is.na(data$MRULEMOD) & data$MRULEMOD == "Y"
  merchandiser:::nvel_rules(
    maximum_length = ifelse(modified, data$NEWMAXLEN, NA_real_),
    minimum_length = ifelse(modified, data$NEWMINLEN, NA_real_),
    primary_top = data$MTOPP, secondary_top = data$MTOPS,
    stump = data$STUMP,
    trim = ifelse(modified, data$NEWTRIM, NA_real_),
    prod = sprintf("%02d", as.integer(data$PROD)),
    live = ifelse(is.na(data$LIVE) | !nzchar(data$LIVE), "L", data$LIVE),
    ctype = ifelse(is.na(data$CTYPE) | !nzchar(data$CTYPE), "C", data$CTYPE),
    cull = ifelse(is.na(data$CULL), 0, data$CULL),
    forest = data$FORST, district = data$DIST
  )
}

.nvel_fixture_groups <- function(data) {
  form <- substr(data$VOLEQ, 4L, 6L)
  three <- form %in% c("FW3", "F32", "F33")
  second <- three & data$UPSHT2 > 0 & data$UPSD2 > 0
  clark_upper <- data$FAMILY == "clark_r8" &
    substr(data$VOLEQ, 3L, 3L) %in% c("4", "7", "9")
  cz3_upper <- form == "CZ3" & data$UPSHT1 > 0 & data$UPSD1 > 0
  form_class <- data$FAMILY %in% c("blm_taper", "behre_taper") &
    data$FCLASS > 0
  interaction(
    data$VOLEQ, three, second, clark_upper, cz3_upper, form_class,
    drop = TRUE
  )
}

.evaluate_nvel_volume_fixture <- function(data) {
  logs <- lapply(split(seq_len(nrow(data)), .nvel_fixture_groups(data)),
    function(rows) {
      selected <- data[rows, , drop = FALSE]
      form <- substr(selected$VOLEQ[[1L]], 4L, 6L)
      arguments <- list(
        dbh = selected$DBHOB, ht = selected$HTTOT,
        model = selected$VOLEQ, rules = .nvel_fixture_rules(selected),
        spcd = selected$FIASPCD, region = selected$REGN, status = TRUE
      )
      if (identical(selected$FAMILY[[1L]], "nsvb")) {
        arguments$division <- .nvel_fixture_division(selected$VOLEQ)
      }
      if (form %in% c("FW3", "F32", "F33")) {
        arguments$upper_ht1 <- selected$UPSHT1
        arguments$upper_d1 <- selected$UPSD1
        if (form == "F32") {
          arguments$upper_ht1[arguments$upper_ht1 <= 0] <- 1
          arguments$upper_d1[arguments$upper_d1 <= 0] <- 1
        }
        if (selected$UPSHT2[[1L]] > 0 && selected$UPSD2[[1L]] > 0) {
          arguments$upper_ht2 <- selected$UPSHT2
          arguments$upper_d2 <- selected$UPSD2
        }
      }
      if (selected$FAMILY[[1L]] == "clark_r8" &&
            substr(selected$VOLEQ[[1L]], 3L, 3L) %in% c("4", "7", "9")) {
        arguments$upper_ht1 <- selected$UPSHT1
      }
      if (form == "CZ3" && selected$UPSHT1[[1L]] > 0 &&
            selected$UPSD1[[1L]] > 0) {
        arguments$upper_ht1 <- selected$UPSHT1
        arguments$upper_d1 <- selected$UPSD1
      }
      if (selected$FAMILY[[1L]] %in% c("blm_taper", "behre_taper") &&
            selected$FCLASS[[1L]] > 0) {
        arguments$form_class <- selected$FCLASS
      }
      evaluated <- do.call(merchandiser:::nvel_volume, arguments)
      evaluated$.fixture_row <- rows
      evaluated
    }
  )
  result <- do.call(rbind, logs)
  result <- result[order(result$.fixture_row), , drop = FALSE]
  result$.fixture_row <- NULL
  rownames(result) <- NULL
  result
}

.nvel_relative_failures <- function(actual, expected, include, tolerance) {
  difference <- abs(actual - expected) / pmax(abs(expected), 1)
  sum(include & (!is.finite(difference) | difference > tolerance))
}

.nvel_volume_comparisons <- function(data, observed, precision) {
  tolerance <- tv_tolerance[[paste0("volume_", precision, "_rel")]]
  valid <- data$ERRFLAG == 0L
  form <- substr(data$VOLEQ, 4L, 6L)
  invalid_conditioning <- data$FAMILY %in% c(
    "flewelling_2pt", "flewelling_3pt"
  ) & .fw_invalid_conditioning(data)
  fwsmall <- data$FAMILY == "flewelling_2pt" &
    .fw_other_group(data) & data$HTTOT <= 15
  missing_bru <- data$FAMILY == "r10_taper" & form == "BRU" &
    !substr(data$VOLEQ, 1L, 3L) %in% c("A01", "A02") &
    data$DBHOB >= 9 & data$HTTOT > 40
  total_difference <- abs(observed$vol_total_cu - data$VOL1) /
    pmax(abs(data$VOL1), 1)
  single_rounding <- precision == "single" &
    data$FAMILY %in% c("clark_r8", "clark_r9") &
    is.finite(total_difference) & total_difference > tolerance &
    abs(observed$vol_total_cu - data$VOL1) <= 0.100001
  total_excluded <- invalid_conditioning | fwsmall | missing_bru |
    single_rounding
  status_exception <- data$FAMILY %in% c("clark_r8", "clark_r9") &
    data$ERRFLAG == 12L & observed$errflag == 0L
  total_include <- valid & observed$errflag == 0L & !total_excluded

  stump_candidate <- valid & observed$errflag == 0L & !fwsmall &
    is.finite(data$VOL14) & is.finite(observed$vol_stump_cu)
  stump_difference <- abs(observed$vol_stump_cu - data$VOL14) /
    pmax(abs(data$VOL14), 1)
  clark_r9_r9cuft <- stump_candidate & data$FAMILY == "clark_r9"
  single_r9cuft_cancellation <- stump_candidate & precision == "single" &
    data$VOLEQ == "900CLKE823"
  stump_driver_difference <- stump_candidate & !clark_r9_r9cuft &
    stump_difference > tolerance
  stump_include <- stump_candidate & !stump_driver_difference &
    !single_r9cuft_cancellation

  nsvb <- data$FAMILY == "nsvb"
  tip_include <- valid & observed$errflag == 0L & nsvb &
    is.finite(data$VOL15)
  tip_segmentation <- valid & observed$errflag == 0L & !nsvb

  list(
    tolerance = tolerance,
    valid = valid,
    invalid_conditioning = invalid_conditioning,
    fwsmall = fwsmall,
    missing_bru = missing_bru,
    single_rounding = single_rounding,
    total_excluded = total_excluded,
    status_exception = status_exception,
    total_include = total_include,
    stump_candidate = stump_candidate,
    clark_r9_r9cuft = clark_r9_r9cuft,
    single_r9cuft_cancellation = single_r9cuft_cancellation,
    stump_driver_difference = stump_driver_difference,
    stump_include = stump_include,
    tip_include = tip_include,
    tip_segmentation = tip_segmentation
  )
}

.nvel_volume_metrics <- function(data, observed, precision) {
  comparison <- .nvel_volume_comparisons(data, observed, precision)
  tolerance <- comparison$tolerance
  valid <- comparison$valid

  c(
    rows = nrow(data), valid = sum(valid),
    status_exceptions = sum(comparison$status_exception),
    status_failures = sum(
      observed$errflag != data$ERRFLAG & !comparison$status_exception &
        !comparison$total_excluded
    ),
    total_excluded = sum(valid & comparison$total_excluded),
    total_compared = sum(comparison$total_include),
    total_failures = .nvel_relative_failures(
      observed$vol_total_cu, data$VOL1, comparison$total_include, tolerance
    ),
    stump_driver_excluded = sum(comparison$stump_driver_difference),
    stump_precision_excluded = sum(comparison$single_r9cuft_cancellation),
    stump_compared = sum(comparison$stump_include),
    stump_failures = .nvel_relative_failures(
      observed$vol_stump_cu, data$VOL14, comparison$stump_include, tolerance
    ),
    tip_segmentation_excluded = sum(comparison$tip_segmentation),
    tip_compared = sum(comparison$tip_include),
    tip_failures = .nvel_relative_failures(
      observed$vol_tip_cu, data$VOL15, comparison$tip_include, tolerance
    )
  )
}

.check_nvel_volume_fixture <- function(path, family, precision,
                                       expected_metrics = NULL,
                                       record = FALSE) {
  data <- .read_nvel_volume_fixture(path, family)
  expect_true(nrow(data) > 0L, info = path)
  expect_true(all(data$FAMILY == family))
  observed <- .evaluate_nvel_volume_fixture(data)
  comparison <- .nvel_volume_comparisons(data, observed, precision)
  metrics <- .nvel_volume_metrics(data, observed, precision)
  if (!is.null(expected_metrics)) expect_identical(metrics, expected_metrics)
  expect_identical(unname(metrics[["status_failures"]]), 0L)
  expect_identical(unname(metrics[["total_failures"]]), 0L)
  expect_identical(unname(metrics[["stump_failures"]]), 0L)
  expect_identical(unname(metrics[["tip_failures"]]), 0L)
  if (identical(family, "clark_r9")) {
    source_rows <- data$ERRFLAG == 12L
    source_observed <- if (any(source_rows)) {
      .with_treevolume_compat(
        "nvel", .evaluate_nvel_volume_fixture(data[source_rows, , drop = FALSE])
      )
    } else {
      data.frame(errflag = integer())
    }
    expect_identical(sum(source_rows), unname(metrics[["status_exceptions"]]))
    expect_true(all(source_observed$errflag == 12L))
    if (any(source_rows)) {
      source_values <- source_observed[merchandiser:::.nvel_volume_names]
      expect_true(all(is.na(as.matrix(source_values))))
    }
  }
  valid <- data$ERRFLAG == 0L & observed$errflag == 0L
  if (any(valid)) {
    merch_status <- observed[valid, paste0(
      merchandiser:::.nvel_volume_names[2:9], "_status"
    ), drop = FALSE]
    expect_true(all(as.matrix(merch_status) == 53L))
    expect_true(all(is.na(as.matrix(observed[
      valid, merchandiser:::.nvel_volume_names[2:9], drop = FALSE
    ]))))
  }
  if (record) {
    base_masks <- list(
      library_nonzero_errflag = !comparison$valid &
        !comparison$status_exception,
      library_error_12_with_available_total = comparison$status_exception
    )
    .gate1_record_values(
      family, "total cubic volume", precision,
      observed$vol_total_cu, data$VOL1, comparison$total_include,
      comparison$tolerance,
      c(base_masks, list(
        invalid_conditioning = comparison$invalid_conditioning,
        fwsmall_uninitialized_state = comparison$fwsmall,
        missing_bru_dispatch = comparison$missing_bru,
        single_rounding_boundary = comparison$single_rounding
      )),
      denominator_floor = 1
    )
    if (identical(family, "clark_r9")) {
      .gate1_record_summary(
        family, "R9LOGS error 12", precision,
        sum(source_rows), sum(source_observed$errflag == 12L),
        0, 0,
        list(outside_error_12 = nrow(data) - sum(source_rows)),
        compat = "nvel"
      )
    }
    missing_stump <- comparison$valid & observed$errflag == 0L &
      !comparison$fwsmall & !comparison$stump_candidate
    .gate1_record_values(
      family, "stump cubic volume", precision,
      observed$vol_stump_cu, data$VOL14, comparison$stump_include,
      comparison$tolerance,
      c(base_masks, list(
        fwsmall_uninitialized_state = comparison$fwsmall,
        library_or_port_stump_unavailable = missing_stump,
        single_precision_r9cuft_cancellation =
          comparison$single_r9cuft_cancellation,
        output_derived_driver_difference =
          comparison$stump_driver_difference
      )),
      denominator_floor = 1
    )
    .gate1_record_values(
      family, "tip cubic volume", precision,
      observed$vol_tip_cu, data$VOL15, comparison$tip_include,
      comparison$tolerance,
      c(base_masks, list(
        segmentation_not_available = comparison$tip_segmentation
      )),
      denominator_floor = 1
    )
  }
  invisible(metrics)
}
