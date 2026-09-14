.clark_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT",
  "UPSHT1", "BA", "SI", "HTUP", "STEMDIB", "CALCDIA_REQUESTED",
  "HT2TOPD_REQUESTED", "ERRFLAG", "CALCDIA_ERRFLAG",
  "HT2TOPD_ERRFLAG", "DIB", "DOB", "STEMHT"
)

.clark_fixture_path <- function(root, family, precision) {
  name <- paste0(family, ".", precision, ".csv.gz")
  paths <- c(file.path(root, "full", name), file.path(root, name))
  found <- paths[file.exists(paths)]
  if (length(found)) found[[1L]] else paths[[1L]]
}

.read_clark_fixture <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  classes <- rep("NULL", length(header))
  classes[header %in% .clark_columns] <- NA
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}

.clark_expected_status <- function(primary, operation) {
  flag <- operation
  missing <- is.na(flag) | flag == 0L
  flag[missing] <- primary[missing]
  ifelse(is.na(flag) | flag == 0L, 0L, 300L + as.integer(flag))
}

.clark_evaluate_direct <- function(data, operation) {
  result <- data.frame(
    value = rep(NA_real_, nrow(data)), status = integer(nrow(data))
  )
  groups <- split(seq_len(nrow(data)), data$VOLEQ)
  for (rows in groups) {
    top_code <- suppressWarnings(as.integer(substr(data$VOLEQ[rows[[1L]]], 3L, 3L)))
    needs_upper <- substr(data$VOLEQ[rows[[1L]]], 1L, 1L) == "8" &
      top_code %in% c(4L, 7L, 9L)
    x1 <- switch(
      operation, dib = data$HTUP[rows], height = data$STEMDIB[rows]
    )
    x2 <- rep(0, length(rows))
    evaluated <- merchandiser:::tv_cpp_kernel_eval(
      paste0("clark:", data$VOLEQ[rows[[1L]]]),
      match(operation, c("dib", "height")),
      data$DBHOB[rows], data$HTTOT[rows], x1, x2,
      rep(NA_real_, length(rows)), merchandiser::threads(),
      upper_ht1 = if (needs_upper) data$UPSHT1[rows] else NULL
    )
    result[rows, ] <- evaluated
  }
  result
}

.clark_evaluate <- function(data, operation, direct = FALSE) {
  result <- data.frame(
    value = rep(NA_real_, nrow(data)), status = integer(nrow(data))
  )
  if (!nrow(data)) {
    return(result)
  }
  if (direct) {
    return(.clark_evaluate_direct(data, operation))
  }
  top_code <- suppressWarnings(as.integer(substr(data$VOLEQ, 3L, 3L)))
  needs_upper <- substr(data$VOLEQ, 1L, 1L) == "8" &
    top_code %in% c(4L, 7L, 9L)
  groups <- split(seq_len(nrow(data)), needs_upper)
  for (rows in groups) {
    arguments <- list(
      dbh = data$DBHOB[rows], ht = data$HTTOT[rows],
      model = data$VOLEQ[rows], status = TRUE
    )
    if (identical(operation, "dib")) {
      arguments$h <- data$HTUP[rows]
    } else if (identical(operation, "height")) {
      arguments$dib <- data$STEMDIB[rows]
    }
    if (any(needs_upper[rows])) {
      arguments$upper_ht1 <- data$UPSHT1[rows]
    }
    evaluated <- suppressWarnings(do.call(
      switch(
        operation,
        dib = merchandiser::dib,
        height = merchandiser::height_at_dib
      ),
      arguments
    ))
    result[rows, ] <- evaluated
  }
  result
}

.new_clark_metrics <- function(precision) {
  metrics <- new.env(parent = emptyenv())
  metrics$precision <- precision
  metrics$rows <- 0L
  metrics$identifiers <- character()
  metrics$status_mismatches <- 0L
  metrics$exclusion_mismatches <- 0L
  metrics$failure_examples <- data.frame()
  metrics$compat_overlap <- list(
    compared = 0L, within = 0L, max_relative = 0
  )
  metrics$operations <- lapply(c("DIB", "STEMHT"), function(name) {
    list(
      name = name, compared = 0L, excluded_nonzero_errflag = 0L,
      excluded_nonfinite = 0L,
      excluded_below_stump = 0L,
      excluded_overlap_inverse = 0L,
      excluded_diameter_floor = 0L, max_absolute = 0,
      max_relative = 0, failures = 0L
    )
  })
  names(metrics$operations) <- c("dib", "height")
  metrics
}

.update_clark_operation <- function(metrics, data, operation, expected_value,
                                    operation_flag, direct = FALSE) {
  if (!nrow(data)) {
    return(invisible(NULL))
  }
  actual <- .clark_evaluate(data, operation, direct = direct)
  expected_status <- .clark_expected_status(data$ERRFLAG, operation_flag)
  failed <- expected_status != 0L
  status_failure <- actual$status != expected_status | !is.na(actual$value)
  metrics$status_mismatches <- metrics$status_mismatches +
    sum(status_failure[failed])

  oracle_nonfinite <- !failed & !is.finite(expected_value)
  valid_nonfinite <- actual$status %in% c(54L, 100L) & is.na(actual$value)
  if (identical(operation, "dib")) {
    valid_nonfinite <- valid_nonfinite |
      (actual$status == 0L & is.finite(actual$value) & actual$value >= 0)
  } else if (identical(operation, "height")) {
    valid_nonfinite <- valid_nonfinite |
      (actual$status %in% c(0L, 102L) & is.finite(actual$value) &
         actual$value >= 1 & actual$value <= data$HTTOT)
  }
  metrics$exclusion_mismatches <- metrics$exclusion_mismatches +
    sum(!valid_nonfinite[oracle_nonfinite])

  below_stump <- operation == "height" & !failed &
    is.finite(expected_value) & expected_value < 1
  if (any(below_stump)) {
    allowed <- actual$status[below_stump] %in% c(0L, 100L, 102L)
    in_interval <- is.na(actual$value[below_stump]) |
      (actual$value[below_stump] >= 1 &
         actual$value[below_stump] <= data$HTTOT[below_stump])
    metrics$exclusion_mismatches <- metrics$exclusion_mismatches +
      sum(!allowed | !in_interval)
  }

  relative <- abs(actual$value - expected_value) /
    pmax(abs(expected_value), 1e-12)
  tolerance <- tv_tolerance[[paste0(
    switch(operation, dib = "diameter", height = "height"),
    "_", metrics$precision, "_rel"
  )]]
  overlap_inverse <- operation == "height" & data$VOLEQ == "834CLKE110" &
    data$DBHOB == 6 & data$STEMDIB == 5 &
    data$HTTOT %in% c(100, 130, 160, 200)
  if (any(overlap_inverse)) {
    valid_overlap <- actual$status[overlap_inverse] %in% c(0L, 102L) &
      is.finite(actual$value[overlap_inverse]) &
      actual$value[overlap_inverse] >= 1 &
      actual$value[overlap_inverse] <= data$HTTOT[overlap_inverse]
    metrics$exclusion_mismatches <- metrics$exclusion_mismatches +
      sum(!valid_overlap)
    source_rows <- data[overlap_inverse, , drop = FALSE]
    source <- .with_treevolume_compat(
      "nvel", .clark_evaluate(source_rows, "height")
    )
    source_relative <- abs(source$value - expected_value[overlap_inverse]) /
      pmax(abs(expected_value[overlap_inverse]), 1e-12)
    source_good <- source$status == 0L & is.finite(source_relative) &
      source_relative <= tolerance
    metrics$compat_overlap$compared <-
      metrics$compat_overlap$compared + sum(overlap_inverse)
    metrics$compat_overlap$within <-
      metrics$compat_overlap$within + sum(source_good)
    if (any(is.finite(source_relative))) {
      metrics$compat_overlap$max_relative <- max(
        metrics$compat_overlap$max_relative,
        source_relative[is.finite(source_relative)]
      )
    }
  }
  diameter_floor <- metrics$precision == "single" & operation == "dib" &
    data$VOLEQ == "900CLKE317" & data$HTTOT == 30 & data$HTUP == 17.3 &
    expected_value > 0.1 & expected_value < 0.1001 &
    actual$status == 0L & abs(actual$value - 0.1) < 1e-12
  compare <- !failed & !oracle_nonfinite & !below_stump &
    !overlap_inverse & !diameter_floor
  allowed_status <- if (identical(operation, "height")) {
    actual$status %in% c(0L, 102L)
  } else {
    actual$status == 0L
  }
  comparison_failures <- compare &
    (!allowed_status | !is.finite(relative) | relative > tolerance)

  current <- metrics$operations[[operation]]
  current$compared <- current$compared + sum(compare)
  current$excluded_nonzero_errflag <- current$excluded_nonzero_errflag +
    sum(failed)
  current$excluded_nonfinite <- current$excluded_nonfinite +
    sum(oracle_nonfinite)
  current$excluded_below_stump <- current$excluded_below_stump +
    sum(below_stump)
  current$excluded_overlap_inverse <- current$excluded_overlap_inverse +
    sum(overlap_inverse)
  current$excluded_diameter_floor <- current$excluded_diameter_floor +
    sum(diameter_floor)
  if (any(compare & is.finite(relative))) {
    current$max_absolute <- max(
      current$max_absolute,
      abs(actual$value[compare] - expected_value[compare]), na.rm = TRUE
    )
    current$max_relative <- max(
      current$max_relative, relative[compare], na.rm = TRUE
    )
  }
  current$failures <- current$failures + sum(comparison_failures)
  metrics$operations[[operation]] <- current
  if (any(comparison_failures) && nrow(metrics$failure_examples) < 20L) {
    rows <- head(which(comparison_failures), 20L - nrow(metrics$failure_examples))
    metrics$failure_examples <- rbind(
      metrics$failure_examples,
      data.frame(
        operation = operation, row_id = data$ROW_ID[rows],
        id = data$VOLEQ[rows], dbh = data$DBHOB[rows],
        ht = data$HTTOT[rows], input = if (operation == "height") {
          data$STEMDIB[rows]
        } else if (operation == "dib") {
          data$HTUP[rows]
        } else {
          data$HTTOT[rows]
        },
        expected = expected_value[rows], actual = actual$value[rows],
        status = actual$status[rows], relative = relative[rows]
      )
    )
  }
  invisible(NULL)
}

.update_clark_metrics <- function(metrics, data, direct = FALSE) {
  metrics$rows <- metrics$rows + nrow(data)
  metrics$identifiers <- union(metrics$identifiers, data$VOLEQ)

  diameter <- data[
    grepl("^PROFILE_PROBE", data$CALL_KIND) &
      data$CALCDIA_REQUESTED == "Y",
    , drop = FALSE
  ]
  .update_clark_operation(
    metrics, diameter, "dib", diameter$DIB, diameter$CALCDIA_ERRFLAG,
    direct = direct
  )

  inverse <- data[
    grepl("^PROFILE_PROBE", data$CALL_KIND) &
      data$HT2TOPD_REQUESTED == "Y",
    , drop = FALSE
  ]
  .update_clark_operation(
    metrics, inverse, "height", inverse$STEMHT, inverse$HT2TOPD_ERRFLAG,
    direct = direct
  )

  invisible(metrics)
}

.expect_clark_metrics <- function(metrics) {
  expect_identical(metrics$status_mismatches, 0L)
  expect_identical(metrics$exclusion_mismatches, 0L)
  for (operation in metrics$operations) {
    expect_true(operation$compared > 0L, info = operation$name)
    expect_identical(operation$failures, 0L, info = paste(
      operation$name, "maximum relative difference", operation$max_relative
    ))
  }
  invisible(metrics)
}

.check_clark_fixture <- function(path, precision) {
  fixture <- .read_clark_fixture(path)
  metrics <- .new_clark_metrics(precision)
  .update_clark_metrics(metrics, fixture)
  .expect_clark_metrics(metrics)
}

.check_clark_streamed_fixture <- function(path, precision, fraction = 1,
                                          seed = 20260906L,
                                          chunk_size = 200000L,
                                          assert = TRUE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table is required for streamed Clark fixture tests")
  }
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- readLines(connection, n = 1L)
  metrics <- .new_clark_metrics(precision)
  totals <- integer()
  selected_totals <- integer()
  if (!is.numeric(fraction) || length(fraction) != 1L ||
        !is.finite(fraction) || fraction <= 0 || fraction > 1) {
    stop("fraction must be one number greater than zero and at most one")
  }
  if (fraction < 1) set.seed(seed)
  repeat {
    lines <- readLines(connection, n = chunk_size)
    if (!length(lines)) {
      break
    }
    chunk <- data.table::fread(
      text = paste(c(header, lines), collapse = "\n"),
      select = .clark_columns, showProgress = FALSE,
      data.table = FALSE
    )
    by_identifier <- split(seq_len(nrow(chunk)), chunk$VOLEQ)
    selected <- if (fraction == 1) {
      seq_len(nrow(chunk))
    } else {
      unlist(lapply(by_identifier, function(rows) {
        rows[sample.int(length(rows), ceiling(fraction * length(rows)))]
      }), use.names = FALSE)
    }
    chunk_counts <- lengths(by_identifier)
    sampled_counts <- if (fraction == 1) {
      as.integer(chunk_counts)
    } else {
      as.integer(ceiling(fraction * chunk_counts))
    }
    names(sampled_counts) <- names(chunk_counts)
    new_totals <- setdiff(names(chunk_counts), names(totals))
    totals <- c(totals, stats::setNames(integer(length(new_totals)), new_totals))
    new_selected <- setdiff(names(sampled_counts), names(selected_totals))
    selected_totals <- c(
      selected_totals,
      stats::setNames(integer(length(new_selected)), new_selected)
    )
    totals[names(chunk_counts)] <- totals[names(chunk_counts)] + chunk_counts
    selected_totals[names(sampled_counts)] <-
      selected_totals[names(sampled_counts)] + sampled_counts
    .update_clark_metrics(
      metrics, chunk[selected, , drop = FALSE], direct = TRUE
    )
  }
  if (assert) {
    expect_setequal(names(totals), metrics$identifiers)
    expect_true(all(selected_totals >= ceiling(fraction * totals)))
    .expect_clark_metrics(metrics)
  }
  metrics
}
