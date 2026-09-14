.smalltaper_columns <- c(
  "ROW_ID", "FAMILY", "CALL_KIND", "VOLEQ", "DBHOB", "HTTOT", "HTUP",
  "STEMDIB", "CALCDIA_REQUESTED", "HT2TOPD_REQUESTED", "ERRFLAG",
  "CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG", "DIB", "STEMHT"
)

.smalltaper_tolerance <- list(
  diameter_double_rel = 2e-12,
  diameter_single_rel = 3e-4,
  height_double_rel = 4e-5,
  height_single_rel = 2e-3
)

.read_smalltaper_fixture <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- strsplit(readLines(connection, n = 1L), ",", fixed = TRUE)[[1L]]
  classes <- rep("NULL", length(header))
  classes[header %in% .smalltaper_columns] <- NA
  utils::read.csv(path, colClasses = classes, stringsAsFactors = FALSE)
}

.smalltaper_fixture_path <- function(root, family, precision) {
  name <- paste0(family, ".", precision, ".csv.gz")
  candidates <- c(file.path(root, "full", name), file.path(root, name))
  found <- candidates[file.exists(candidates)]
  if (length(found)) found[[1L]] else candidates[[1L]]
}

.smalltaper_expected_status <- function(primary, operation) {
  flag <- operation
  missing <- is.na(flag) | flag == 0L
  flag[missing] <- primary[missing]
  ifelse(is.na(flag) | flag == 0L, 0L, 300L + as.integer(flag))
}

.expect_smalltaper_relative <- function(actual, expected, tolerance) {
  relative <- abs(actual - expected) / pmax(abs(expected), 1e-12)
  expect_true(
    all(relative <= tolerance),
    info = paste("maximum relative difference", max(relative, na.rm = TRUE))
  )
}

.check_smalltaper_fixture <- function(path, precision, record = FALSE,
                                      expected_source_domain = NULL,
                                      expected_below_contract = NULL) {
  fixture <- .read_smalltaper_fixture(path)

  diameter <- fixture[
    fixture$CALL_KIND == "PROFILE_PROBE" &
      fixture$CALCDIA_REQUESTED == "Y", , drop = FALSE
  ]
  expected <- .smalltaper_expected_status(
    diameter$ERRFLAG, diameter$CALCDIA_ERRFLAG
  )
  actual <- dib(
    diameter$DBHOB, diameter$HTTOT, diameter$HTUP, diameter$VOLEQ,
    status = TRUE
  )
  rejected <- expected != 0L
  expect_identical(actual$status[rejected], expected[rejected])
  expect_true(all(is.na(actual$value[rejected])))
  source_nonfinite <- expected == 0L & !is.finite(diameter$DIB)
  expect_true(all(actual$status[source_nonfinite] == 54L))
  good <- expected == 0L & !source_nonfinite
  expect_true(all(actual$status[good] == 0L))
  .expect_smalltaper_relative(
    actual$value[good], diameter$DIB[good],
    .smalltaper_tolerance[[paste0("diameter_", precision, "_rel")]]
  )
  if (record) {
    .gate1_record_values(
      unique(fixture$FAMILY), "DIB", precision,
      actual$value, diameter$DIB, good,
      .smalltaper_tolerance[[paste0("diameter_", precision, "_rel")]],
      list(nonzero_errflag = rejected, nonfinite_oracle = source_nonfinite)
    )
  }

  inverse <- fixture[
    fixture$CALL_KIND == "PROFILE_PROBE" &
      fixture$HT2TOPD_REQUESTED == "Y", , drop = FALSE
  ]
  expected <- .smalltaper_expected_status(
    inverse$ERRFLAG, inverse$HT2TOPD_ERRFLAG
  )
  port <- height_at_dib(
    inverse$DBHOB, inverse$HTTOT, inverse$STEMDIB, inverse$VOLEQ,
    status = TRUE
  )
  actual <- .with_treevolume_compat("nvel", height_at_dib(
    inverse$DBHOB, inverse$HTTOT, inverse$STEMDIB, inverse$VOLEQ,
    status = TRUE
  ))
  rejected <- expected != 0L
  expect_identical(actual$status[rejected], expected[rejected])
  expect_true(all(is.na(actual$value[rejected])))
  r12_undispatched <- inverse$FAMILY == "r12_taper" & expected == 0L
  expect_true(all(inverse$STEMHT[r12_undispatched] == 0))
  source_nonfinite <- expected == 0L & !is.finite(inverse$STEMHT)
  below_contract_family <- inverse$FAMILY %in%
    c("r1_taper", "blm_taper", "behre_taper")
  below_contract <- expected == 0L & below_contract_family &
    is.finite(inverse$STEMHT) & inverse$STEMHT <= 1
  if (!is.null(expected_below_contract)) {
    expect_identical(sum(below_contract), expected_below_contract)
  }
  source_domain <- expected == 0L & actual$status == 54L & !below_contract
  if (!is.null(expected_source_domain)) {
    expect_identical(sum(source_domain), expected_source_domain)
  }
  expect_true(all(is.na(actual$value[source_domain])))
  good <- expected == 0L & !source_nonfinite & !source_domain &
    !below_contract
  expect_true(all(actual$status[good] == 0L))
  .expect_smalltaper_relative(
    actual$value[good], inverse$STEMHT[good],
    .smalltaper_tolerance[[paste0("height_", precision, "_rel")]]
  )
  port_root <- expected == 0L & port$status %in% c(0L, 102L)
  expect_true(all(is.finite(port$value[port_root])))
  expect_true(all(port$value[port_root] >= 1))
  expect_true(all(port$value[port_root] <= inverse$HTTOT[port_root]))
  if (record) {
    .gate1_record_values(
      unique(fixture$FAMILY), "height at DIB", precision,
      actual$value, inverse$STEMHT, good,
      .smalltaper_tolerance[[paste0("height_", precision, "_rel")]],
      list(
        nonzero_errflag = rejected,
        nonfinite_oracle = source_nonfinite,
        source_domain_status = source_domain,
        at_or_below_contract_stump = below_contract
      ),
      compat = "nvel"
    )
  }

  invisible(TRUE)
}
