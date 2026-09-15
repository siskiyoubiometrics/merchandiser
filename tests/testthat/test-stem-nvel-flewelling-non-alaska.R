test_that("non-Alaska three-point groups agree with direct NVEL probes", {
  fixture <- utils::read.csv(
    testthat::test_path(
      "fixtures",
      "flewelling_3pt_non_alaska.csv"
    ),
    stringsAsFactors = FALSE
  )

  expect_setequal(unique(fixture$GROUP), c("west", "ingy", "black_hills", "other"))
  expect_true(all(fixture$ERRFLAG == 0L))

  actual_dib <- .oracle_dib(
    dbh = fixture$DBHOB, ht = fixture$HTTOT, h = fixture$HTUP, model = fixture$VOLEQ,
    upper_ht1 = fixture$UPSHT1, upper_d1 = fixture$UPSD1, upper_bark = fixture$UPPER_BARK,
    spcd = .interface_spcd(fixture$VOLEQ)
  )
  actual_dob <- .oracle_dob(
    dbh = fixture$DBHOB, ht = fixture$HTTOT, h = fixture$HTUP, model = fixture$VOLEQ,
    upper_ht1 = fixture$UPSHT1, upper_d1 = fixture$UPSD1, upper_bark = fixture$UPPER_BARK,
    spcd = .interface_spcd(fixture$VOLEQ)
  )

  expect_identical(actual_dib$status, integer(nrow(fixture)))
  expect_identical(actual_dob$status, integer(nrow(fixture)))
  for (precision in c("double", "single")) {
    selected <- fixture$BUILD == precision
    tolerance <- tv_tolerance[[paste0("diameter_", precision, "_rel")]]
    .expect_flewelling_relative(
      actual_dib$value[selected], fixture$DIB[selected],
      tolerance
    )
    .expect_flewelling_relative(
      actual_dob$value[selected], fixture$DOB[selected],
      tolerance
    )
  }
})

test_that("dynamic Pacific Northwest and INGY FW3 and F33 ids match oracle probes", {
  fixture <- utils::read.csv(
    testthat::test_path(
      "fixtures",
      "flewelling_dynamic_3pt_oracle.csv"
    ),
    stringsAsFactors = FALSE
  )
  metadata <- utils::read.csv(
    system.file("extdata", "flewelling_models.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )

  expect_setequal(unique(substr(fixture$VOLEQ, 4L, 6L)), c("FW3", "F33"))
  expect_setequal(unique(substr(fixture$VOLEQ, 1L, 1L)), c("F", "I"))
  expect_false(any(unique(fixture$VOLEQ) %in% metadata$id))
  expect_true(all(fixture$ERRFLAG == 0L))

  two_point <- fixture$UPSHT2 > 0 & fixture$UPSD2 > 0
  actual <- rep(NA_real_, nrow(fixture))
  actual_status <- rep(NA_integer_, nrow(fixture))
  for (two in c(FALSE, TRUE)) {
    selected <- two_point == two
    arguments <- list(
      dbh = fixture$DBHOB[selected], ht = fixture$HTTOT[selected],
      h = fixture$HTUP[selected],
      model = fixture$VOLEQ[selected], upper_ht1 = fixture$UPSHT1[selected],
      upper_d1 = fixture$UPSD1[selected],
      upper_bark = fixture$UPPER_BARK[selected], spcd = .interface_spcd(
        fixture$VOLEQ[selected]
      )
    )
    if (two) {
      arguments$upper_ht2 <- fixture$UPSHT2[selected]
      arguments$upper_d2 <- fixture$UPSD2[selected]
    }
    evaluated <- do.call(.oracle_dib, arguments)
    actual[selected] <- evaluated$value
    actual_status[selected] <- evaluated$status
  }

  expect_identical(actual_status, integer(nrow(fixture)))
  for (precision in c("double", "single")) {
    selected <- fixture$BUILD == precision
    .expect_flewelling_relative(
      actual[selected], fixture$DIB[selected],
      tv_tolerance[[paste0(
        "diameter_",
        precision, "_rel"
      )]]
    )
  }
})
