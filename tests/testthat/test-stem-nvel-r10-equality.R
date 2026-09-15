test_that("Region 10 cedar equality reproduces both oracle builds in NVEL mode", {
  fixture <- utils::read.csv(
    testthat::test_path(
      "fixtures",
      "r10_cedar_equality_oracle.csv"
    ),
    stringsAsFactors = FALSE
  )

  for (precision in c("double", "single")) {
    rows <- fixture$precision == precision
    source <- .with_treevolume_compat("nvel", .oracle_height_at_dib(
      dbh = fixture$DBHOB[rows], ht = fixture$HTTOT[rows],
      dib = fixture$STEMDIB[rows], model = fixture$VOLEQ[rows],
      spcd = .interface_spcd(fixture$VOLEQ[rows])
    ))
    tolerance <- .r10r4_tolerance[[paste0("height_", precision, "_rel")]]
    expect_identical(sum(rows), 2L)
    expect_identical(source$status, fixture$ERRFLAG[rows])
    .expect_r10r4_relative(source$value, fixture$STEMHT[rows], tolerance)
    .gate1_record_values("r10_taper", "cedar equality height at DIB", precision,
      source$value,
      fixture$STEMHT[rows], rep(TRUE, sum(rows)), tolerance,
      compat = "nvel"
    )
  }
})

test_that("Region 10 cedar equality is internally consistent by default", {
  fixture <- utils::read.csv(
    testthat::test_path(
      "fixtures",
      "r10_cedar_equality_oracle.csv"
    ),
    stringsAsFactors = FALSE
  )
  fixture <- fixture[fixture$precision == "double", , drop = FALSE]
  result <- .oracle_height_at_dib(
    dbh = fixture$DBHOB, ht = fixture$HTTOT, dib = fixture$STEMDIB, model = fixture$VOLEQ,
    spcd = .interface_spcd(fixture$VOLEQ)
  )

  expect_true(all(result$status %in% c(0L, 102L)))
  expect_equal(.oracle_dib(
    dbh = fixture$DBHOB, ht = fixture$HTTOT, h = result$value, model = fixture$VOLEQ,
    spcd = .interface_spcd(fixture$VOLEQ)
  )$value, fixture$STEMDIB, tolerance = 1e-04)
  expect_true(all(abs(result$value - fixture$STEMHT) > 1))
})
