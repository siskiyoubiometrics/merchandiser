test_that("Region 10 cedar equality reproduces both oracle builds in NVEL mode", {
  fixture <- utils::read.csv(
    testthat::test_path("fixtures", "r10_cedar_equality_oracle.csv"),
    stringsAsFactors = FALSE
  )

  for (precision in c("double", "single")) {
    rows <- fixture$precision == precision
    source <- .with_treevolume_compat("nvel", height_at_dib(
      fixture$DBHOB[rows], fixture$HTTOT[rows], fixture$STEMDIB[rows],
      fixture$VOLEQ[rows], status = TRUE
    ))
    tolerance <- .r10r4_tolerance[[paste0("height_", precision, "_rel")]]
    expect_identical(sum(rows), 2L)
    expect_identical(source$status, fixture$ERRFLAG[rows])
    .expect_r10r4_relative(
      source$value, fixture$STEMHT[rows], tolerance
    )
    .gate1_record_values(
      "r10_taper", "cedar equality height at DIB", precision,
      source$value, fixture$STEMHT[rows], rep(TRUE, sum(rows)), tolerance,
      compat = "nvel"
    )
  }
})

test_that("Region 10 cedar equality is internally consistent by default", {
  fixture <- utils::read.csv(
    testthat::test_path("fixtures", "r10_cedar_equality_oracle.csv"),
    stringsAsFactors = FALSE
  )
  fixture <- fixture[fixture$precision == "double", , drop = FALSE]
  result <- height_at_dib(
    fixture$DBHOB, fixture$HTTOT, fixture$STEMDIB, fixture$VOLEQ,
    status = TRUE
  )

  expect_true(all(result$status %in% c(0L, 102L)))
  expect_equal(
    dib(fixture$DBHOB, fixture$HTTOT, result$value, fixture$VOLEQ),
    fixture$STEMDIB,
    tolerance = 1e-4
  )
  expect_true(all(abs(result$value - fixture$STEMHT) > 1))
})
