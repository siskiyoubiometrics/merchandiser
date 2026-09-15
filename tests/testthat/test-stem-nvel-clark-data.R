test_that("Clark coefficient inventory is complete and round trips", {
  path <- system.file("extdata", "clark_coefficients.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  coefficients <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  expect_equal(nrow(coefficients), 28726L)
  expect_setequal(unique(coefficients$source_file), c(
    "r8dib.f", "r8dib.inc", "r8clkcoef.inc",
    "r8cfo.inc", "r8clist.inc", "r8vlist.f", "r8vlist.inc", "r8init.f", "r8vol2.f",
    "r9clark.f",
    "r9coeff.inc", "r9init.f", "r9vol.f", "voleqdef.f"
  ))
  expect_true(all(
    coefficients$upstream_commit == "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))

  round_trip_path <- tempfile(fileext = ".csv")
  utils::write.csv(coefficients, round_trip_path, row.names = FALSE, na = "")
  round_trip <- utils::read.csv(round_trip_path, stringsAsFactors = FALSE, na.strings = "")
  expect_equal(round_trip, coefficients, tolerance = 0)
})

test_that("compiled Clark tables have their declared literal counts", {
  coefficients <- utils::read.csv(system.file("extdata", "clark_coefficients.csv",
                                    package = "merchandiser",
                                    mustWork = TRUE
                                  ), stringsAsFactors = FALSE, na.strings = "")
  count <- function(file, pattern) {
    source_match <- coefficients$source_file == file
    target_match <- grepl(pattern, coefficients$target, ignore.case = TRUE)
    sum(source_match & target_match)
  }
  expect_equal(count("r8dib.inc", "R8CF\\("), 182L * 18L)
  expect_equal(count("r8cfo.inc", "R8CFO\\("), 182L * 9L)
  expect_equal(count("r8clkcoef.inc", "DIBMEN\\("), 49L * 3L)
  expect_equal(count("r8clkcoef.inc", "^[(][(]TOTAL\\("), 49L * 7L)
  expect_equal(count("r8clkcoef.inc", "^[(][(]OTOTAL\\("), 49L * 7L)
  expect_equal(count("r8clkcoef.inc", "FOUR\\("), 49L * 6L)
  expect_equal(count("r8clkcoef.inc", "SEVEN\\("), 15L * 6L)
  expect_equal(count("r8clkcoef.inc", "NINE\\("), 34L * 6L)
  expect_equal(count("r9coeff.inc", "coefA\\("), 47L * 4L)
  expect_equal(count("r9coeff.inc", "coef0\\("), 47L * 9L)
  expect_equal(count("r9coeff.inc", "coef4\\("), 47L * 8L)
  expect_equal(count("r9coeff.inc", "coef79\\("), 47L * 8L)
})
