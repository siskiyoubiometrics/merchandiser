test_that("Flewelling coefficient literals round trip with provenance", {
  path <- system.file("extdata", "flewelling_coefficients.csv",
    package = "merchandiser", mustWork = TRUE
  )
  coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_equal(nrow(coefficients), 2322L)
  expect_equal(
    length(unique(paste(coefficients$source_file, coefficients$source_line_start))),
    237L
  )
  expect_setequal(unique(coefficients$source_file), c(
    "f_west.f", "f_ingy.f", "f_alaska.f",
    "f_other.f", "sf_hs.f", "sf_zero.f"
  ))
  expect_true(all(
    coefficients$upstream_commit == "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))
  expect_true(all(coefficients$source_line_start <= coefficients$source_line_end))

  numeric <- coefficients$value_type == "numeric"
  parsed <- as.double(gsub("[dD]", "e", coefficients$literal[numeric]))
  expect_identical(parsed, coefficients$value_numeric[numeric])

  character <- coefficients$value_type == "character"
  stripped <- substring(coefficients$literal[character], 2L, nchar(
    coefficients$literal[character]
  ) -
    1L)
  expect_identical(stripped, coefficients$value_character[character])
})
