test_that("regional small-taper literals round trip with provenance", {
  expected_rows <- c(
    r1_taper = 254L, r2_taper = 120L, r5_taper = 63L,
    r12_taper = 1149L, blm_taper = 2915L, behre_taper = 2878L
  )
  for (family in names(expected_rows)) {
    path <- system.file(
      "extdata", paste0(family, "_coefficients.csv"),
      package = "merchandiser", mustWork = TRUE
    )
    coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)
    expect_equal(nrow(coefficients), expected_rows[[family]])
    expect_true(all(
      coefficients$upstream_commit ==
        "38548071d5aa652bb90c7f111f86b427f798a1c9"
    ))
    expect_true(all(
      coefficients$source_line_start <= coefficients$source_line_end
    ))
    numeric <- coefficients$value_type == "numeric"
    parsed <- as.double(gsub("[dD]", "e", coefficients$literal[numeric]))
    expect_identical(parsed, coefficients$value_numeric[numeric])
    character <- coefficients$value_type == "character"
    if (any(character)) {
      stripped <- substring(
        coefficients$literal[character], 2L,
        nchar(coefficients$literal[character]) - 1L
      )
      expect_identical(stripped, coefficients$value_character[character])
    }
  }
})
