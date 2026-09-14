test_that("NSVB coefficient literals round trip with provenance", {
  path <- system.file(
    "extdata", "nsvb_coefficients.csv", package = "merchandiser",
    mustWork = TRUE
  )
  coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(coefficients), 37935L)
  expect_setequal(
    unique(coefficients$source_file),
    c(
      paste0("tables", 1:11, ".inc"), "dist_ecoprov.inc",
      "regndftdata.inc"
    )
  )
  expect_true(all(
    coefficients$upstream_commit ==
      "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))
  expect_true(all(
    coefficients$source_line_start <= coefficients$source_line_end
  ))
  parsed <- as.double(gsub("[dD]", "e", coefficients$literal))
  expect_identical(parsed, coefficients$value)
})

test_that("NSVB species literals and public reference are complete", {
  path <- system.file(
    "extdata", "nsvb_species_literals.csv", package = "merchandiser",
    mustWork = TRUE
  )
  literals <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(literals), 2677L * 12L)
  expect_identical(
    as.double(gsub("[dD]", "e", literals$literal)), literals$value
  )
  expect_true(all(
    literals$upstream_commit ==
      "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))

  expect_equal(nrow(species_reference), 2677L)
  expect_identical(
    names(species_reference),
    c(
      "spcd", "symbol", "common", "scientific", "genus",
      "softwood_hardwood", "bark_ratio", "wood_density", "sources"
    )
  )
  expect_identical(anyDuplicated(species_reference$spcd), 0L)
  expect_true(all(species_reference$bark_ratio > 0 & species_reference$bark_ratio <= 1))
  expect_true(all(species_reference$wood_density >= 0))
  expect_true(all(grepl("wdbkwtdata.inc", species_reference$sources, fixed = TRUE)))
})

test_that("NSVB Scribner literals retain executable provenance", {
  path <- system.file(
    "extdata", "nsvb_scribner_coefficients.csv", package = "merchandiser",
    mustWork = TRUE
  )
  coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(coefficients), 132L + 149L)
  expect_setequal(unique(coefficients$target), c("FACTOR", "EXCEPT"))
  expect_true(all(coefficients$source_file == "scrib.f"))
  expect_true(all(
    coefficients$upstream_commit ==
      "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))
  expect_identical(
    as.double(gsub("[dD]", "e", coefficients$literal)), coefficients$value
  )
})

test_that("county division lookup has pinned Forest Service provenance", {
  path <- system.file(
    "extdata", "nsvb_county_divisions.csv", package = "merchandiser",
    mustWork = TRUE
  )
  divisions <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(divisions), 3104L)
  expect_identical(anyDuplicated(paste(divisions$state, divisions$county)), 0L)
  expect_true(all(divisions$source_page >= 21L & divisions$source_page <= 94L))

  provenance_path <- system.file(
    "extdata", "nsvb_county_divisions_provenance.csv",
    package = "merchandiser", mustWork = TRUE
  )
  provenance <- utils::read.csv(provenance_path, stringsAsFactors = FALSE)
  expect_identical(
    provenance$sha256,
    "a84f6596f623ad1efc148730e666c748056bb5430b3d925d21b61527feeca1e8"
  )
  expect_identical(provenance$rows, 3104L)
})
