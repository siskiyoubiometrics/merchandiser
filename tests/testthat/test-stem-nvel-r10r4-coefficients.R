test_that("Region 10 and Region 4 coefficient literals round trip", {
  path <- system.file("extdata", "r10r4_coefficients.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  coefficients <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(coefficients), 190L)
  expect_identical(sort(unique(coefficients$source_file)), c(
    "r10tap.f", "r10vol.f", "r10vol1.f",
    "r4vol.f"
  ))
  expect_identical(sum(coefficients$family == "r10"), 50L)
  expect_identical(sum(coefficients$family == "r4_driver"), 140L)
  expect_true(all(
    coefficients$upstream_commit == "38548071d5aa652bb90c7f111f86b427f798a1c9"
  ))
  expect_true(all(coefficients$source_line_start <= coefficients$source_line_end))

  parsed <- as.double(gsub("[[:space:]]", "", gsub("[dD]", "e", coefficients$literal)))
  expect_identical(parsed, coefficients$value_numeric)

  r4 <- coefficients[coefficients$family == "r4_driver", ]
  expect_identical(r4$ordinal, seq_len(140L))
  expect_true(all(r4$target == "CFCOEF(20,7)"))
})

test_that("regional model metadata preserves source provenance", {
  metadata <- utils::read.csv(
    system.file("extdata", "r10r4_models.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(metadata), 58L)
  expect_identical(sum(metadata$family == "r10_taper"), 38L)
  expect_identical(sum(metadata$family == "r4_driver"), 20L)
  expect_true(all(metadata$oracle_tested))
  expect_true(all(has_taper_model(metadata$id)))

  sources <- utils::read.csv(
    system.file("extdata", "r10r4_sources.csv",
      package = "merchandiser"
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(sources), 11L)
  expect_true(all(sources$upstream_commit == unique(metadata$upstream_commit)))
  expect_match(sources$evidence[sources$source_file == "r4vol.f"], "03/21/2017")
})
