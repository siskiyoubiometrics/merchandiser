test_that("NVEL integration metadata is public and pinned", {
  expect_true(all(c("nvel_rules", "nvel_source_revision") %in% getNamespaceExports(
    "merchandiser"
  )))

  revision <- nvel_source_revision()
  expect_type(revision, "character")
  expect_length(revision, 1L)
  expect_identical(revision[[1L]], "38548071d5aa652bb90c7f111f86b427f798a1c9")
  expect_identical(
    attr(revision, "upstream_url"),
    "https://github.com/FMSC-Measurements/VolumeLibrary"
  )
  expect_identical(attr(revision, "fixtures_release_tag"), "v0.1.0")
})
