# This is a permitted guarded skip: complete oracle fixtures are protected and
# remain outside the package. Set TREEVOLUME_FIXTURES to exercise them.
test_that("full Clark fixtures agree when supplied", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES",
                     unset = Sys.getenv("TREEVOLUME_FIXTURES", unset = ""))
  if (!dir.exists(root)) {
    skip("MERCHANDISER_FIXTURES directory is missing. full Clark oracle tests not run")
  }
  expected_identifiers <- c(clark_r8 = 2156L, clark_r9 = 668L)
  for (precision in c("double", "single")) {
    for (family in names(expected_identifiers)) {
      path <- .clark_fixture_path(root, family, precision)
      expect_true(file.exists(path), info = path)
      metrics <- .check_clark_streamed_fixture(path, precision)
      expect_equal(
        length(metrics$identifiers), expected_identifiers[[family]],
        info = paste(family, precision)
      )
      expected_overlap <- if (identical(family, "clark_r8")) 4L else 0L
      expect_identical(metrics$compat_overlap$compared, expected_overlap)
      expect_identical(metrics$compat_overlap$within, expected_overlap)
      dib <- metrics$operations$dib
      .gate1_record_summary(
        family, "DIB", precision, dib$compared,
        dib$compared - dib$failures, dib$max_relative,
        tv_tolerance[[paste0("diameter_", precision, "_rel")]],
        list(
          nonzero_errflag = dib$excluded_nonzero_errflag,
          nonfinite_oracle = dib$excluded_nonfinite,
          single_diameter_floor = dib$excluded_diameter_floor
        )
      )
      height <- metrics$operations$height
      if (expected_overlap) {
        outside_overlap <- height$compared +
          height$excluded_nonzero_errflag + height$excluded_nonfinite +
          height$excluded_below_stump
        .gate1_record_summary(
          family, "height at DIB", precision,
          metrics$compat_overlap$compared,
          metrics$compat_overlap$within,
          metrics$compat_overlap$max_relative,
          tv_tolerance[[paste0("height_", precision, "_rel")]],
          list(outside_834CLKE110_overlap = outside_overlap),
          compat = "nvel"
        )
      }
      .gate1_record_summary(
        family, "height at DIB", precision, height$compared,
        height$compared - height$failures, height$max_relative,
        tv_tolerance[[paste0("height_", precision, "_rel")]],
        list(
          nonzero_errflag = height$excluded_nonzero_errflag,
          nonfinite_oracle = height$excluded_nonfinite,
          below_contract_stump = height$excluded_below_stump,
          overlapping_roots = height$excluded_overlap_inverse
        )
      )
    }
  }
})
