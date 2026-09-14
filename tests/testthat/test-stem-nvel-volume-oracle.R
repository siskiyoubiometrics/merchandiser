.nvel_volume_families <- c(
  "behre_taper", "blm_taper", "clark_r8", "clark_r9",
  "flewelling_2pt", "flewelling_3pt", "nsvb", "r1_taper", "r10_taper",
  "r12_taper", "r2_taper", "r4_driver", "r5_taper"
)

.nvel_expected_metrics <- function(
    rows, valid, status_exceptions = 0L, total_excluded = 0L,
    total_compared = valid, stump_driver_excluded = 0L,
    stump_precision_excluded = 0L,
    stump_compared = valid, tip_segmentation_excluded = valid,
    tip_compared = 0L) {
  c(
    rows = rows, valid = valid, status_exceptions = status_exceptions,
    status_failures = 0L, total_excluded = total_excluded,
    total_compared = total_compared, total_failures = 0L,
    stump_driver_excluded = stump_driver_excluded,
    stump_precision_excluded = stump_precision_excluded,
    stump_compared = stump_compared, stump_failures = 0L,
    tip_segmentation_excluded = tip_segmentation_excluded,
    tip_compared = tip_compared, tip_failures = 0L
  )
}

.nvel_pruned_metrics <- function(family, precision) {
  values <- switch(family,
    behre_taper = .nvel_expected_metrics(2L, 0L),
    blm_taper = .nvel_expected_metrics(35L, 0L),
    clark_r8 = .nvel_expected_metrics(
      26L, 20L, stump_compared = 0L
    ),
    clark_r9 = .nvel_expected_metrics(
      18L, 18L, stump_compared = 0L
    ),
    flewelling_2pt = .nvel_expected_metrics(27L, 26L),
    flewelling_3pt = .nvel_expected_metrics(13L, 13L),
    nsvb = .nvel_expected_metrics(
      23L, 23L, tip_segmentation_excluded = 0L, tip_compared = 23L
    ),
    r1_taper = .nvel_expected_metrics(6L, 6L),
    r10_taper = .nvel_expected_metrics(
      121L, 121L, total_excluded = 14L, total_compared = 107L,
      stump_driver_excluded = if (precision == "double") 97L else 36L,
      stump_compared = if (precision == "double") 24L else 85L
    ),
    r12_taper = .nvel_expected_metrics(4L, 0L),
    r2_taper = .nvel_expected_metrics(16L, 8L),
    r4_driver = .nvel_expected_metrics(60L, 60L),
    r5_taper = .nvel_expected_metrics(18L, 18L)
  )
  as.integer(values) |> stats::setNames(names(values))
}

.nvel_full_metrics <- function(family, precision) {
  values <- switch(family,
    behre_taper = .nvel_expected_metrics(205L, 0L),
    blm_taper = .nvel_expected_metrics(3431L, 0L),
    clark_r8 = .nvel_expected_metrics(
      869656L, 652388L,
      stump_driver_excluded = if (precision == "double") 23408L else 23391L,
      stump_compared = if (precision == "double") 600721L else 600738L
    ),
    clark_r9 = .nvel_expected_metrics(
      269418L, 262156L, status_exceptions = 7262L,
      total_excluded = if (precision == "single") 23L else 0L,
      total_compared = if (precision == "single") 262133L else 262156L,
      stump_precision_excluded = if (precision == "single") 390L else 0L,
      stump_compared = if (precision == "single") 261766L else 262156L
    ),
    flewelling_2pt = .nvel_expected_metrics(
      15506L, 14589L, total_excluded = 173L, total_compared = 14416L,
      stump_compared = 14416L
    ),
    flewelling_3pt = .nvel_expected_metrics(
      4565L, 4565L, total_excluded = 601L, total_compared = 3964L,
      stump_compared = 3964L, tip_segmentation_excluded = 3964L
    ),
    nsvb = .nvel_expected_metrics(
      2523L, 2523L, tip_segmentation_excluded = 0L, tip_compared = 2523L
    ),
    r1_taper = .nvel_expected_metrics(592L, 592L),
    r10_taper = .nvel_expected_metrics(
      3844L, 3844L, total_excluded = 650L, total_compared = 3194L,
      stump_driver_excluded = if (precision == "double") 2850L else 2144L,
      stump_compared = if (precision == "double") 994L else 1700L
    ),
    r12_taper = .nvel_expected_metrics(395L, 0L),
    r2_taper = .nvel_expected_metrics(1612L, 804L),
    r4_driver = .nvel_expected_metrics(2012L, 2012L),
    r5_taper = .nvel_expected_metrics(1810L, 1810L)
  )
  as.integer(values) |> stats::setNames(names(values))
}

test_that("one NVEL volume harness covers every profile family", {
  root <- testthat::test_path("fixtures")
  metrics <- list()
  for (precision in c("double", "single")) {
    for (family in .nvel_volume_families) {
      path <- file.path(root, paste0(family, ".", precision, ".csv.gz"))
      metrics[[paste(family, precision)]] <- .check_nvel_volume_fixture(
        path, family, precision, .nvel_pruned_metrics(family, precision)
      )
    }
  }
  expect_identical(length(metrics), 26L)
})

test_that("the NSVB 210 and 230 volume anomaly uses the shared harness", {
  path <- testthat::test_path("fixtures", "nsvb_210_230.double.csv.gz")
  .check_nvel_volume_fixture(
    path, "nsvb", "double",
    .nvel_expected_metrics(
      20L, 20L, tip_segmentation_excluded = 0L, tip_compared = 20L
    )
  )
})

test_that("one NVEL volume harness covers full fixtures when supplied", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES",
                     unset = Sys.getenv("TREEVOLUME_FIXTURES", unset = ""))
  if (!dir.exists(root)) {
    skip("MERCHANDISER_FIXTURES directory is missing. full volume tests not run")
  }
  for (precision in c("double", "single")) {
    for (family in .nvel_volume_families) {
      path <- file.path(root, "full", paste0(family, ".", precision, ".csv.gz"))
      .check_nvel_volume_fixture(
        path, family, precision, .nvel_full_metrics(family, precision),
        record = TRUE
      )
      gc(verbose = FALSE)
    }
  }
  anomaly <- file.path(
    root, "anomalies", "nsvb_210_230_representative.double.csv"
  )
  .check_nvel_volume_fixture(
    anomaly, "nsvb", "double",
    .nvel_expected_metrics(
      20L, 20L, tip_segmentation_excluded = 0L, tip_compared = 20L
    )
  )
  .gate1_write()
})
