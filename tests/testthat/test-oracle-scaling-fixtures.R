test_that("full native NVEL log tables agree with verified oracle fixtures", {
  root <- Sys.getenv("MERCHANDISER_FIXTURES", unset = Sys.getenv(
    "TREEVOLUME_FIXTURES",
    unset = ""
  ))
  if (!dir.exists(root)) {
    skip("MERCHANDISER_FIXTURES directory is missing. Full scaling tests not run")
  }
  agreement <- mc_full_oracle_agreement(root)
  expect_equal(agreement$source_rows_scanned, agreement$manifest_rows)
  expect_true(all(agreement$invalid_build_rows == 0))
  expect_identical(agreement$source_rows_scanned, agreement$rows_compared +
                     agreement$excluded_not_volumelibrary +
                     agreement$excluded_oracle_error + agreement$excluded_no_log_table)
  expect_identical(agreement$source_logs, agreement$included_logs +
                     agreement$excluded_missing_log_fields)
  f32 <- agreement$family == "flewelling_3pt"
  expect_identical(
    agreement$included_logs[f32],
    agreement$scribner_source_logs[f32] + agreement$excluded_f32_unpaired_logs[f32] +
      agreement$excluded_f32_incomplete_logs[f32] +
      agreement$excluded_scribner_non_split_logs[f32]
  )
  expect_identical(
    agreement$included_logs[!f32],
    agreement$scribner_source_logs[!f32] + agreement$excluded_scribner_non_split_logs[!f32]
  )
  clark <- startsWith(agreement$family, "clark_")
  expect_identical(
    agreement$rows_compared[clark],
    agreement$excluded_international_family[clark]
  )
  expect_identical(
    agreement$rows_compared[!clark],
    agreement$international_compared[!clark] +
      agreement$excluded_international_incomplete[!clark]
  )
  expect_identical(agreement$scribner_within_tolerance, agreement$scribner_compared)
  expect_identical(
    agreement$international_within_tolerance,
    agreement$international_compared
  )
  expect_true(all(agreement$rows_compared > 50))
  expected <- data.frame(
    family = c("flewelling_2pt", "flewelling_3pt", "clark_r8", "clark_r9"),
    excluded_not_volumelibrary = c(165874, 48505, 2353547, 729339),
    excluded_oracle_error = c(
      868,
      0, 207030, 7262
    ), excluded_no_log_table = c(4215, 1616, 414021, 75849),
    excluded_missing_log_fields = c(
      0,
      0, 0, 54
    ), excluded_f32_unpaired_logs = c(0, 573, 0, 0), excluded_f32_incomplete_logs = c(
      0,
      0, 0, 0
    ), excluded_scribner_non_split_logs = c(0, 0, 0, 1240290),
    excluded_international_family = c(
      0,
      0, 207083, 173488
    ), excluded_international_incomplete = c(0, 0, 0, 0), invalid_build_rows = c(
      0,
      0, 0, 0
    ), stringsAsFactors = FALSE
  )
  target <- match(agreement$family, expected$family)
  reason_columns <- setdiff(names(expected), "family")
  for (reason in reason_columns) {
    expect_identical(agreement[[reason]], expected[[reason]][target], info = paste(
      "exact exclusion count for",
      reason
    ))
  }
  case <- .mc_oracle_public_case(root)
  trim <- case$BOLHT2 - case$STUMP - case$LOGLEN1
  rules <- merchandiser::nvel_rules(
    even_or_odd = 2L, option = 22L, maximum_length = 16, minimum_length = 2,
    minimum_top_length = 2, merchantable_length = case$BOLHT2 - case$STUMP,
    primary_top = case$MTOPP,
    secondary_top = case$MTOPS, stump = case$STUMP, trim = trim,
    bark_ratio = NA_real_, minimum_board_foot_dbh = 1,
    scribner = "table", prod = "01", ht_type = "F", live = "L", ctype = "C", forest = 0,
    district = 1
  )
  translated <- products_from_nvel_rules(rules)
  translated$products$price[] <- 1
  run <- function(products) {
    products$min_length <- as.double(case$LOGLEN1)
    products$max_length <- as.double(case$LOGLEN1)
    products$max_logs <- 1
    merchandise(
      tree_id = 1, dbh = as.double(case$DBHOB), ht = as.double(case$HTTOT),
      model = case$VOLEQ,
      products = products, spcd = as.integer(case$FIASPCD), stump_ht = as.double(
        case$STUMP
      ),
      defects = defect(1, as.double(case$BOLHT2), NA_real_, "end"), quiet = TRUE
    )
  }
  scribner <- run(translated$products)
  expect_identical(scribner$status$status, integer())
  expect_identical(scribner$logs$length, as.double(case$LOGLEN1))
  expect_identical(scribner$logs$scale, as.double(case$LOGVOL_1_1))
  international_products <- translated$products
  international_products$volume_unit[] <- "international"
  international <- run(international_products)
  expect_identical(international$status$status, integer())
  expect_identical(international$logs$length, as.double(case$LOGLEN1))
  expect_identical(international$logs$scale, as.double(case$VOL10))
  saveRDS(agreement, "/tmp/mc-hardening-oracle-agreement.rds")
})
