test_that("Clark default equations match every oracle default row", {
  source <- Sys.getenv("MERCHANDISER_FIXTURES", unset = Sys.getenv(
    "TREEVOLUME_FIXTURES",
    unset = ""
  ))
  if (!dir.exists(source)) {
    skip("MERCHANDISER_FIXTURES directory is missing. Clark defaults not tested")
  }
  path <- file.path(source, "clark_identifiers.csv")
  if (!file.exists(path))
    stop("Clark identifier oracle is unavailable: ", path)
  fixture <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(
    identifier = "character",
    DOUBLE_VOLEQ_OUT = "character"
  ))
  fixture <- fixture[fixture$discovery_mode == "default", , drop = FALSE]
  observed <- nvel_default_equation(
    fixture$REGN, fixture$FORST, fixture$DIST, fixture$SPEC,
    fixture$VAR
  )
  expect_identical(length(observed), 668L)
  expect_identical(observed, fixture$DOUBLE_VOLEQ_OUT)
})

test_that("default-equation construction covers the source domain", {
  regional_species <- list(
    `1` = merchandiser:::.nvel_fia("R1_EQN"), `2` = merchandiser:::.nvel_fia("R2_EQN"),
    `3` = merchandiser:::.nvel_fia("R3_EQN"), `4` = merchandiser:::.nvel_fia(
      "R4_EQN"
    ), `5` = merchandiser:::.nvel_fia("R5_EQN"),
    `6` = merchandiser:::.nvel_fia("R6_EQN"), `7` = merchandiser:::.nvel_fia(
      "R7_EQN"
    ), `8` = merchandiser:::.nvel_array(
      "R8_CEQN",
      "SNFIA", "numeric"
    ), `9` = unique(c(merchandiser:::.nvel_array(
      "R9_EQN", "LSFIA",
      "numeric"
    ), merchandiser:::.nvel_array("R9_EQN", "CSFIA", "numeric"), merchandiser:::.nvel_array(
      "R9_EQN",
      "NEFIA", "numeric"
    ), merchandiser:::.nvel_array("R9_EQN", "SNFIA", "numeric"))),
    `10` = merchandiser:::.nvel_array("R10_EQN", "FIA", "numeric")
  )
  forests <- list(`1` = c(3, 4, 5, 8, 14, 16, 17), `2` = c(2, 3, 13, 14), `3` = c(
    2, 3, 5,
    6, 7, 8, 9, 10, 11
  ), `4` = c(1:19), `5` = c(3, 5, 6, 8, 9, 11, 13, 14, 15, 16, 17), `6` = c(
    1:12,
    14:18, 20, 21
  ), `7` = c(2, 3, 12), `8` = c(1:13, 36, 60), `9` = c(2:14, 19:22), `10` = 4)
  districts <- list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 0, `6` = c(
    0, 1, 2, 3, 5, 6,
    7, 9
  ), `7` = 0, `8` = c(0, 2, 3, 5, 6, 7, 8, 10, 17), `9` = 0, `10` = 0)
  domain <- do.call(rbind, lapply(1:10, function(region) {
    expand.grid(
      region = region, forest = forests[[as.character(region)]],
      district = districts[[as.character(region)]],
      spcd = regional_species[[as.character(region)]], KEEP.OUT.ATTRS = FALSE
    )
  }))
  observed <- nvel_default_equation(
    domain$region, domain$forest, domain$district,
    domain$spcd
  )
  expect_identical(nrow(domain), 27971L)
  expect_identical(sum(is.na(observed)), 15L)
  expect_identical(length(unique(observed[!is.na(observed)])), 925L)
  expect_true(all(is.na(observed) | nchar(observed) == 10L))
  reasons <- vapply(unique(observed[!is.na(observed)]), function(id) {
    merchandiser:::.nvel_identifier_resolution(id)$reason
  }, character(1))
  expect_false(any(reasons == "unknown model id", na.rm = TRUE))
  expect_identical(nvel_default_equation(8, 1, 3, c(100, 107, 999)), c(
    "811CLKE100", "811CLKE107",
    "811CLKE300"
  ))
})

test_that("Region 4 default selection covers every source override", {
  forest <- c(
    2, 9, 1, 2, 1, 5, 1, 1, 3, 3, 1, 2, 7, 8, 1, 9, 1, 1, 2, 2, 1, 7, 9, 3, 2, 5,
    1, 2, 1, 1, 14
  )
  species <- c(
    15, 15, 15, 17, 17, 19, 19, 64, 64, 65, 65, 93, 93, 93, 93, 101, 101, 106, 106,
    122, 122, 122, 122, 122, 202, 202, 202, 998, 998, 999, 998
  )
  index <- c(
    28, 29, 30, 31, 30, 32, 33, 49, NA, 34, 35, 36, 37, 56, 38, 39, 40, 50, NA, 41,
    42, 43, 44, 45, 46, 47, 48, 17, 21, 26, 17
  )
  equations <- merchandiser:::.nvel_eq("R4_EQN")
  expected <- rep(NA_character_, length(index))
  expected[!is.na(index)] <- equations[index[!is.na(index)]]
  observed <- nvel_default_equation(4, forest, 0, species, variant = "")
  expect_identical(observed, expected)
})

test_that("FIA equation crosswalk is complete and applies substitutions", {
  table <- merchandiser:::.nvel_reference("nvel_fia_crosswalk.csv")
  expect_identical(nrow(table), 592L)
  expect_identical(as.integer(table(table$volume_class)[c("board_foot", "cubic")]), c(
    231L,
    361L
  ))
  result <- nvel_from_fia_code(c("BD000006", "CU000030", "NOTACODE"), c(
    122, 122,
    122
  ), geosub = c(
    "",
    "7", ""
  ), primary_top = c(6, 0, 6))
  expect_identical(result$model, c("R02ALN0122", "P02WEN1122", NA_character_))
  expect_identical(result$volume_type, c("SV6", "CV7", NA_character_))
  expect_equal(result$primary_top, c(6, 7, 6))
  expect_identical(result$errflag, c(0L, 0L, 1L))

  roundtrip <- nvel_from_fia_code(table$fia_code, rep(999L, nrow(table)), primary_top = 99)
  expect_identical(roundtrip$model, table$nvel_template)
  expect_identical(roundtrip$volume_type, table$volume_type)
  expect_identical(roundtrip$errflag, integer(592L))

  geographic <- nvel_from_fia_code(rep("BD000030", 5), rep(122, 5), geosub = c(
    "1", "01", "11",
    "001", "1.0"
  ), primary_top = 6)
  expect_identical(geographic$model, c(
    "801DVEE122", "801DVEE122", "811DVEE122", "825DVEE122",
    "825DVEE122"
  ))

  source_geographic <- .with_treevolume_compat("nvel", nvel_from_fia_code(
    rep(
      "BD000030", 5
    ),
    rep(122, 5),
    geosub = c("1", "01", "11", "001", "1.0"), primary_top = 6
  ))
  expected_source <- rep("825DVEE122", 5)
  expect_identical(source_geographic$model, expected_source)
  .gate1_record_values("FIAEQ2NVELEQ", "short geographic substitution", "double",
    as.double(source_geographic$model ==
                expected_source), rep(1, 5), rep(TRUE, 5), 0,
    compat = "nvel"
  )
})

test_that("the compatibility option is validated centrally", {
  old <- options(merchandiser.compat = "unsupported")
  on.exit(options(old), add = TRUE)
  expect_error(nvel_from_fia_code("BD000030", 122), "merchandiser.compat")
  expect_error(
    .oracle_dib(dbh = 12, ht = 80, h = 40, model = "100FW2W202", spcd = .interface_spcd(
      "100FW2W202"
    ))$value,
    "merchandiser.compat"
  )
})

test_that("NVEL identifier helpers honor zero-length input", {
  expect_identical(nvel_default_equation(numeric(), 1, 0, 122), character())
  result <- nvel_from_fia_code(character(), numeric())
  expect_identical(nrow(result), 0L)
  expect_named(result, c("model", "volume_type", "primary_top", "errflag"))
})

test_that("Region 8 and Region 9 validity follows source lists", {
  expect_true(merchandiser:::.nvel_r8_ceqn_valid("811CLKE100"))
  expect_false(merchandiser:::.nvel_r8_ceqn_valid("801CLKE100"))
  expect_false(merchandiser:::.nvel_r8_ceqn_valid("812CLKE100"))
  expect_true(merchandiser:::.nvel_r8_beqn_valid("801DVEE261"))
  expect_false(merchandiser:::.nvel_r8_beqn_valid("834DVEE261"))
  expect_true(merchandiser:::.nvel_r9_valid("900CLKE012"))
  expect_false(merchandiser:::.nvel_r9_valid("900CLKE013"))
})
