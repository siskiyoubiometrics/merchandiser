test_that("literal identifier inventory resolves at the NVEL boundary", {
  catalog <- merchandiser:::.nvel_reference("nvel_identifier_catalog.csv")
  literal <- !grepl("[?*]", catalog$voleq)
  observed <- has_taper_model(catalog$voleq[literal])
  expect_identical(sum(literal), 520L)
  expect_true(all(observed[catalog$profile_based[literal]]))
  expect_false(any(observed[!catalog$profile_based[literal]]))
  expect_identical(sum(catalog$profile_based[literal]), 285L)
  expect_identical(sum(!catalog$profile_based[literal]), 235L)
})

test_that("pattern inventory resolves concrete identifiers only", {
  expect_true(all(has_taper_model(c(
    "300FW2W202", "300FW3W202", "200CZ2W108", "500WO2W202",
    "A01DEMW000", "100JB2W108", "B00BEHW202", "400MATW122",
    "H00SN2W510", "811CLKE100", "900CLKE012", "NVBM240202",
    "NVB0210110P"
  ))))
  expect_false(any(has_taper_model(c(
    "900DVEE122", "R01FAU0202", "101DVEW122", "???FW2????"
  ))))
  expect_error(get_taper_model("900DVEE122"), "direct volume equations")
  result <- suppressWarnings(stem_volume(
    12, 80, "900DVEE122", status = TRUE
  ))
  expect_identical(result$status, 50L)
})

test_that("every wildcard inventory row has the expected concrete boundary", {
  catalog <- merchandiser:::.nvel_reference("nvel_identifier_catalog.csv")
  literal <- !grepl("[?*]", catalog$voleq)
  rows <- which(!literal)
  candidates <- character(length(rows))
  for (at in seq_along(rows)) {
    row <- rows[[at]]
    pattern <- catalog$voleq[[row]]
    if (!catalog$profile_based[[row]]) {
      candidates[[at]] <- gsub("[?*]", "0", pattern)
      next
    }
    pool <- catalog$voleq[literal & catalog$family == catalog$family[[row]]]
    pool <- pool[vapply(pool, function(id) {
      grepl(merchandiser:::.nvel_identifier_pattern(pattern), id)
    }, logical(1))]
    pool <- pool[has_taper_model(pool)]
    if (length(pool)) {
      candidates[[at]] <- pool[[1L]]
      next
    }
    candidates[[at]] <- switch(pattern,
      `891CLKE***` = "891CLKE100", `891CLKO***` = "891CLKO100",
      `???FW3????` = "300FW3W202", `???F33????` = "A03F33W098",
      `9??CLK????` = "900CLKE012", `8?1CLK????` = "811CLKE100",
      `8??CLK????` = "834CLKE110"
    )
  }
  profile <- catalog$profile_based[rows]
  expect_identical(length(rows), 139L)
  expect_identical(sum(profile), 24L)
  expect_true(all(has_taper_model(candidates[profile])))
  expect_false(any(has_taper_model(candidates[!profile])))
  reasons <- vapply(candidates[!profile], function(id) {
    merchandiser:::.nvel_identifier_resolution(id)$reason
  }, character(1))
  expect_true(all(reasons == "direct volume equations are not yet ported"))
})

test_that("Clark validation identifiers resolve through source validity", {
  source <- Sys.getenv("MERCHANDISER_FIXTURES",
                       unset = Sys.getenv("TREEVOLUME_FIXTURES", unset = ""))
  if (!dir.exists(source)) {
    skip("MERCHANDISER_FIXTURES directory is missing. Clark identifiers not tested")
  }
  path <- file.path(source, "clark_identifiers.csv")
  if (!file.exists(path)) stop("Clark identifier oracle is unavailable: ", path)
  fixture <- utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = c(identifier = "character")
  )
  fixture <- fixture[fixture$discovery_mode == "validation", , drop = FALSE]
  expect_identical(nrow(fixture), 2156L)
  expect_true(all(fixture$DOUBLE_ERRFLAG == 0L))
  expect_true(all(has_taper_model(fixture$identifier)))
})
