test_that("only size-one inputs recycle", {
  expect_length(dib(12, 80, 1:3, "demo.paraboloid"), 3L)
  expect_error(
    dib(1:2, 70:72, 10, "demo.paraboloid"),
    "size-one"
  )
  expect_error(
    dib(1:2, 80, 10, "demo.paraboloid", bark_ratio = 1:3 / 4),
    "size-one"
  )
})

test_that("zero-length inputs produce zero-length outputs", {
  expect_identical(dib(numeric(), 80, numeric(), "demo.paraboloid"), numeric())
  result <- dib(numeric(), 80, numeric(), "demo.paraboloid", status = TRUE)
  expect_named(result, c("value", "status"))
  expect_equal(nrow(result), 0L)
  expect_equal(nrow(stem_profile(numeric(), 80, "demo.paraboloid")), 0L)
})

test_that("missing and factor model ids follow the input contract", {
  missing <- capture_warnings(dib(12, 80, 20, NA_character_))
  expect_true(is.na(missing$value))
  expect_length(missing$messages, 0L)
  expect_identical(
    dib(12, 80, 20, NA_character_, status = TRUE)$status,
    1L
  )
  expect_equal(
    dib(12, 80, 20, factor("demo.paraboloid")),
    dib(12, 80, 20, "demo.paraboloid")
  )
})

test_that("input and model statuses follow precedence", {
  result <- dib(
    dbh = c(NA, 0, 10, 10, 10, 10),
    ht = c(80, 80, 0, 80, 80, 80),
    h = c(10, 10, 10, 90, 10, 10),
    model = c(rep("demo.paraboloid", 5), "missing.model"),
    status = TRUE
  )
  expect_identical(result$status, c(1L, 2L, 3L, 4L, 0L, 50L))
})

test_that("diameter and bound input status codes are reachable", {
  diameter <- height_at_dib(
    c(12, 12), c(80, 80), c(0, 30), "demo.paraboloid", status = TRUE
  )
  expect_identical(diameter$status, c(5L, 101L))

  bounds <- stem_volume(
    12, 80, "demo.paraboloid", lower = c(10, 90),
    lower_type = "height", upper = c(10, 0), upper_type = c("height", "tip"),
    status = TRUE
  )
  expect_identical(bounds$status, c(6L, 4L))
})

test_that("species and auxiliary statuses are evaluated after grouping", {
  register_test_model(
    "private.scope", species = 122L,
    inputs = list(required = "site_index", optional = character(), pairs = list())
  )
  on.exit(unregister_taper_model("private.scope"), add = TRUE)

  missing <- dib(10, 80, 20, "private.scope", status = TRUE)
  expect_identical(missing$status, 51L)
  supplied_missing <- dib(
    10, 80, 20, "private.scope", site_index = NA_real_, status = TRUE
  )
  expect_identical(supplied_missing$status, 1L)
  unknown_species <- dib(
    10, 80, 20, "private.scope", site_index = 80, spcd = 0, status = TRUE
  )
  expect_identical(unknown_species$status, 7L)
  large_species <- dib(
    10, 80, 20, "private.scope", site_index = 80, spcd = 1e20, status = TRUE
  )
  expect_identical(large_species$status, 7L)
  outside <- dib(
    10, 80, 20, "private.scope", site_index = 80, spcd = 202L, status = TRUE
  )
  expect_identical(outside$status, 52L)
  expect_true(is.finite(outside$value))
  expect_error(
    dib(10, 80, 20, "private.scope", site_index = 80, mystery = 1),
    "undeclared"
  )
  precedence <- dib(
    10, 80, 20, "missing.model", spcd = 0, status = TRUE
  )
  expect_identical(precedence$status, 7L)
  expect_error(
    dib(10, 80, 20, "demo.paraboloid", bark_ratio = 2, status = TRUE),
    "bark_ratio has 1 out-of-domain value"
  )
})

test_that("capability and callback errors become model statuses", {
  register_test_model("private.no_bark", bark_ratio = NA_real_)
  on.exit(unregister_taper_model("private.no_bark"), add = TRUE)
  expect_identical(
    dob(10, 80, 20, "private.no_bark", status = TRUE)$status,
    53L
  )

  failing <- function(dbh, ht, h, aux) {
    if (any(dbh == 13)) stop("deliberate boom")
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.failing", dib = failing)
  on.exit(unregister_taper_model("private.failing"), add = TRUE)
  expect_identical(
    dib(13, 80, 20, "private.failing", status = TRUE)$status,
    54L
  )
  warning <- capture_warnings(dib(13, 80, 20, "private.failing"))
  expect_match(warning$messages, "deliberate boom")
})

test_that("warnings are consolidated and sorted by code", {
  warnings <- capture_warnings(dib(
    dbh = c(0, 0, 10), ht = c(80, 80, 0), h = 10,
    model = "demo.paraboloid"
  ))
  expect_length(warnings$messages, 2L)
  expect_match(warnings$messages[[1L]], "dbh_nonpositive")
  expect_match(warnings$messages[[2L]], "ht_nonpositive")

  no_warning <- capture_warnings(dib(
    dbh = c(0, 10), ht = c(80, 0), h = 10,
    model = "demo.paraboloid", status = TRUE
  ))
  expect_length(no_warning$messages, 0L)
  expect_named(no_warning$value, c("value", "status"))

  error_a <- function(dbh, ht, h, aux) {
    if (any(dbh == 11)) stop("error a")
    as.double(dbh * (1 - h / ht))
  }
  error_b <- function(dbh, ht, h, aux) {
    if (any(dbh == 12)) stop("error b")
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.error_a", dib = error_a)
  register_test_model("private.error_b", dib = error_b)
  on.exit(unregister_taper_model("private.error_a"), add = TRUE)
  on.exit(unregister_taper_model("private.error_b"), add = TRUE)
  consolidated <- capture_warnings(dib(
    c(11, 12), 80, 20, c("private.error_a", "private.error_b")
  ))
  expect_length(consolidated$messages, 1L)
  expect_match(consolidated$messages, "kernel_error for 2 of 2 trees")
  expect_match(consolidated$messages, "error a.*error b")
})

test_that("stem_profile has the binding public signature", {
  expect_identical(names(formals(stem_profile)), c(
    "dbh", "ht", "model", "step", "id", "lower", "lower_type", "upper",
    "upper_type", "...", "units", "status"
  ))
})

test_that("scalar controls and reserved auxiliaries are rejected", {
  expect_error(dib(10, 80, 10, "demo.paraboloid", units = "Imperial"), "units")
  expect_error(dib(10, 80, 10, "demo.paraboloid", status = NA), "status")
  expect_error(stem_volume(10, 80, "demo.paraboloid", bark = "both"), "bark")
  expect_error(
    merchandiser:::.capture_aux(list(id = 1)),
    "reserved"
  )
  expect_error(merchandiser:::.capture_aux(list(1)), "unique")
  expect_error(merchandiser:::.capture_aux(list(a = 1, a = 2)), "unique")
  expect_error(merchandiser:::.capture_aux(list(a = list(1))), "atomic")
  expect_error(
    dib(10, 80, 10, "demo.paraboloid", spcd = "122"),
    "spcd.*wrong type"
  )
  expect_error(
    dib(10, 80, 10, "demo.paraboloid", bark_ratio = "0.9"),
    "bark_ratio.*wrong type"
  )
})

test_that("paired auxiliaries are complete for each selected model", {
  paired_dib <- function(dbh, ht, h, aux) {
    as.double(dbh * (1 - h / ht) + aux$upper_ht1 - aux$upper_d1)
  }
  register_test_model(
    "private.paired",
    dib = paired_dib,
    inputs = list(
      required = character(), optional = c("upper_ht1", "upper_d1"),
      pairs = list(c("upper_ht1", "upper_d1"))
    )
  )
  on.exit(unregister_taper_model("private.paired"), add = TRUE)

  absent <- dib(10, 80, 20, "private.paired", status = TRUE)
  expect_identical(absent$status, 54L)
  partial <- dib(
    10, 80, 20, "private.paired", upper_ht1 = 1, status = TRUE
  )
  expect_identical(partial$status, 51L)
  row_missing <- dib(
    10, 80, 20, "private.paired", upper_ht1 = 1,
    upper_d1 = NA_real_, status = TRUE
  )
  expect_identical(row_missing$status, 1L)
  complete <- dib(
    10, 80, 20, "private.paired", upper_ht1 = 1, upper_d1 = 1
  )
  expect_true(is.finite(complete))
})

test_that("auxiliary domains are enforced only for declaring model rows", {
  inputs <- list(
    required = character(),
    optional = c("form_class", "upper_bark", "decay_class", "cull"),
    pairs = list()
  )
  register_test_model("private.domains", inputs = inputs)
  on.exit(unregister_taper_model("private.domains"), add = TRUE)

  expect_error(
    dib(10, 80, 20, "private.domains", form_class = 0),
    "form_class has 1 out-of-domain value"
  )
  expect_error(
    dib(10, 80, 20, "private.domains", upper_bark = "outside"),
    "upper_bark has 1 out-of-domain value"
  )
  expect_error(
    dib(10, 80, c(20, 21), "private.domains", decay_class = c(0, 2)),
    "decay_class has 1 out-of-domain value"
  )
  expect_error(
    dib(10, 80, 20, "private.domains", cull = 101),
    "cull has 1 out-of-domain value"
  )
  expect_identical(
    dib(10, 80, 20, "private.domains", cull = NA_real_, status = TRUE)$status,
    1L
  )

  ignored <- dib(
    10, 80, 20, c("private.domains", "demo.paraboloid"),
    form_class = c(80, -1), status = TRUE
  )
  expect_identical(ignored$status, c(0L, 0L))
})

test_that("status 52 never displaces a numeric failure", {
  details <- rep(NA_character_, 1L)
  numeric_after <- merchandiser:::.merge_kernel_result(
    52L, details, 1L,
    list(value = NA_real_, status = 103L, details = "did not converge")
  )
  expect_identical(numeric_after$status, 103L)

  scope_after <- merchandiser:::.merge_kernel_result(
    103L, details, 1L,
    list(value = 1, status = 52L, details = "out of scope")
  )
  expect_identical(scope_after$status, 103L)
  expect_true(is.na(scope_after$details))

  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  register_test_model(
    "private.scope_numeric", dib = positive_tip, species = 122L
  )
  on.exit(unregister_taper_model("private.scope_numeric"), add = TRUE)
  public <- height_at_dib(
    10, 80, 1, "private.scope_numeric", spcd = 999L, status = TRUE
  )
  expect_identical(public$status, 100L)
  expect_true(is.na(public$value))
  warning_result <- capture_warnings(height_at_dib(
    10, 80, 1, "private.scope_numeric", spcd = 999L
  ))
  expect_true(is.na(warning_result$value))
  expect_length(warning_result$messages, 1L)
  expect_match(warning_result$messages, "above_tip.*NA returned")
  expect_false(grepl("species_out_of_scope", warning_result$messages))
})

test_that("scalar and factor model ids avoid character expansion", {
  row_count <- 5000000L
  dbh <- rep(10, row_count)
  gc(reset = TRUE)
  before <- gc()["Vcells", "max used"] * 8
  prepared <- merchandiser:::.prepare_vectors(
    list(dbh = dbh, ht = 80, h = 20, model = "demo.paraboloid"),
    numeric_names = c("dbh", "ht", "h"), character_names = "model",
    aux = list()
  )
  after <- gc()["Vcells", "max used"] * 8
  expect_identical(prepared$size, row_count)
  expect_length(prepared$values$model, 1L)
  expect_lt(after - before, row_count * 21)

  gc(reset = TRUE)
  before <- gc()["Vcells", "max used"] * 8
  scalar <- merchandiser:::.dictionary_groups("demo.paraboloid", row_count)
  after <- gc()["Vcells", "max used"] * 8
  expect_length(scalar$dictionary, 1L)
  expect_length(scalar$encoded, 1L)
  expect_length(scalar$groups[[1L]], row_count)
  expect_lt(after - before, row_count * 4)

  ids <- factor(c("demo.paraboloid", "demo.paraboloid.r", "demo.paraboloid"))
  tracemem(ids)
  copies <- capture.output(grouped <- merchandiser:::.dictionary_groups(ids))
  untracemem(ids)
  expect_length(copies, 0L)
  expect_identical(grouped$dictionary, levels(ids))
  expect_identical(grouped$encoded, ids)
})
