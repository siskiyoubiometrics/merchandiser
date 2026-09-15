test_that("only size-one inputs recycle", {
  expect_length(
    dib(
      dbh = 12, ht = 80, h = 1:3, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    3L
  )
  expect_error(
    dib(
      dbh = 1:2, ht = 70:72, h = 10, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value,
    "size-one"
  )
  expect_error(dib(
    dbh = 1:2, ht = 80, h = 10, model = "demo.paraboloid", bark_ratio = 1:3 / 4,
    spcd = .interface_spcd("demo.paraboloid")
  )$value, "size-one")
})

test_that("zero-length inputs produce zero-length outputs", {
  expect_identical(dib(
    dbh = numeric(), ht = 80, h = numeric(), model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value, numeric())
  result <- dib(
    dbh = numeric(), ht = 80, h = numeric(), model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )
  expect_named(result, c("value", "status"))
  expect_equal(nrow(result), 0L)
  expect_error(stem_profile(
    dbh = numeric(), ht = 80, model = "demo.paraboloid", spcd = 202,
    tree_id = integer()
  ), "dbh")
})

test_that("missing and factor model ids follow the input contract", {
  missing <- capture_warnings(dib(
    dbh = 12, ht = 80, h = 20, model = NA_character_,
    spcd = .interface_spcd(NA_character_)
  )$value)
  expect_true(is.na(missing$value))
  expect_length(missing$messages, 0L)
  expect_identical(
    dib(dbh = 12, ht = 80, h = 20, model = NA_character_, spcd = .interface_spcd(
      NA_character_
    ))$status,
    1L
  )
  expect_equal(
    dib(
      dbh = 12, ht = 80, h = 20, model = factor("demo.paraboloid"),
      spcd = .interface_spcd(factor("demo.paraboloid"))
    )$value,
    dib(
      dbh = 12, ht = 80, h = 20, model = "demo.paraboloid",
      spcd = .interface_spcd("demo.paraboloid")
    )$value
  )
})

test_that("input and model statuses follow precedence", {
  result <- dib(dbh = c(NA, 0, 10, 10, 10, 10), ht = c(80, 80, 0, 80, 80, 80), h = c(
    10, 10,
    10, 90, 10, 10
  ), model = c(rep("demo.paraboloid", 5), "missing.model"), spcd = .interface_spcd(c(rep(
    "demo.paraboloid",
    5
  ), "missing.model")))
  expect_identical(result$status, c(1L, 2L, 3L, 4L, 0L, 50L))
})

test_that("diameter and bound input status codes are reachable", {
  diameter <- height_at_dib(
    dbh = c(12, 12), ht = c(80, 80), dib = c(0, 30), model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )
  expect_identical(diameter$status, c(5L, 101L))

  bounds <- stem_volume(
    dbh = 12, ht = 80, model = "demo.paraboloid", from = c(10, 90),
    to = c(10, 80), spcd = .interface_spcd(
      "demo.paraboloid"
    )
  )
  expect_identical(bounds$status, c(6L, 4L))
})

test_that("species and auxiliary statuses are evaluated after grouping", {
  register_test_model("private.scope", spcd = 122L, inputs = list(
    required = "site_index",
    optional = character(), pairs = list()
  ))
  on.exit(unregister_taper_model("private.scope"), add = TRUE)

  missing <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope",
    spcd = .interface_spcd("private.scope")
  )
  expect_identical(missing$status, 51L)
  supplied_missing <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope", site_index = NA_real_,
    spcd = .interface_spcd("private.scope")
  )
  expect_identical(supplied_missing$status, 1L)
  unknown_species <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope", site_index = 80,
    spcd = 0
  )
  expect_identical(unknown_species$status, 7L)
  large_species <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope", site_index = 80,
    spcd = 1e+20
  )
  expect_identical(large_species$status, 7L)
  outside <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope",
    site_index = 80, spcd = 202L
  )
  expect_identical(outside$status, 52L)
  expect_true(is.finite(outside$value))
  expect_error(dib(
    dbh = 10, ht = 80, h = 20, model = "private.scope", site_index = 80, mystery = 1,
    spcd = .interface_spcd("private.scope")
  )$value, "undeclared")
  precedence <- dib(dbh = 10, ht = 80, h = 20, model = "missing.model", spcd = 0)
  expect_identical(precedence$status, 7L)
  expect_identical(
    dib(
      dbh = 10, ht = 80, h = 20, model = "demo.paraboloid", bark_ratio = 2,
      spcd = .interface_spcd("demo.paraboloid")
    )$status,
    55L
  )
})

test_that("capability and callback errors become model statuses", {
  register_test_model("private.no_bark", bark_ratio = NA_real_)
  on.exit(unregister_taper_model("private.no_bark"), add = TRUE)
  expect_identical(
    dob(
      dbh = 10, ht = 80, h = 20, model = "private.no_bark",
      spcd = .interface_spcd("private.no_bark")
    )$status,
    53L
  )

  failing <- function(dbh, ht, h, aux) {
    if (any(dbh == 13))
      stop("deliberate boom")
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.failing", dib = failing)
  on.exit(unregister_taper_model("private.failing"), add = TRUE)
  expect_identical(
    dib(
      dbh = 13, ht = 80, h = 20, model = "private.failing",
      spcd = .interface_spcd("private.failing")
    )$status,
    54L
  )
  warning <- capture_warnings(dib(
    dbh = 13, ht = 80, h = 20,
    model = "private.failing", spcd = .interface_spcd("private.failing")
  )$value)
  expect_length(warning$messages, 0L)
})

test_that("warnings are consolidated and sorted by code", {
  warnings <- capture_warnings(dib(
    dbh = c(0, 0, 10), ht = c(80, 80, 0), h = 10, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value)
  expect_length(warnings$messages, 0L)

  no_warning <- capture_warnings(dib(
    dbh = c(0, 10), ht = c(80, 0), h = 10, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  ))
  expect_length(no_warning$messages, 0L)
  expect_named(no_warning$value, c("value", "status"))

  error_a <- function(dbh, ht, h, aux) {
    if (any(dbh == 11))
      stop("error a")
    as.double(dbh * (1 - h / ht))
  }
  error_b <- function(dbh, ht, h, aux) {
    if (any(dbh == 12))
      stop("error b")
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.error_a", dib = error_a)
  register_test_model("private.error_b", dib = error_b)
  on.exit(unregister_taper_model("private.error_a"), add = TRUE)
  on.exit(unregister_taper_model("private.error_b"), add = TRUE)
  consolidated <- capture_warnings(dib(dbh = c(11, 12), ht = 80, h = 20, model = c(
    "private.error_a",
    "private.error_b"
  ), spcd = .interface_spcd(c("private.error_a", "private.error_b")))$value)
  expect_length(consolidated$messages, 0L)
})

test_that("stem_profile has the binding public signature", {
  expect_identical(names(formals(stem_profile)), c(
    "tree_id", "dbh", "ht", "spcd", "model",
    "step", "from", "to", "from_dib", "from_dob", "to_dib", "to_dob", "stump_ht", "..."
  ))
})

test_that("scalar controls and reserved auxiliaries are rejected", {
  expect_error(
    stem_volume(10, 80, 202, model = "demo.paraboloid", inside_bark = "outside"),
    "inside_bark"
  )
  expect_error(merchandiser:::.capture_aux(list(id = 1)), "reserved")
  expect_error(merchandiser:::.capture_aux(list(1)), "unique")
  expect_error(merchandiser:::.capture_aux(list(a = 1, a = 2)), "unique")
  expect_error(merchandiser:::.capture_aux(list(a = list(1))), "atomic")
  expect_error(
    dib(dbh = 10, ht = 80, h = 10, model = "demo.paraboloid", spcd = "122")$value,
    "spcd.*numeric"
  )
  expect_error(dib(
    dbh = 10, ht = 80, h = 10, model = "demo.paraboloid", bark_ratio = "0.9",
    spcd = .interface_spcd("demo.paraboloid")
  )$value, "bark_ratio.*wrong type")
})

test_that("paired auxiliaries are complete for each selected model", {
  paired_dib <- function(dbh, ht, h, aux) {
    as.double(dbh * (1 - h / ht) + aux$upper_ht1 - aux$upper_d1)
  }
  register_test_model("private.paired", dib = paired_dib, inputs = list(
    required = character(),
    optional = c("upper_ht1", "upper_d1"), pairs = list(c("upper_ht1", "upper_d1"))
  ))
  on.exit(unregister_taper_model("private.paired"), add = TRUE)

  absent <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.paired",
    spcd = .interface_spcd("private.paired")
  )
  expect_identical(absent$status, 54L)
  partial <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.paired", upper_ht1 = 1,
    spcd = .interface_spcd("private.paired")
  )
  expect_identical(partial$status, 51L)
  row_missing <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.paired", upper_ht1 = 1, upper_d1 = NA_real_,
    spcd = .interface_spcd("private.paired")
  )
  expect_identical(row_missing$status, 1L)
  complete <- dib(
    dbh = 10, ht = 80, h = 20, model = "private.paired", upper_ht1 = 1, upper_d1 = 1,
    spcd = .interface_spcd("private.paired")
  )$value
  expect_true(is.finite(complete))
})

test_that("auxiliary domains are enforced only for declaring model rows", {
  inputs <- list(required = character(), optional = c(
    "form_class", "upper_bark", "decay_class",
    "cull"
  ), pairs = list())
  register_test_model("private.domains", inputs = inputs)
  on.exit(unregister_taper_model("private.domains"), add = TRUE)

  expect_identical(
    dib(
      dbh = 10, ht = 80, h = 20, model = "private.domains", form_class = 0,
      spcd = .interface_spcd("private.domains")
    )$status,
    55L
  )
  expect_identical(dib(
    dbh = 10, ht = 80, h = 20, model = "private.domains", upper_bark = "outside",
    spcd = .interface_spcd("private.domains")
  )$status, 55L)
  expect_identical(
    dib(
      dbh = 10, ht = 80, h = c(20, 21), model = "private.domains",
      decay_class = c(
        0,
        2
      ), spcd = .interface_spcd("private.domains")
    )$status,
    c(55L, 0L)
  )
  expect_identical(
    dib(
      dbh = 10, ht = 80, h = 20, model = "private.domains", cull = 101,
      spcd = .interface_spcd("private.domains")
    )$status,
    55L
  )
  expect_identical(dib(
    dbh = 10, ht = 80, h = 20, model = "private.domains", cull = NA_real_,
    spcd = .interface_spcd("private.domains")
  )$status, 1L)

  ignored <- dib(
    dbh = 10, ht = 80, h = 20, model = c("private.domains", "demo.paraboloid"),
    form_class = c(80, -1), spcd = .interface_spcd(c("private.domains", "demo.paraboloid"))
  )
  expect_identical(ignored$status, c(0L, 0L))
})

test_that("status 52 never displaces a numeric failure", {
  details <- rep(NA_character_, 1L)
  numeric_after <- merchandiser:::.merge_kernel_result(52L, details, 1L, list(
    value = NA_real_,
    status = 103L, details = "did not converge"
  ))
  expect_identical(numeric_after$status, 103L)

  scope_after <- merchandiser:::.merge_kernel_result(103L, details, 1L, list(
    value = 1, status = 52L,
    details = "out of scope"
  ))
  expect_identical(scope_after$status, 103L)
  expect_true(is.na(scope_after$details))

  positive_tip <- function(dbh, ht, h, aux) {
    as.double(2 + dbh * (1 - h / ht))
  }
  register_test_model("private.scope_numeric", dib = positive_tip, spcd = 122L)
  on.exit(unregister_taper_model("private.scope_numeric"), add = TRUE)
  public <- height_at_dib(
    dbh = 10, ht = 80, dib = 1,
    model = "private.scope_numeric", spcd = 999L
  )
  expect_identical(public$status, 100L)
  expect_true(is.na(public$value))
  warning_result <- capture_warnings(height_at_dib(
    dbh = 10, ht = 80, dib = 1, model = "private.scope_numeric",
    spcd = 999L
  )$value)
  expect_true(is.na(warning_result$value))
  expect_length(warning_result$messages, 0L)
})

test_that("scalar and factor model ids avoid character expansion", {
  row_count <- 5000000L
  dbh <- rep(10, row_count)
  gc(reset = TRUE)
  before <- gc()["Vcells", "max used"] * 8
  prepared <- merchandiser:::.prepare_vectors(
    list(
      dbh = dbh, ht = 80, h = 20,
      model = "demo.paraboloid"
    ),
    numeric_names = c("dbh", "ht", "h"), character_names = "model", aux = list()
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
