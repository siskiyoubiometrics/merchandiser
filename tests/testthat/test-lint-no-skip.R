# Parse every test and helper so comments and quoted examples cannot hide a
# forbidden call or be mistaken for one. Only the true branch of a verified
# fixture-directory or benchmark condition permits a plain skip.
find_forbidden_skips <- function(code) {
  violations <- character()
  fixture_value <- quote(Sys.getenv(
    "MERCHANDISER_FIXTURES", unset = Sys.getenv("TREEVOLUME_FIXTURES", unset = "")
  ))
  benchmark_value <- quote(Sys.getenv(
    "MERCHANDISER_BENCH", unset = Sys.getenv("TREEVOLUME_BENCH", unset = "")
  ))
  walk <- function(node, bindings = list(), guarded = FALSE) {
    if (missing(node)) return(bindings)
    if (is.expression(node)) node <- as.call(c(list(as.name("{")), as.list(node)))
    if (!is.call(node)) return(bindings)
    head <- node[[1L]]
    name <- if (is.symbol(head)) as.character(head) else ""
    if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
      name <- as.character(head[[3L]])
    }
    if (grepl("^skip($|_)", name)) {
      allowed <- name == "skip_if_not_installed" ||
        (name == "skip" && guarded)
      if (!allowed) violations <<- c(violations, paste(deparse(node), collapse = " "))
    }
    if (name == "{") {
      for (child in as.list(node)[-1L]) bindings <- walk(child, bindings, guarded)
      return(bindings)
    }
    if (name == "<-" && is.symbol(node[[2L]])) {
      walk(node[[3L]], bindings, guarded)
      bindings[as.character(node[[2L]])] <- list(node[[3L]])
      return(bindings)
    }
    if (name == "function") {
      walk(node[[3L]])
      return(bindings)
    }
    if (name == "if") {
      condition <- node[[2L]]
      permitted <- FALSE
      for (variable in names(bindings)) {
        symbol <- as.name(variable)
        if (identical(bindings[[variable]], fixture_value)) {
          permitted <- permitted || identical(
            condition, substitute(!dir.exists(x), list(x = symbol))
          )
        }
        if (identical(bindings[[variable]], benchmark_value)) {
          permitted <- permitted || identical(condition, substitute(x != "true", list(x = symbol)))
        }
      }
      walk(condition, bindings, guarded)
      walk(node[[3L]], bindings, permitted)
      if (length(node) == 4L) walk(node[[4L]], bindings, FALSE)
      return(bindings)
    }
    for (index in seq_along(node)[-1L]) {
      if (!identical(node[[index]], quote(expr = ))) walk(node[[index]], bindings, guarded)
    }
    bindings
  }
  walk(code)
  violations
}

test_that("every test file uses only the three permitted skip forms", {
  files <- list.files(testthat::test_path(), pattern = "[.][Rr]$",
                      recursive = TRUE, full.names = TRUE)
  for (file in files) {
    expect_identical(find_forbidden_skips(parse(file)), character(), info = basename(file))
  }
  for (code in c(
    'skip("failure")', "testthat::skip_if(TRUE)", "skip_if_not(FALSE)",
    "skip_on_ci()", "testthat::skip_on_cran()", 'if (TRUE) skip("missing fixtures")',
    'root <- "arbitrary"; if (!dir.exists(root)) skip("fixtures")',
    'if (checksum_mismatch) testthat::skip("bad checksum")'
  )) {
    expect_length(find_forbidden_skips(parse(text = code)), 1L)
  }
  expect_identical(find_forbidden_skips(parse(text = c(
    'skip_if_not_installed("optional")', 'testthat::skip_if_not_installed("optional")'
  ))), character())
  for (variable in c("FIXTURES", "BENCH")) {
    binding <- sprintf(
      'guard <- Sys.getenv("MERCHANDISER_%s", unset = Sys.getenv("TREEVOLUME_%s", unset = ""))',
      variable, variable
    )
    condition <- if (variable == "FIXTURES") "!dir.exists(guard)" else 'guard != "true"'
    expect_identical(find_forbidden_skips(parse(text = c(
      binding, sprintf('if (%s) testthat::skip("guarded")', condition)
    ))), character())
    expect_length(find_forbidden_skips(parse(text = c(
      binding, sprintf('if (%s) TRUE else skip("wrong branch")', condition)
    ))), 1L)
  }

})

test_that("a checksum mismatch fails instead of skipping the oracle test", {
  check <- mc_full_oracle_agreement
  environment(check) <- list2env(list(
    .mc_oracle_manifest = function(root, expected_files) {
      list(checksum_mismatch = rep(TRUE, length(expected_files)))
    }
  ), parent = environment(check))
  expect_error(check(tempdir()), "Oracle fixture checksum mismatch")
})

test_that("a missing fixture directory still skips", {
  missing <- file.path(tempdir(), "merchandiser-fixtures-do-not-exist")
  expect_false(dir.exists(missing))
  previous <- Sys.getenv("MERCHANDISER_FIXTURES", unset = NA_character_)
  on.exit({
    if (is.na(previous)) Sys.unsetenv("MERCHANDISER_FIXTURES") else
      Sys.setenv(MERCHANDISER_FIXTURES = previous)
  }, add = TRUE)
  Sys.setenv(MERCHANDISER_FIXTURES = missing)
  guards <- 0L
  files <- list.files(testthat::test_path(), pattern = "^test-(stem-nvel-|oracle-).*\\.R$",
                      full.names = TRUE)
  for (file in files) {
    for (block in parse(file)) {
      if (!is.call(block) || !identical(block[[1L]], as.name("test_that"))) next
      body <- block[[3L]]
      if (!is.call(body) || length(body) < 3L) next
      guard <- as.call(as.list(body)[1:3])
      if (!grepl("MERCHANDISER_FIXTURES", paste(deparse(guard), collapse = " "))) next
      guards <- guards + 1L
      condition <- tryCatch(eval(guard), skip = identity)
      expect_s3_class(condition, "skip")
    }
  }
  expect_identical(guards, 8L)
})
