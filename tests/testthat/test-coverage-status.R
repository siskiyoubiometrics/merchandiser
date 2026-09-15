test_that("coverage mixed invalid trees retain their individual statuses and residual causes", {
  ids <- c("dbh", "species", "model", "unresolved", "short", "stump", "pruned",
           "ineligible", "diameter", "valid")
  dbh <- rep(20, length(ids))
  ht <- rep(80, length(ids))
  spcd <- rep(202, length(ids))
  model <- rep("F00FW2W202", length(ids))
  stump <- rep(1, length(ids))
  pruned <- rep(0, length(ids))
  # Described inputs isolate each failure, with valid explicit models and zero
  # pruning on every other row. Codes come from R/stem-status.R and status_codes.
  dbh[ids == "dbh"] <- -1
  spcd[ids == "species"] <- -1
  model[ids == "model"] <- "coverage.unregistered"
  model[ids == "unresolved"] <- ""
  ht[ids == "short"] <- 5
  stump[ids == "stump"] <- -1
  pruned[ids == "pruned"] <- -1
  dbh[ids == "ineligible"] <- 8
  dbh[ids == "diameter"] <- 40
  p <- coverage_product(min_dbh = 10, max_led = 25)
  x <- merchandise(ids, dbh, ht, spcd, p, model = model, stump_ht = stump, pruned_ht = pruned)
  expected <- c(dbh = 2L, species = 7L, model = 50L, unresolved = 404L, short = 410L,
                stump = 411L, pruned = 408L, ineligible = 400L, diameter = 410L)
  expect_identical(x$status$tree_id, names(expected))
  expect_identical(x$status$status, unname(expected))
  expect_setequal(unique(x$logs$tree_id), "valid")
  expect_identical(x$residuals$cause[x$residuals$tree_id == "ineligible"],
                   c("stump", "no_entry_product"))
  expect_true("diameter_limit" %in% x$residuals$cause[x$residuals$tree_id == "diameter"])
})

test_that("coverage dbh equal to max_dbh is strictly ineligible", {
  p <- coverage_product(max_dbh = 20)
  x <- merchandise(1, 20, 80, 202, p, model = "F00FW2W202")
  # Source: product contract dbh < max_dbh, status_codes no eligible product = 400.
  expect_identical(x$status$status, 400L)
})

test_that("coverage missing age is ineligible for an age-limited product", {
  p <- coverage_product(min_age = 20)
  x <- merchandise(1, 20, 80, 202, p, model = "F00FW2W202", age = NA_real_)
  # Source: product age eligibility requires a finite age, status_codes = 400.
  expect_identical(x$status$status, 400L)
})

test_that("coverage product species exclusions leave no entry product", {
  p <- coverage_product(spcd = 131)
  x <- merchandise(1, 20, 80, 202, p, model = "F00FW2W202")
  # Source: product species eligibility and status_codes no eligible product = 400.
  expect_identical(x$status$status, 400L)
  expect_true("no_entry_product" %in% x$residuals$cause)
})

test_that("coverage sweep exactly at the limit is accepted", {
  p <- coverage_product(max_sweep = 25)
  records <- defect(1, 0, 80, "sweep", percent = p$max_sweep)
  x <- merchandise(1, 20, 80, 202, p, model = "F00FW2W202", defects = records)
  # Source: sweep <= max_sweep is eligible, yielding logs and no nonzero status.
  expect_equal(nrow(x$status), 0)
  expect_gt(nrow(x$logs), 0)
})

for (kind in c("Date", "factor", "logical")) {
  test_that(paste("coverage", kind, "tree identifiers survive every result table"), {
    ids <- switch(kind, Date = as.Date(c("2026-09-14", "2026-09-15")),
                  factor = factor(c("valid", "invalid")), logical = c(TRUE, FALSE))
    x <- merchandise(ids, c(20, -1), 80, 202, coverage_product(), model = "F00FW2W202")
    # Source: identifier preservation contract, with successful and failed rows
    # so logs, residuals, status and assumptions all contain actual identifiers.
    for (table in x[c("logs", "residuals", "status", "assumptions")]) {
      expect_gt(nrow(table), 0)
      expect_false(anyNA(table$tree_id))
      expect_identical(class(table$tree_id), class(ids))
      expect_identical(table$tree_id, ids[match(table$tree_id, ids)])
    }
    expect_identical(unique(x$logs$tree_id), ids[1])
    expect_identical(x$status$tree_id, ids[2])
  })
}
