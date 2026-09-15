test_that("a bad stump, species, defect, or auxiliary affects only its tree", {
  trees <- hardening_trees()
  for (height in c(60, 61)) {
    args <- trees
    args$stump_ht <- replace(rep(1, 6), 3, height)
    expect_one_bad_tree(args, 411L)
  }
  args <- trees
  args$spcd[3] <- 99999
  expect_one_bad_tree(args, 7L)
  args <- trees
  args$defects <- defect(3, 20, 30, "restrict", product = "unknown")
  expect_one_bad_tree(args, 407L)
  for (record in list(defect(3, -1, 20, "cull"), defect(3, 20, 10, "cull"),
                      defect(3, 10, 20, "sweep", percent = 101))) {
    args <- trees
    args$defects <- record
    code <- validate_defects(record, trees$tree_id, trees$ht, trees$products)$status
    expect_true(code %in% 401:403)
    expect_one_bad_tree(args, code)
  }
  args <- trees
  args$bark_ratio <- replace(rep(0.9, 6), 3, -1)
  result <- do.call(merchandise, args)
  expect_identical(result$status$status, 55L)
  expect_identical(result$status$tree_id, 3L)
  expect_setequal(unique(result$logs$tree_id), (1:6)[-3])
  args$bark_ratio[3] <- 0.9
  clean <- do.call(merchandise, args)$logs
  clean <- clean[clean$tree_id != 3, ]
  rownames(clean) <- NULL
  expect_identical(result$logs, clean)
})

test_that("unknown defect trees are diagnosed and dropped with one warning", {
  trees <- hardening_trees()
  d <- defect(c(7, 8), 10, 20, "cull")
  checked <- validate_defects(d, trees$tree_id, trees$ht, trees$products)
  expect_identical(checked$status, c(405L, 405L))
  trees$defects <- d
  expect_warning(result <- do.call(merchandise, trees), "tree_id: 7, 8")
  expect_identical(result$logs, do.call(merchandise, hardening_trees())$logs)
  expect_equal(nrow(result$status), 0)
})

test_that("auxiliary domains are row statuses in every stem function", {
  args <- hardening_trees()[c("dbh", "ht", "spcd", "model")]
  args$bark_ratio <- replace(rep(0.9, 6), 3, -1)
  for (fn in c("dib", "dob", "height_at_dib", "height_at_dob", "stem_volume")) {
    extra <- switch(fn, dib = list(h = 20), dob = list(h = 20),
                    height_at_dib = list(dib = 6), height_at_dob = list(dob = 6), list())
    result <- do.call(fn, c(args, extra))
    expect_identical(result$status, replace(integer(6), 3, 55L))
    expect_true(is.na(result$value[3]))
    expect_true(all(is.finite(result$value[-3])))
  }
  result <- do.call(stem_profile, c(list(tree_id = 1:6), args))
  expect_identical(result$status[result$tree_id == 3], 55L)
  expect_true(all(is.na(result$dib[result$tree_id == 3])))
  expect_true(all(result$status[result$tree_id != 3] == 0L))
})

test_that("whole-call shape errors still identify their argument", {
  args <- hardening_trees()
  args$dbh <- as.character(args$dbh)
  expect_error(do.call(merchandise, args), "dbh")
  args <- hardening_trees()
  args$dbh <- NULL
  expect_error(do.call(merchandise, args), "dbh")
  args <- hardening_trees()
  args$products <- args$products[FALSE, ]
  expect_error(do.call(merchandise, args), "products")
  args <- hardening_trees()
  args$tree_id[3] <- NA
  expect_error(do.call(merchandise, args), "tree_id")
  args$tree_id[3] <- 1
  expect_error(do.call(merchandise, args), "tree_id")
})

test_that("mixed models validate only their declared auxiliary measurements", {
  args <- hardening_trees()
  register_test_model("hardening.domains", inputs = list(
    required = character(), optional = "form_class", pairs = list()
  ))
  on.exit(unregister_taper_model("hardening.domains"), add = TRUE)
  args$model <- rep(c("hardening.domains", "demo.paraboloid"), 3)
  args$form_class <- c(80, -1, -1, -1, 80, -1)
  result <- do.call(merchandise, args)
  expect_identical(result$status$tree_id, 3L)
  expect_identical(result$status$status, 55L)
  expect_setequal(unique(result$logs$tree_id), (1:6)[-3])
})

test_that("misplaced defect products affect only their tree", {
  for (effect in c("cull", "end", "sweep")) {
    args <- hardening_trees()
    args$defects <- defect(3, 10, if (effect == "end") NA_real_ else 20,
      effect, product = "saw", percent = if (effect == "sweep") 10 else NULL
    )
    checked <- validate_defects(args$defects, args$tree_id, args$ht, args$products)
    expect_identical(checked$status, 409L)
    expect_one_bad_tree(args, 409L)
    args$defects$tree_id <- 99
    expect_warning(result <- do.call(merchandise, args), "tree_id: 99")
    expect_identical(result$logs, do.call(merchandise, hardening_trees())$logs)
    expect_equal(nrow(result$status), 0)
  }
})

test_that("orphans are dropped before any defect consistency check", {
  args <- hardening_trees()
  args$defects <- defect(c(98, 99), -1, -2, "cull", product = "saw")
  args$defects$effect <- "invalid"
  expect_warning(result <- do.call(merchandise, args), "tree_id: 98, 99")
  expect_identical(result$logs, do.call(merchandise, hardening_trees())$logs)
})

test_that("stump and pruning domains are isolated to each tree", {
  for (name in c("stump_ht", "pruned_ht")) {
    for (value in c(-.Machine$double.eps, Inf, -Inf, NaN, NA_real_, 60, 61)) {
      if (name == "pruned_ht" && identical(value, 60))
        next
      args <- hardening_trees()
      args[[name]] <- replace(rep(1, 6), 3, value)
      expect_one_bad_tree(args, if (name == "stump_ht") 411L else 408L)
    }
    for (value in list("bad", 1:2)) {
      args <- hardening_trees()
      args[[name]] <- value
      expect_error(do.call(merchandise, args), name)
    }
  }
  args <- hardening_trees()
  args$ht[3] <- 0
  expect_one_bad_tree(args, 3L)
})


test_that("a misplaced sweep product is diagnosed even without a percentage", {
  args <- hardening_trees()
  args$defects <- defect(3, 10, 20, "sweep", product = "saw")
  checked <- validate_defects(args$defects, args$tree_id, args$ht, args$products)
  expect_identical(checked$status, 409L)
  expect_one_bad_tree(args, 409L)
  args$stump_ht <- NULL
  expect_error(do.call(merchandise, c(args, list(stump_ht = NULL))), "stump_ht")
})
