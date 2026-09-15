hardening_product <- function() {
  product("saw", min_length = 16, max_length = 16, min_sed = 6,
    volume_unit = "cubic", price = 1
  )
}

hardening_trees <- function() {
  list(tree_id = 1:6, dbh = rep(16, 6), ht = rep(60, 6), spcd = rep(202, 6),
    products = hardening_product(), model = "F00FW2W202", quiet = TRUE
  )
}

expect_one_bad_tree <- function(args, status) {
  result <- do.call(merchandise, args)
  expect_identical(result$status$tree_id, 3L)
  expect_identical(result$status$status, as.integer(status))
  expect_setequal(unique(result$logs$tree_id), (1:6)[-3])
  clean <- do.call(merchandise, hardening_trees())
  expected <- clean$logs[clean$logs$tree_id != 3L, ]
  rownames(expected) <- NULL
  expect_identical(result$logs, expected)
}
