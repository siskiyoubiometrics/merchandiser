test_that("the public status catalogue has stable surviving codes", {
  table <- status_codes()
  expect_false(anyDuplicated(table$status) > 0)
  expect_identical(table$status, c(0L, 1:8, 50:55, 100:102, 400L:405L, 407:412))
  expect_true(all(nzchar(table$name)))
  expect_true(all(endsWith(table$description, ".")))
})
