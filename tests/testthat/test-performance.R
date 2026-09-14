allocation_product <- function() {
  .mc_legacy_product(
    "allocation", 1L, lengths = 8, min_sed = 0,
    diameter_basis = "ib", scale_rule = "cubic",
    measurement_quantity = "cubic", scale_unit = "ft3", scale_bark_basis = "ib",
    allow_lower_products = FALSE
  )
}

test_that("allocation event count is constant in repeated-stem count", {
  product <- allocation_product()
  allocation_events <- function(size) {
    gc()
    trace <- profmem::profmem(invisible(merchandise(
      rep(12, size), rep(40, size), rep("demo.paraboloid", size), product,
      id = seq_len(size), utilization_height = 9, status = TRUE
    )))
    sum(!is.na(trace$bytes))
  }
  invisible(allocation_events(64L))
  invisible(allocation_events(64L))
  expect_identical(allocation_events(1000L), allocation_events(10000L))
})

test_that("deduplicated failures report the original stem count", {
  product <- allocation_product()
  messages <- character()
  result <- withCallingHandlers(
    merchandise(
      rep(12, 64), rep(40, 64), rep("missing.model", 64), product,
      id = seq_len(64), utilization_height = 9
    ),
    warning = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(
    messages,
    paste0(
      "merchandise(): unknown_model [50] for 64 stem(s). ",
      "Call status_codes() to see what to change."
    )
  )
  expect_false("status" %in% names(result$trees))
  expect_false("status" %in% names(result$logs))
  expect_null(result$diagnostics)
})
