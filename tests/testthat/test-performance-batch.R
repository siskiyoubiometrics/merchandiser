test_that("merchandising sessions close after success and preparation errors", {
  p <- product("wood", min_length = 8, max_length = 16, min_sed = 0,
               volume_unit = "cubic", price = 1)
  run <- function() {
    merchandise(1:4, c(12, 14, 16, 18), c(40, 45, 50, 55), 202, p,
                model = rep(c("demo.paraboloid", "F00FW2W202"), 2))
  }
  active <- .tv_registry$active
  expect_gt(nrow(run()$logs), 0)
  expect_identical(.tv_registry$active, active)
  query <- .mc_query_points
  calls <- 0L
  local_mocked_bindings(.mc_query_points = function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("preparation probe")
    query(...)
  })
  expect_error(run(), "preparation probe")
  expect_identical(.tv_registry$active, active)
})

test_that("model batches retain column absence and original tree order", {
  model <- get_taper_model("F00FW2W202")
  model$id <- "performance.no.bark.input"
  model$inputs$optional <- character()
  register_taper_model(model)
  on.exit(unregister_taper_model(model$id), add = TRUE)
  p <- products(
    product("inside", min_length = 8, max_length = 16, min_sed = 3,
            volume_unit = "cubic", price = 1),
    product("outside", min_length = 8, max_length = 16, min_sed = 3,
            inside_bark = FALSE, volume_unit = "green_ton", price = 2)
  )
  args <- list(tree_id = 1:6, dbh = c(12, 14, 16, 18, 20, 22), ht = 60,
               spcd = rep(c(202, 131, 202), 2), products = p,
               model = rep(c(model$id, "831CLKE131", "F00FW2W202"), 2))
  open <- .mc_open_provider
  query <- .mc_query_points
  crossings <- .mc_all_crossings
  opened <- queried <- crossed <- character()
  local_mocked_bindings(
    .mc_open_provider = function(model) {
      expect_length(unique(model), 1L)
      opened <<- c(opened, unique(model))
      open(model)
    },
    .mc_query_points = function(session, call, tree, height) {
      queried <<- c(queried, unique(call$model))
      if (call$model[1] == model$id) expect_false("bark_ratio" %in% names(call$aux))
      query(session, call, tree, height)
    },
    .mc_all_crossings = function(session, call, tree, target, basis, lower, upper) {
      crossed <<- c(crossed, unique(call$model))
      crossings(session, call, tree, target, basis, lower, upper)
    }
  )
  result <- do.call(merchandise, args)
  expect_identical(opened, unique(args$model))
  expect_identical(queried, unique(args$model))
  expect_identical(crossed, unique(args$model))
  expect_identical(unique(result$logs$tree_id), args$tree_id)
  rows <- which(args$model == model$id)
  alone <- merchandise(args$tree_id[rows], args$dbh[rows], 60, 202, p, model = model$id)
  actual <- result$logs[result$logs$tree_id %in% args$tree_id[rows], ]
  rownames(actual) <- NULL
  expect_identical(actual, alone$logs)
})

test_that("large cap counts retain distinct optimizer states", {
  for (count in c(3L, 70L)) {
    p <- do.call(products, lapply(seq_len(count), function(i) {
      product(paste0("wood_", i), min_length = 8, max_length = 8, min_sed = 0,
              volume_unit = "cubic", price = 1, max_logs = .Machine$integer.max)
    }))
    capped <- merchandise(1, 16, 17, 202, p, model = "demo.paraboloid", strategy = "optimize")
    p$max_logs <- NA_real_
    uncapped <- merchandise(1, 16, 17, 202, p, model = "demo.paraboloid", strategy = "optimize")
    expect_identical(capped[1:4], uncapped[1:4])
  }
})
