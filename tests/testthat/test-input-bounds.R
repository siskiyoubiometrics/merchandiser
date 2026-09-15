test_that("worker counts are bounded before any compiled calculation", {
  original <- threads()
  on.exit(threads(original), add = TRUE)
  cores <- parallel::detectCores()
  if (is.na(cores)) cores <- 1L
  for (n in list(0, -1, 1.5, Inf, NA_real_, "2", numeric())) {
    expect_error(threads(n), "n")
    expect_error(with_threads(n, 1), "n")
  }
  expect_error(with_threads(NULL, 1), "n")
  expect_warning(threads(1e9), "n.*clamped")
  expect_identical(threads(), as.integer(cores))
  expect_identical(dib(16, 60, 20, 202)$status, 0L)
  expect_warning(with_threads(1e9, dib(16, 60, 20, 202)), "n.*clamped")
  expect_warning(value <- .thread_count_from_env("1000000000"), "MERCHANDISER_THREADS")
  expect_identical(value, as.integer(cores))
  expect_identical(.thread_count_from_env("bad"), 1L)
  expect_warning(value <- .thread_count_from_env(strrep("9", 400)), "clamped")
  expect_identical(value, as.integer(cores))
  testthat::local_mocked_bindings(detectCores = function(...) NA_integer_, .package = "parallel")
  expect_warning(threads(8), "clamped to 1")
  expect_identical(threads(), 1L)
  expect_warning(value <- .thread_count_from_env("8"), "clamped to 1")
  expect_identical(value, 1L)
})

test_that("physical bounds keep hostile trees away from stem grids", {
  for (field in c("dbh", "ht")) {
    expected <- if (field == "dbh") 2L else 3L
    for (value in c(1e9, if (field == "dbh") 400.01 else 500.01)) {
      trees <- hardening_trees()
      trees[[field]][3] <- value
      expect_one_bad_tree(trees, expected)
      args <- trees[c("dbh", "ht", "spcd", "model")]
      for (fn in c("dib", "dob", "height_at_dib", "height_at_dob", "stem_volume")) {
        extra <- switch(fn, dib = list(h = 20), dob = list(h = 20),
                        height_at_dib = list(dib = 6), height_at_dob = list(dob = 6), list())
        result <- do.call(fn, c(args, extra))
        expect_identical(result$status, replace(integer(6), 3, expected))
        expect_true(is.na(result$value[3]))
      }
      profile <- do.call(stem_profile, c(list(tree_id = 1:6), args))
      expect_identical(profile$status[profile$tree_id == 3], expected)
      mass <- do.call(biomass, args[c("dbh", "ht", "spcd")])
      expect_identical(mass$status[3], expected)
    }
  }
  expect_false(dib(400, 500, 20, 202, model = "demo.paraboloid")$status %in% c(2L, 3L))
  expect_error(stem_profile(1, 16, 60, 202, step = 1e-6), "step")
  expect_error(stem_profile(1, 16, 60, 202, step = 0.049), "step")
  expect_gt(nrow(stem_profile(1, 16, 60, 202, step = 0.05)), 1)
  fit <- fit_height(example_trees_pnw$dbh, example_trees_pnw$ht,
    example_trees_pnw$spcd, form = "curtis"
  )
  expect_error(predict_height(fit, 16, 202, nsim = 1e9), "nsim")
  expect_error(predict_height(fit, 16, 202, nsim = 1000001), "nsim")
  expect_type(predict_height(fit, 16, 202, nsim = 1e6), "double")
})

test_that("fitting omits impossible dimensions and unknown species", {
  measurements <- example_stem_measurements
  args <- as.list(measurements[c("tree_id", "dbh", "ht", "h", "dib", "spcd")])
  for (name in c("dbh", "ht", "h", "dib", "spcd")) {
    bad <- args
    bad[[name]][3] <- if (name == "spcd") 5 else 1e9
    expect_warning(fit <- do.call(fit_taper, bad), "omitted 1.*(out-of-domain|spcd)")
    clean <- lapply(args, function(x) x[-3])
    expected <- do.call(fit_taper, clean)
    expect_identical(fit$coefficients, expected$coefficients)
    expect_equal(fit$n_measurements, nrow(measurements) - 1L)
  }
  trees <- example_trees_pnw
  for (name in c("dbh", "ht", "spcd")) {
    bad <- c(as.list(trees[c("dbh", "ht", "spcd")]), list(form = "curtis"))
    bad[[name]][3] <- if (name == "spcd") 5 else 1e9
    expect_warning(fit <- do.call(fit_height, bad), "1 rows.*omitted")
    clean <- lapply(as.list(trees[c("dbh", "ht", "spcd")]), function(x) x[-3])
    expected <- do.call(fit_height, c(clean, list(form = "curtis")))
    expect_identical(fit$fixed_effects, expected$fixed_effects)
  }
  args$spcd[] <- 5
  expect_warning(expect_error(do.call(fit_taper, args), "No complete"), "spcd")
  expect_warning(expect_error(fit_height(trees$dbh, trees$ht, 5), "found 0"), "spcd")
})


test_that("fitting combines missing and invalid rows in one omission warning", {
  measurements <- rbind(example_stem_measurements[1:3, ], example_stem_measurements)
  args <- as.list(measurements[c("tree_id", "dbh", "ht", "h", "dib", "spcd")])
  args$dbh[1] <- NA_real_
  args$dib[2] <- 401
  args$spcd[3] <- 5
  result <- capture_warnings(do.call(fit_taper, args))
  expect_length(result$messages, 1L)
  expect_match(result$messages, "omitted 3.*incomplete.*out-of-domain.*spcd")
  args <- as.list(example_trees_pnw[c("dbh", "ht", "spcd")])
  args$dbh[1] <- NA_real_
  args$ht[2] <- 501
  args$spcd[3] <- 5
  result <- capture_warnings(do.call(fit_height, c(args, list(form = "curtis"))))
  expect_length(result$messages, 1L)
  expect_match(result$messages, "3 rows.*missing.*invalid_height.*unknown_species.*omitted")
})
