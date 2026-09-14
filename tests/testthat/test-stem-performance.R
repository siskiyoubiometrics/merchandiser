compiled_key_plan <- function(dbh, ht, x1, x2, bark_ratio) {
  size <- length(dbh)
  merchandiser:::.tv_cpp_duplicate_map_impl(
    dbh, ht, x1, x2, bark_ratio,
    rep(NA_real_, size), rep(NA_real_, size),
    rep(NA_real_, size), rep(NA_real_, size), integer(size),
    rep(NA_real_, size), rep(NA_real_, size), rep(NA_real_, size)
  )
}

test_that("compiled outside-bark operations use the C++ ABI", {
  flewelling <- merchandiser:::tv_cpp_kernel_eval(
    "flewelling:F00FW2W202", 4L, 12, 80, 20, 0, 0, 1L
  )
  expect_identical(flewelling$status, 0L)
  expect_identical(
    flewelling$value,
    dob(12, 80, 20, "F00FW2W202")
  )

  clark <- merchandiser:::tv_cpp_kernel_eval(
    "clark:900CLKE621", 4L, 12, 80, 20, 0, 0.9, 1L
  )
  expect_identical(clark$status, 0L)
  expect_equal(
    clark$value,
    dib(12, 80, 20, "900CLKE621") / 0.9,
    tolerance = 0
  )
  missing_bark <- merchandiser:::tv_cpp_kernel_eval(
    "clark:900CLKE621", 4L, 12, 80, 20, 0, NA_real_, 1L
  )
  expect_identical(missing_bark$status, 53L)

  nsvb <- merchandiser:::tv_cpp_kernel_eval(
    "nsvb:NVBM240202", 4L, 12, 80, 20, 0, 0, 1L
  )
  expect_identical(nsvb$status, 0L)
  expect_identical(nsvb$value, dob(12, 80, 20, "NVBM240202"))
  expect_gt(nsvb$value, dib(12, 80, 20, "NVBM240202"))
})

test_that("every compiled path is exactly thread invariant", {
  count <- 32L
  index <- seq_len(count)
  dbh <- 10 + index / 20
  ht <- 70 + index / 5
  h <- 10 + index
  model <- rep(c("F00FW2W202", "900CLKE621"), length.out = count)
  bark_ratio <- rep(0.9, count)
  target_dib <- with_threads(1, dib(dbh, ht, h, model, bark_ratio = bark_ratio))
  target_dob <- with_threads(1, dob(dbh, ht, h, model, bark_ratio = bark_ratio))

  taper_paths <- function(threads) {
    with_threads(threads, list(
      dib = dib(dbh, ht, h, model, bark_ratio = bark_ratio),
      dob = dob(dbh, ht, h, model, bark_ratio = bark_ratio),
      height_at_dib = height_at_dib(
        dbh, ht, target_dib, model, bark_ratio = bark_ratio, status = TRUE
      ),
      height_at_dob = height_at_dob(
        dbh, ht, target_dob, model, bark_ratio = bark_ratio, status = TRUE
      ),
      stem_volume = stem_volume(
        dbh, ht, model, bark_ratio = bark_ratio, status = TRUE
      ),
      stem_profile = stem_profile(
        dbh, ht, model, step = 600, id = index,
        bark_ratio = bark_ratio, status = TRUE
      )
    ))
  }
  expect_identical(taper_paths(1), taper_paths(8))

  spcd <- rep(c(110, 202, 263, 621), length.out = count)
  division <- rep(c(1240, 1330), length.out = count)
  biomass_paths <- function(threads) {
    with_threads(threads, biomass(dbh, ht, spcd, division, status = TRUE))
  }
  expect_identical(biomass_paths(1), biomass_paths(8))
})

test_that("regional, small-taper, and NSVB workers are thread invariant", {
  cases <- list(
    list(id = "A16DEMW098", aux = list(bark_ratio = 0.9)),
    list(id = "400MATW202", aux = list(bark_ratio = 0.9)),
    list(
      id = "B00BEHW011", aux = list(form_class = 80, bark_ratio = 0.9)
    ),
    list(id = "NVBM240202", aux = list())
  )
  paths <- function(case, threads) {
    with_threads(threads, {
      inside <- do.call(dib, c(list(12, 80, 30, case$id), case$aux))
      outside <- do.call(dob, c(list(12, 80, 30, case$id), case$aux))
      list(
        dib = inside,
        dob = outside,
        height_at_dib = do.call(
          height_at_dib,
          c(list(12, 80, inside, case$id, status = TRUE), case$aux)
        ),
        height_at_dob = do.call(
          height_at_dob,
          c(list(12, 80, outside, case$id, status = TRUE), case$aux)
        ),
        volume_ib = do.call(
          stem_volume, c(list(12, 80, case$id, status = TRUE), case$aux)
        ),
        volume_ob = do.call(
          stem_volume,
          c(list(12, 80, case$id, bark = "outside", status = TRUE), case$aux)
        ),
        profile = do.call(
          stem_profile,
          c(list(12, 80, case$id, step = 600, status = TRUE), case$aux)
        )
      )
    })
  }

  for (case in cases) {
    expect_identical(paths(case, 1), paths(case, 8), info = case$id)
  }
})

test_that("NSVB biomass deduplicates and scatters complete rows", {
  key <- rep(1:2, 32)
  unique_result <- biomass(
    c(12, 14), c(80, 90), c(202, 263), c(1240, 1330), status = TRUE
  )
  repeated_result <- biomass(
    c(12, 14)[key], c(80, 90)[key], c(202, 263)[key],
    c(1240, 1330)[key], status = TRUE
  )
  expected <- unique_result[key, , drop = FALSE]
  rownames(expected) <- NULL

  expect_identical(repeated_result, expected)
})

test_that("compiled profiles preserve supplied upper-bark conditioning", {
  profile <- stem_profile(
    12, 80, "F00FW3W202", step = 240,
    upper_ht1 = 30, upper_d1 = 9, upper_bark = "ib", status = TRUE
  )

  expect_identical(
    profile$dib,
    dib(
      12, 80, profile$h, "F00FW3W202",
      upper_ht1 = 30, upper_d1 = 9, upper_bark = "ib"
    )
  )
  expect_identical(
    profile$dob,
    dob(
      12, 80, profile$h, "F00FW3W202",
      upper_ht1 = 30, upper_d1 = 9, upper_bark = "ib"
    )
  )
})

test_that("compiled volume and profile allocation counts do not grow by tree", {
  skip_if_not_installed("profmem")

  volume_call <- function(size) {
    index <- seq_len(size)
    stem_volume(
      12 + index %% 3, 80 + index %% 5,
      rep(c("F00FW2W202", "900CLKE621"), length.out = size)
    )
  }
  profile_call <- function(size) {
    index <- seq_len(size)
    stem_profile(
      12 + index %% 3, 10,
      rep("demo.paraboloid", size), step = 120
    )
  }
  allocation_count <- function(code) {
    profile <- profmem::profmem(force(code))
    sum(is.finite(profile$bytes))
  }

  invisible(profmem::profmem(volume_call(100L)))
  volume_small <- allocation_count(volume_call(1000L))
  volume_large <- allocation_count(volume_call(10000L))
  profile_small <- allocation_count(profile_call(1000L))
  profile_large <- allocation_count(profile_call(10000L))

  expect_lte(volume_large, volume_small + 25L)
  expect_lte(profile_large, profile_small + 25L)
})

test_that("stand-table keys compute once and scatter", {
  count <- 30000L
  repeated_dbh <- rep(10:19, length.out = count)
  repeated_ht <- rep(70:79, length.out = count)
  model <- rep("F00FW2W202", count)

  plan <- compiled_key_plan(
    repeated_dbh, repeated_ht, rep(1, count), rep(80, count), rep(0, count)
  )
  expect_length(plan$unique, 10L)

  expected <- stem_volume(10:19, 70:79, "F00FW2W202")
  actual <- stem_volume(repeated_dbh, repeated_ht, model)
  expect_identical(actual, rep(expected, length.out = count))
})

test_that("the guarded scaled benchmark records timings without a budget", {
  benchmark <- Sys.getenv("MERCHANDISER_BENCH",
                          unset = Sys.getenv("TREEVOLUME_BENCH", unset = ""))
  if (benchmark != "true") skip("Set MERCHANDISER_BENCH=true to run benchmarks")
  skip_if_not_installed("bench")

  count <- 100000L
  index <- seq_len(count)
  dbh <- 8 + (index %% 221) / 10
  ht <- 50 + (index %% 901) / 10
  model <- rep(c("F00FW2W202", "900CLKE621"), length.out = count)
  h <- ht / 2
  for (threads in c(1L, 4L, 8L)) {
    volume <- with_threads(threads, bench::mark(
      stem_volume(dbh, ht, model), iterations = 1,
      check = FALSE, filter_gc = FALSE
    ))
    message(
      "MERCHANDISER_BENCH stem_volume threads=", threads,
      " median=", format(volume$median),
      " mem_alloc=", format(volume$mem_alloc), " n_gc=", volume$n_gc
    )
  }
  diameter <- with_threads(8, bench::mark(
    dib(dbh, ht, h, model), iterations = 1,
    check = FALSE, filter_gc = FALSE
  ))
  mass <- with_threads(8, bench::mark(
    biomass(dbh, ht, rep(202, count), rep(1240, count)),
    iterations = 1, check = FALSE, filter_gc = FALSE
  ))
  message(
    "MERCHANDISER_BENCH dib threads=8 median=", format(diameter$median),
    " mem_alloc=", format(diameter$mem_alloc), " n_gc=", diameter$n_gc
  )
  message(
    "MERCHANDISER_BENCH biomass threads=8 median=", format(mass$median),
    " mem_alloc=", format(mass$mem_alloc), " n_gc=", mass$n_gc
  )
  succeed()
})
