compiled_key_plan <- function(dbh, ht, x1, x2, bark_ratio) {
  size <- length(dbh)
  merchandiser:::.tv_cpp_duplicate_map_impl(
    dbh, ht, x1, x2, bark_ratio, rep(NA_real_, size),
    rep(NA_real_, size), rep(NA_real_, size), rep(NA_real_, size), integer(size), rep(
      NA_real_,
      size
    ), rep(NA_real_, size), rep(NA_real_, size)
  )
}

test_that("compiled outside-bark operations use the C++ ABI", {
  flewelling <- merchandiser:::tv_cpp_kernel_eval(
    "flewelling:F00FW2W202", 4L, 12, 80, 20,
    0, 0, 1L
  )
  expect_identical(flewelling$status, 0L)
  expect_identical(flewelling$value, dob(
    dbh = 12, ht = 80, h = 20,
    model = "F00FW2W202", spcd = .interface_spcd("F00FW2W202")
  )$value)

  clark <- merchandiser:::tv_cpp_kernel_eval("clark:900CLKE621", 4L, 12, 80, 20, 0, 0.9, 1L)
  expect_identical(clark$status, 0L)
  expect_equal(clark$value, dib(
    dbh = 12, ht = 80, h = 20, model = "900CLKE621",
    spcd = .interface_spcd("900CLKE621")
  )$value / 0.9,
  tolerance = 0
  )
  missing_bark <- merchandiser:::tv_cpp_kernel_eval(
    "clark:900CLKE621", 4L, 12, 80, 20, 0,
    NA_real_, 1L
  )
  expect_identical(missing_bark$status, 53L)

  nsvb <- merchandiser:::tv_cpp_kernel_eval("nsvb:NVBM240202", 4L, 12, 80, 20, 0, 0, 1L)
  expect_identical(nsvb$status, 0L)
  expect_identical(nsvb$value, dob(
    dbh = 12, ht = 80, h = 20, model = "NVBM240202",
    spcd = .interface_spcd("NVBM240202")
  )$value)
  expect_gt(nsvb$value, dib(
    dbh = 12, ht = 80, h = 20, model = "NVBM240202",
    spcd = .interface_spcd("NVBM240202")
  )$value)
})

test_that("every compiled path is exactly thread invariant", {
  count <- 32L
  index <- seq_len(count)
  dbh <- 10 + index / 20
  ht <- 70 + index / 5
  h <- 10 + index
  model <- rep(c("F00FW2W202", "900CLKE621"), length.out = count)
  bark_ratio <- rep(0.9, count)
  target_dib <- with_threads(1, dib(
    dbh = dbh, ht = ht, h = h, model = model, bark_ratio = bark_ratio,
    spcd = .interface_spcd(model)
  )$value)
  target_dob <- with_threads(1, dob(
    dbh = dbh, ht = ht, h = h, model = model, bark_ratio = bark_ratio,
    spcd = .interface_spcd(model)
  )$value)

  taper_paths <- function(threads) {
    with_threads(threads, list(
      dib = dib(
        dbh = dbh, ht = ht, h = h, model = model, bark_ratio = bark_ratio,
        spcd = .interface_spcd(model)
      )$value, dob = dob(
        dbh = dbh, ht = ht, h = h, model = model,
        bark_ratio = bark_ratio, spcd = .interface_spcd(model)
      )$value, height_at_dib = height_at_dib(
        dbh = dbh,
        ht = ht, dib = target_dib, model = model, bark_ratio = bark_ratio,
        spcd = .interface_spcd(model)
      ),
      height_at_dob = height_at_dob(
        dbh = dbh, ht = ht, dob = target_dob, model = model,
        bark_ratio = bark_ratio, spcd = .interface_spcd(model)
      ), stem_volume = stem_volume(
        dbh = dbh,
        ht = ht, model = model, bark_ratio = bark_ratio, spcd = .interface_spcd(model),
        inside_bark = TRUE
      ), stem_profile = stem_profile(
        dbh = dbh,
        ht = ht,
        model = model,
        step = 50,
        bark_ratio = bark_ratio,
        spcd = .interface_spcd(model),
        tree_id = index
      )
    ))
  }
  expect_identical(taper_paths(1), taper_paths(8))

  spcd <- rep(c(110, 202, 263, 621), length.out = count)
  division <- rep(c(1240, 1330), length.out = count)
  biomass_paths <- function(threads) {
    with_threads(threads, biomass(dbh, ht, spcd, division))
  }
  expect_identical(biomass_paths(1), biomass_paths(8))
})

test_that("regional, small-taper, and NSVB workers are thread invariant", {
  cases <- list(
    list(id = "A16DEMW098", aux = list(bark_ratio = 0.9)), list(
      id = "400MATW202",
      aux = list(bark_ratio = 0.9)
    ), list(id = "B00BEHW011", aux = list(form_class = 80, bark_ratio = 0.9)),
    list(id = "NVBM240202", aux = list())
  )
  paths <- function(case, threads) {
    with_threads(threads, {
      common <- c(list(dbh = 12, ht = 80, spcd = .interface_spcd(case$id),
                       model = case$id), case$aux)
      inside <- do.call(dib, c(common, list(h = 30)))
      outside <- do.call(dob, c(common, list(h = 30)))
      list(
        dib = inside, dob = outside,
        height_at_dib = do.call(height_at_dib, c(common, list(dib = inside$value))),
        height_at_dob = do.call(height_at_dob, c(common, list(dob = outside$value))),
        volume_ib = do.call(stem_volume, common),
        volume_ob = do.call(stem_volume, c(common, list(inside_bark = FALSE))),
        profile = do.call(stem_profile, c(list(tree_id = 1), common, list(step = 50)))
      )
    })
  }

  for (case in cases) {
    expect_identical(paths(case, 1), paths(case, 8), info = case$id)
  }
})

test_that("NSVB biomass deduplicates and scatters complete rows", {
  key <- rep(1:2, 32)
  unique_result <- biomass(c(12, 14), c(80, 90), c(202, 263), c(1240, 1330))
  repeated_result <- biomass(c(12, 14)[key], c(80, 90)[key], c(202, 263)[key], c(
    1240, 1330
  )[key]
  )
  expected <- unique_result[key, , drop = FALSE]
  rownames(expected) <- NULL

  expect_identical(repeated_result, expected)
})

test_that("compiled profiles preserve supplied upper-bark conditioning", {
  profile <- stem_profile(
    dbh = 12,
    ht = 80,
    model = "F00FW3W202",
    step = 20,
    upper_ht1 = 30,
    upper_d1 = 9,
    upper_bark = "ib",
    spcd = .interface_spcd("F00FW3W202"),
    tree_id = seq_along(rep_len(
      12,
      max(length(12), length(80))
    ))
  )

  expect_identical(profile$dib, dib(
    dbh = 12, ht = 80, h = profile$h, model = "F00FW3W202",
    upper_ht1 = 30, upper_d1 = 9, upper_bark = "ib", spcd = .interface_spcd("F00FW3W202")
  )$value)
  expect_identical(profile$dob, dob(
    dbh = 12, ht = 80, h = profile$h, model = "F00FW3W202",
    upper_ht1 = 30, upper_d1 = 9, upper_bark = "ib", spcd = .interface_spcd("F00FW3W202")
  )$value)
})

test_that("compiled volume and profile allocation counts do not grow by tree", {

  volume_call <- function(size) {
    index <- seq_len(size)
    stem_volume(dbh = 12 + index %% 3, ht = 80 + index %% 5, model = rep(
      c(
        "F00FW2W202", "900CLKE621"
      ),
      length.out = size
    ), spcd = .interface_spcd(rep(c("F00FW2W202", "900CLKE621"), length.out = size)))$value
  }
  profile_call <- function(size) {
    index <- seq_len(size)
    stem_profile(
      dbh = 12 + index %% 3,
      ht = 10,
      model = rep("demo.paraboloid", size),
      step = 10,
      spcd = .interface_spcd(rep("demo.paraboloid", size)),
      tree_id = seq_along(rep_len(
        12 +
          index %% 3,
        max(
          length(
            12 +
              index %% 3
          ),
          length(
            10
          )
        )
      ))
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

  plan <- compiled_key_plan(repeated_dbh, repeated_ht, rep(1, count), rep(80, count), rep(
    0,
    count
  ))
  expect_length(plan$unique, 10L)

  expected <- stem_volume(
    dbh = 10:19, ht = 70:79, model = "F00FW2W202",
    spcd = .interface_spcd("F00FW2W202")
  )$value
  actual <- stem_volume(
    dbh = repeated_dbh, ht = repeated_ht, model = model,
    spcd = .interface_spcd(model)
  )$value
  expect_identical(actual, rep(expected, length.out = count))
})

test_that("the scaled benchmark records timings without a budget", {
  count <- 100000L
  index <- seq_len(count)
  dbh <- 8 + (index %% 221) / 10
  ht <- 50 + (index %% 901) / 10
  model <- rep(c("F00FW2W202", "900CLKE621"), length.out = count)
  h <- ht / 2
  for (threads in c(1L, 4L, 8L)) {
    volume <- with_threads(threads, bench::mark(stem_volume(
      dbh = dbh, ht = ht, model = model,
      spcd = .interface_spcd(model)
    )$value, iterations = 1, check = FALSE, filter_gc = FALSE))
    message(
      "MERCHANDISER_BENCH stem_volume threads=", threads, " median=", format(volume$median),
      " mem_alloc=", format(volume$mem_alloc), " n_gc=", volume$n_gc
    )
  }
  diameter <- with_threads(8, bench::mark(
    dib(
      dbh = dbh, ht = ht, h = h,
      model = model, spcd = .interface_spcd(model)
    )$value,
    iterations = 1, check = FALSE, filter_gc = FALSE
  ))
  mass <- with_threads(8, bench::mark(biomass(dbh, ht, rep(202, count), rep(1240, count)),
    iterations = 1, check = FALSE, filter_gc = FALSE
  ))
  message(
    "MERCHANDISER_BENCH dib threads=8 median=", format(diameter$median), " mem_alloc=",
    format(diameter$mem_alloc), " n_gc=", diameter$n_gc
  )
  message(
    "MERCHANDISER_BENCH biomass threads=8 median=", format(mass$median), " mem_alloc=",
    format(mass$mem_alloc), " n_gc=", mass$n_gc
  )
  succeed()
})
