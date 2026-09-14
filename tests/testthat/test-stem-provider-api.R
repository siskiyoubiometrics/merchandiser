.load_provider_probe <- function() stem_provider_probe

.provider_expected_weight <- function(volume, spcd, basis, component, moisture) {
  vapply(seq_along(volume), function(index) {
    green_weight(
      volume[index], spcd[index],
      volume_basis = c("inside", "outside")[[basis[index] + 1L]],
      component = c("stem", "wood", "bark")[[component[index] + 1L]],
      moisture = c("green", "dry")[[moisture[index] + 1L]]
    )
  }, numeric(1))
}

test_that("internal provider header matches public R computations", {
  probe <- .load_provider_probe()
  model <- c("demo.paraboloid", "F00FW2W202", "demo.paraboloid.r")
  dbh <- c(12, 16, 10)
  ht <- c(80, 100, 75)
  tree_index <- rep(0:2, each = 3L)
  h <- c(1, 30, 60, 1, 40, 80, 1, 25, 60)
  crossing_height <- c(35, 50, 30)
  crossing_basis <- c(0L, 1L, 0L)
  crossing_target <- c(
    dib(dbh[1], ht[1], crossing_height[1], model[1]),
    dob(dbh[2], ht[2], crossing_height[2], model[2]),
    dib(dbh[3], ht[3], crossing_height[3], model[3])
  )
  weight_volume <- c(10, 2, 3, 4)
  weight_spcd <- c(202L, 131L, 202L, 131L)
  weight_basis <- c(0L, 1L, 0L, 1L)
  weight_component <- c(0L, 1L, 2L, 0L)
  weight_moisture <- c(0L, 1L, 0L, 1L)
  expected_weight <- .provider_expected_weight(
    weight_volume, weight_spcd, weight_basis,
    weight_component, weight_moisture
  )

  results <- lapply(c(1L, 8L), function(threads) {
    old <- threads(threads)
    on.exit(threads(old), add = TRUE)
    probe(
      model, dbh, ht, tree_index, h, crossing_target, crossing_basis,
      rep(1, 3), ht, weight_volume, weight_spcd, weight_basis,
      weight_component, weight_moisture
    )
  })

  for (result in results) {
    expect_identical(
      result$nvel_revision,
      "38548071d5aa652bb90c7f111f86b427f798a1c9"
    )
    expect_identical(result$generation, result$runtime_generation)
    expect_identical(result$resolve_status, c(0L, 0L, 0L))
    expect_identical(result$r_kernel, c(FALSE, FALSE, TRUE))

    metadata <- result$metadata
    expect_identical(metadata$id, model)
    expect_identical(metadata$kernel, c(1L, 1L, 2L))
    expect_identical(metadata$units, c(1L, 1L, 1L))
    expect_identical(metadata$oracle_verified, c(0L, 2L, 0L))
    expect_equal(metadata$stump_height, unname(vapply(model, function(id) {
      get_taper_model(id)$stump_ht
    }, numeric(1))))
    expect_equal(metadata$bark_ratio, unname(vapply(model, function(id) {
      get_taper_model(id)$bark_ratio
    }, numeric(1))))

    query_model <- model[tree_index + 1L]
    query_dbh <- dbh[tree_index + 1L]
    query_ht <- ht[tree_index + 1L]
    compiled <- tree_index < 2L
    expect_equal(
      result$query$dib[compiled],
      dib(
        query_dbh[compiled], query_ht[compiled], h[compiled],
        query_model[compiled], status = TRUE
      )$value,
      tolerance = 1e-10
    )
    expect_equal(
      result$query$dob[compiled],
      dob(
        query_dbh[compiled], query_ht[compiled], h[compiled],
        query_model[compiled], status = TRUE
      )$value,
      tolerance = 1e-10
    )
    expect_equal(
      result$query$cum_ib[compiled],
      stem_volume(
        query_dbh[compiled], query_ht[compiled], query_model[compiled],
        lower = 0, lower_type = "height", upper = h[compiled],
        upper_type = "height", bark = "inside", status = TRUE
      )$value,
      tolerance = 1e-10
    )
    expect_equal(
      result$query$cum_ob[compiled],
      stem_volume(
        query_dbh[compiled], query_ht[compiled], query_model[compiled],
        lower = 0, lower_type = "height", upper = h[compiled],
        upper_type = "height", bark = "outside", status = TRUE
      )$value,
      tolerance = 1e-10
    )
    interval_lower <- c(30, 40)
    interval_upper <- c(60, 80)
    expect_equal(
      c(
        result$query$cum_ib[3] - result$query$cum_ib[2],
        result$query$cum_ib[6] - result$query$cum_ib[5]
      ),
      stem_volume(
        dbh[1:2], ht[1:2], model[1:2],
        lower = interval_lower, lower_type = "height",
        upper = interval_upper, upper_type = "height",
        bark = "inside", status = TRUE
      )$value,
      tolerance = 2e-3
    )
    expect_equal(
      c(
        result$query$cum_ob[3] - result$query$cum_ob[2],
        result$query$cum_ob[6] - result$query$cum_ob[5]
      ),
      stem_volume(
        dbh[1:2], ht[1:2], model[1:2],
        lower = interval_lower, lower_type = "height",
        upper = interval_upper, upper_type = "height",
        bark = "outside", status = TRUE
      )$value,
      tolerance = 1e-10
    )
    expect_identical(result$query$status, c(rep(0L, 6), rep(53L, 3)))
    expect_true(all(is.na(result$query$dib[!compiled])))

    crossings <- result$crossings
    expect_identical(crossings$short_code, 2L)
    expect_identical(crossings$status, c(0L, 0L, 53L))
    expect_true(all(diff(crossings$offsets) >= 0))
    for (tree in 1:2) {
      positions <- seq.int(
        crossings$offsets[tree] + 1,
        crossings$offsets[tree + 1]
      )
      roots <- crossings$roots[positions]
      expect_true(length(roots) >= 1L)
      expected_height <- if (crossing_basis[tree] == 0L) {
        height_at_dib(
          dbh[tree], ht[tree], crossing_target[tree], model[tree],
          status = TRUE
        )$value
      } else {
        height_at_dob(
          dbh[tree], ht[tree], crossing_target[tree], model[tree],
          status = TRUE
        )$value
      }
      expect_equal(tail(roots, 1), expected_height, tolerance = 1e-4)
      evaluated <- if (crossing_basis[tree] == 0L) {
        dib(dbh[tree], ht[tree], roots, model[tree])
      } else {
        dob(dbh[tree], ht[tree], roots, model[tree])
      }
      expect_equal(
        evaluated, rep(crossing_target[tree], length(roots)),
        tolerance = 1e-3
      )
    }
    expect_equal(result$green_weight, expected_weight, tolerance = 1e-12)
    expect_identical(result$green_weight_status, integer(4))
  }

  expect_identical(results[[1]], results[[2]])
})
