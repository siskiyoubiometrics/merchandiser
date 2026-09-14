test_that("measurement combinations survive validation and combination", {
  mapping <- .mc_measurement_map()
  expect_false(anyDuplicated(.mc_measurement_key(mapping)) > 0)
  for (i in seq_len(nrow(mapping))) {
    row <- mapping[i, ]
    fields <- c("volume_unit", "inside_bark", "split_scale", "round")
    args <- c(list(product = "test", priority = 1, lengths = 16, min_sed = 4),
              as.list(row[fields]))
    if (row$volume_unit == "cord") args$cord_solid_fraction <- 0.7
    p <- do.call(product, args)
    for (candidate in list(p, validate_products(p), products(p))) {
      for (field in fields) expect_identical(candidate[[field]], row[[field]])
      expanded <- .validate_products_units(candidate, "imperial")
      expect_identical(expanded$scale_rule, row$scale_rule)
      expect_identical(expanded$diameter_round, row$diameter_round)
    }
  }
})
test_that("diameter preprocessing precedes the unchanged board foot procedures", {
  for (rule in c("scribner", "international", "doyle")) {
    for (units in c("imperial", "metric")) {
      factor <- if (units == "imperial") 1 else 2.54
      length_factor <- if (units == "imperial") 1 else 0.3048
      p <- product("test", 1, lengths = 16 * length_factor, min_sed = 0,
                   volume_unit = rule, round = "down")
      q <- p
      q$round <- "default"
      run <- function(specifications) {
        merchandise(20 * factor, 100 * length_factor, "F00FW2W202", specifications,
                    spcd = 202, utilization_height = 17 * length_factor,
                    units = units, status = TRUE)
      }
      down <- run(p)
      defaults <- run(q)
      expect_identical(down$logs$end_height, defaults$logs$end_height)
      nominal <- dib(20 * factor, 100 * length_factor,
                     down$logs$nominal_end_height, "F00FW2W202", units = units) / factor
      nominal_length <- down$logs$nominal_length / length_factor
      expected <- switch(rule,
        scribner = .mc_scribner(floor(nominal), nominal_length, TRUE),
        international = .mc_intl14(floor(nominal), nominal_length),
        doyle = max(floor(nominal) - 4, 0)^2 * nominal_length / 16
      )
      expect_equal(down$logs$gross_scale, expected)
    }
  }
})
test_that("the opening example includes every stated product", {
  tree <- example_trees[5, ]
  p <- example_products("douglas_fir")
  result <- merchandise(tree$dbh, tree$ht, tree$model, p, spcd = tree$spcd,
                        id = tree$tree, currency = "USD", status = TRUE)
  expect_setequal(result$logs$product, p$product)
  expect_setequal(result$logs$volume_unit, c("scribner", "green_ton"))
  expect_identical(result$logs$gross_scale, result$logs$net_scale)
})
