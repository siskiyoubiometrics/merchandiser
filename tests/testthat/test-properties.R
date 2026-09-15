test_that("log positions, trim, and stem coverage are invariant properties", {
  for (trim in c(0, 0.5)) {
    for (inside in c(TRUE, FALSE)) {
      p <- products(product("saw",
                      min_length = 16, max_length = 32, trim = trim,
                      min_sed = 8,
                      max_sed = 30, min_led = 8, max_led = 40, inside_bark = inside,
                      volume_unit = "cubic"
                    ), product("pulp",
                      min_length = 8, max_length = 16,
                      min_sed = 3,
                      inside_bark = inside, volume_unit = "cubic"
                    ))
      defects <- rbind(defect(1, 10, 20, "cull"), defect(1, 60, 80, "restrict",
        product = "pulp"
      ))
      x <- merchandise(1, 24, 120, 202, p,
        model = "F00FW2W202", defects = defects,
        quiet = TRUE
      )
      logs <- x$logs
      spec <- p[match(logs$product, p$product), ]
      expect_gt(nrow(logs), 0)
      expect_true(all(diff(logs$start_height) >= 0))
      expect_true(all(head(logs$end_height, -1) <= tail(
        logs$start_height,
        -1
      )))
      expect_equal(logs$end_height, logs$start_height + logs$length + spec$trim)
      expect_true(all(logs$length >= spec$min_length & logs$length <= spec$max_length))
      expect_true(all(logs$sed >= spec$min_sed))
      expect_true(all(logs$led >= spec$min_led))
      expect_true(all(is.na(spec$max_sed) | logs$sed <= spec$max_sed))
      expect_true(all(is.na(spec$max_led) | logs$led <= spec$max_led))
      trims <- x$residuals[x$residuals$cause == "trim", ]
      trimmed <- spec$trim > 0
      expect_equal(trims$start_height, logs$start_height[trimmed] + logs$length[trimmed])
      expect_equal(trims$end_height, logs$end_height[trimmed])
      expect_true(all(!is.na(x$residuals$cause) & nzchar(x$residuals$cause)))
      pieces <- rbind(data.frame(start_height = logs$start_height, end_height = logs$start_height +
                                   logs$length), x$residuals[c("start_height", "end_height")])
      pieces <- pieces[order(pieces$start_height), ]
      expect_identical(pieces$start_height[1], 0)
      expect_identical(tail(pieces$end_height, 1), 120)
      expect_true(all(pieces$end_height > pieces$start_height))
      expect_equal(head(pieces$end_height, -1), tail(pieces$start_height, -1))
    }
  }
})

test_that("thread counts one and eight return identical results", {
  trees <- example_trees
  p <- product("saw",
    min_length = 16, max_length = 32, min_sed = 6, trim = 0.5,
    volume_unit = "scribner"
  )
  run <- function(n) {
    with_threads(n, merchandise(trees$tree_id, trees$dbh, trees$ht, trees$spcd,
      p,
      model = trees$model, quiet = TRUE
    ))
  }
  expect_identical(run(1), run(8))
})

test_that("only size one call vectors recycle", {
  p <- product("saw", min_length = 16, max_length = 32, min_sed = 4, volume_unit = "cubic")
  recycled <- merchandise(1:2, c(12, 14), 80, 202, p, quiet = TRUE)
  expanded <- merchandise(1:2, c(12, 14), c(80, 80), c(202, 202), p, quiet = TRUE)
  expect_identical(recycled, expanded)
  expect_error(merchandise(1:2, c(12, 14), c(40, 45, 50), 202, p), "size-one")
  expect_error(merchandise(c(1, 1), 12, 40, 202, p), "unique")
  expect_error(stem_volume(c(12, 14), c(40, 45, 50), 202), "size-one")
})

test_that("zero length and single tree calls are stable", {
  p <- product("saw", min_length = 16, max_length = 32, min_sed = 4, volume_unit = "cubic")
  empty <- merchandise(numeric(), numeric(), numeric(), numeric(), p, quiet = TRUE)
  expect_s3_class(empty, "merch_result")
  for (table in empty[1:4]) expect_identical(nrow(table), 0L)
  expect_output(print(empty), "merch_result")
  single <- merchandise(1, 12, 40, 202, p, quiet = TRUE)
  expect_true(all(single$logs$tree_id == 1))
  expect_identical(nrow(single$status), 0L)
  expect_output(print(single), "merch_result")
  expect_identical(nrow(stem_volume(numeric(), numeric(), numeric())), 0L)
  expect_identical(nrow(stem_volume(12, 40, 202)), 1L)
})

test_that("size stable helpers work inside dplyr mutate", {
  trees <- example_trees
  volume <- dplyr::mutate(trees, stem_volume(dbh, ht, spcd, model = model))
  expect_identical(volume$tree_id, trees$tree_id)
  expect_identical(volume$value, stem_volume(trees$dbh, trees$ht, trees$spcd,
                     model = trees$model
                   )$value)
  percent <- dplyr::mutate(trees, defect_by_thirds(dbh, ht, spcd,
    lower = 10,
    middle = 20,
    upper = 30, model = model
  ))
  expect_identical(nrow(percent), nrow(trees))
})

test_that("green weight and cord conversions execute", {
  cubic <- product("log",
    min_length = 16, max_length = 16, min_sed = 0, max_logs = 1,
    volume_unit = "cubic"
  )
  run <- function(p) merchandise(1, 24, 120, 202, p, model = "F00FW2W202", quiet = TRUE)
  body <- run(cubic)$logs$scale
  weight <- cubic
  weight$volume_unit <- "green_ton"
  actual <- run(weight)
  expect_true(all(is.finite(actual$logs$scale)))
  expect_equal(actual$logs$scale, green_weight(body, 202)$value)
  cord <- cubic
  cord$volume_unit <- "cord"
  cord$cord_solid_fraction <- 0.75
  actual <- run(cord)
  expect_equal(actual$logs$scale, body / (128 * cord$cord_solid_fraction))
  expect_identical(actual$logs$volume_unit, "cord")
})
