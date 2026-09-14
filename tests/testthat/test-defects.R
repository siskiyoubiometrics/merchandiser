test_that("defects receive row-order-independent canonical ids", {
  products <- acceptance_products_a()
  x <- rbind(
    defect(1L, 20, 30, "rot", percent = 15),
    defect(1L, 5, 10, "exclude")
  )
  first <- validate_defects(x, products, 80, 1L)
  second <- validate_defects(x[2:1, ], products, 80, 1L)
  expect_identical(first, second)
  expect_identical(validate_defects(first, products, 80, 1L), first)
  expect_identical(first$defect_id, c("auto:000001", "auto:000002"))
  expect_error(validate_defects(rbind(x[1, ], x[1, ]), products, 80, 1L),
               "Exact duplicate")
})

test_that("keyed defect values receive reserved statuses", {
  products <- acceptance_products_a()
  x <- rbind(
    defect(1:4, as.double(c(-1, 4, 4, 4)),
           as.double(c(2, 4, 8, 8)),
           c("cull", "cull", "rot", "mystery"),
           percent = as.double(c(NA, NA, 101, NA)))
  )
  result <- validate_defects(x, products, rep(80, 4), 1:4)
  expect_identical(result$status, c(401L, 402L, 403L, 406L))
})

test_that("point effects retain canonical point form", {
  products <- acceptance_products_a()
  x <- rbind(
    defect(1L, 30, NA_real_, "fork"),
    defect(1L, 60, 60, "break")
  )
  result <- validate_defects(x, products, 80, 1L)
  expect_equal(result$to, c(30, 60))
  expect_identical(result$effect, c("fork", "break"))
  expect_identical(validate_defects(result, products, 80, 1L), result)
})

test_that("stopper translation enforces order and alternatives", {
  result <- defects_from_stoppers(
    1L, 75, saw_stop = 40, pulp_stop = 60, jump_butt = 4, stump_ht = 1
  )
  expect_identical(result$defect_id,
                   c(".stopper.jump_butt", ".stopper.pulp", ".stopper.cull"))
  expect_error(defects_from_stoppers(
    1L, 75, saw_stop = 40, always_pulp = TRUE
  ), "mutually exclusive")
  expect_error(defects_from_stoppers(
    1L, 75, saw_stop = 50, pulp_stop = 40
  ), "strictly ordered")
  expect_equal(nrow(defects_from_stoppers(1L, 75, pulp_stop = 75)), 0L)
})

test_that("third percentages use each third's physical volume", {
  answer <- defect_pct_from_thirds(
    c(12, 12), 80, "demo.paraboloid", 10, c(20, 10), 30,
    status = TRUE
  )
  expect_identical(answer$status, c(0L, 0L))
  expect_true(answer$value[1] > 10 && answer$value[1] < 30)
  bounds <- c(0, 80 / 3, 160 / 3, 80)
  volumes <- vapply(seq_len(3L), function(i) {
    merchandiser::stem_volume(
      12, 80, "demo.paraboloid", lower = bounds[i], lower_type = "height",
      upper = bounds[i + 1L], upper_type = "height", bark = "inside"
    )
  }, numeric(1L))
  expect_equal(answer$value[2], sum(volumes * c(10, 10, 30)) / sum(volumes))
})

test_that("overlap precedence restricts product and weights rot by volume", {
  register_acceptance_model()
  on.exit(unregister_acceptance_model(), add = TRUE)
  products <- acceptance_products_a()
  defects <- rbind(
    defect(1L, 42, 50, "rot", percent = 10),
    defect(1L, 46, 55, "rot", percent = 30),
    defect(1L, 42, 74, "pulp")
  )
  result <- merchandise(
    12, 80, acceptance_model_id, products, id = 1L, spcd = 202,
    defects = defects, stump_ht = 1, status = TRUE
  )
  expect_identical(result$logs$product, c("domestic_saw", "pulp"))
  expect_gt(result$logs$located_deduction_cubic_ib[2], 0)
  expect_equal(
    result$logs$log_located_net_cubic_ib,
    result$logs$log_gross_cubic_ib - result$logs$located_deduction_cubic_ib
  )
})
