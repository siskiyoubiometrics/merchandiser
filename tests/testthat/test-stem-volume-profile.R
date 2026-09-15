test_that("volume bound forms resolve to the same interval", {
  lower_h <- 10
  upper_h <- 60
  lower_d <- dib(
    dbh = 12, ht = 80, h = lower_h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value
  upper_d <- dib(
    dbh = 12, ht = 80, h = upper_h, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value

  by_height <- stem_volume(
    dbh = 12,
    ht = 80,
    model = "demo.paraboloid",
    from = lower_h,
    to = upper_h,
    spcd = .interface_spcd("demo.paraboloid")
  )$value
  by_diameter <- stem_volume(
    dbh = 12,
    ht = 80,
    model = "demo.paraboloid",
    from_dib = lower_d,
    to_dib = upper_d,
    spcd = .interface_spcd(
      "demo.paraboloid"
    )
  )$value
  expect_equal(by_diameter, by_height, tolerance = 1e-12)
})

test_that("stump overrides and bark basis are honored", {
  default <- stem_volume(
    dbh = 12, ht = 80, model = "demo.paraboloid",
    spcd = .interface_spcd("demo.paraboloid")
  )$value
  override <- stem_volume(
    dbh = 12, ht = 80, model = "demo.paraboloid",
    stump_ht = 5, spcd = .interface_spcd("demo.paraboloid")
  )$value
  expect_lt(override, default)

  outside <- stem_volume(
    dbh = 12, ht = 80, model = "demo.paraboloid", spcd = .interface_spcd("demo.paraboloid"),
    inside_bark = FALSE
  )$value
  expect_equal(outside, default / 0.9^2, tolerance = 1e-13)
})

test_that("stem profiles have exact columns, ordering, and reconciliation", {
  profile <- stem_profile(
    dbh = c(12, 10),
    ht = c(30, 25),
    model = "demo.paraboloid",
    step = 2,
    spcd = .interface_spcd("demo.paraboloid"),
    tree_id = c("b", "a")
  )
  expect_named(profile, c(
    "tree_id", "h", "dib", "dob", "cum_volume_ib",
    "cum_volume_ob", "status"
  ))
  expect_identical(unique(profile$tree_id), c("b", "a"))
  expect_true(all(profile$status == 0L))

  totals <- tapply(profile$cum_volume_ib, profile$tree_id, max)
  expected <- stem_volume(
    dbh = c(10, 12), ht = c(25, 30),
    model = "demo.paraboloid", spcd = .interface_spcd("demo.paraboloid")
  )$value
  expect_equal(as.vector(totals[c("a", "b")]), expected, tolerance = 1e-13)
})

test_that("profile endpoints are inclusive and cumulative volumes start at zero", {
  profile <- stem_profile(
    dbh = 12,
    ht = 20,
    model = "demo.paraboloid",
    step = 3,
    from = 4.5,
    to = 17,
    spcd = .interface_spcd("demo.paraboloid"),
    tree_id = seq_along(rep_len(12, max(length(12), length(20))))
  )
  expect_equal(profile$h[[1L]], 4.5)
  expect_equal(tail(profile$h, 1L), 17)
  expect_identical(profile$cum_volume_ib[[1L]], 0)
})

test_that("profile failures retain one diagnostic row", {
  profile <- capture_warnings(stem_profile(
    dbh = c(0, 12),
    ht = 20,
    model = "demo.paraboloid",
    step = 10,
    spcd = .interface_spcd("demo.paraboloid"),
    tree_id = seq_along(rep_len(c(
      0,
      12
    ), max(length(c(0, 12)), length(20))))
  ))
  expect_true(any(profile$value$status == 2L))
  expect_length(profile$messages, 0L)
})

test_that("profile kernel errors do not retain computed values", {
  failed_dib <- function(dbh, ht, h, aux) {
    value <- as.double(dbh * (1 - h / ht))
    if (any(dbh == 13))
      attr(value, "status") <- rep(54L, length(value))
    value
  }
  register_test_model("private.profile_failure", dib = failed_dib)
  on.exit(unregister_taper_model("private.profile_failure"), add = TRUE)
  profile <- stem_profile(
    dbh = 13,
    ht = 20,
    model = "private.profile_failure",
    step = 10,
    spcd = .interface_spcd("private.profile_failure"),
    tree_id = seq_along(rep_len(13, max(
      length(13),
      length(20)
    )))
  )
  expect_true(all(profile$status == 54L))
  expect_true(all(is.na(profile$dib)))
  expect_true(all(is.na(profile$dob)))
  expect_true(all(is.na(profile$cum_volume_ib)))
  expect_true(all(is.na(profile$cum_volume_ob)))
})
