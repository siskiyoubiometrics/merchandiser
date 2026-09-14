test_that("volume bound forms resolve to the same interval", {
  lower_h <- 10
  upper_h <- 60
  lower_d <- dib(12, 80, lower_h, "demo.paraboloid")
  upper_d <- dib(12, 80, upper_h, "demo.paraboloid")

  by_height <- stem_volume(
    12, 80, "demo.paraboloid", lower_h, "height", upper_h, "height"
  )
  by_diameter <- stem_volume(
    12, 80, "demo.paraboloid", lower_d, "dib", upper_d, "dib"
  )
  expect_equal(by_diameter, by_height, tolerance = 1e-12)
})

test_that("stump overrides and bark basis are honored", {
  default <- stem_volume(12, 80, "demo.paraboloid")
  override <- stem_volume(12, 80, "demo.paraboloid", stump_ht = 5)
  expect_lt(override, default)

  outside <- stem_volume(12, 80, "demo.paraboloid", bark = "outside")
  expect_equal(outside, default / 0.9^2, tolerance = 1e-13)
})

test_that("stem profiles have exact columns, ordering, and reconciliation", {
  profile <- stem_profile(
    dbh = c(12, 10), ht = c(30, 25), model = "demo.paraboloid",
    step = 24, id = c("b", "a")
  )
  expect_named(profile, c(
    "id", "h", "dib", "dob", "cum_volume_ib", "cum_volume_ob", "status"
  ))
  expect_identical(unique(profile$id), c("a", "b"))
  expect_true(all(profile$status == 0L))

  totals <- tapply(profile$cum_volume_ib, profile$id, max)
  expected <- stem_volume(c(10, 12), c(25, 30), "demo.paraboloid")
  expect_equal(as.vector(totals[c("a", "b")]), expected, tolerance = 1e-13)
})

test_that("profile endpoints are inclusive and cumulative volumes start at zero", {
  profile <- stem_profile(
    12, 20, "demo.paraboloid", step = 36,
    lower = 4.5, lower_type = "height", upper = 17, upper_type = "height"
  )
  expect_equal(profile$h[[1L]], 4.5)
  expect_equal(tail(profile$h, 1L), 17)
  expect_identical(profile$cum_volume_ib[[1L]], 0)
})

test_that("profile failures retain one diagnostic row", {
  profile <- capture_warnings(stem_profile(
    c(0, 12), 20, "demo.paraboloid", step = 120
  ))
  expect_true(any(profile$value$status == 2L))
  expect_match(profile$messages, "dbh_nonpositive")
})

test_that("profile kernel errors do not retain computed values", {
  failed_dib <- function(dbh, ht, h, aux) {
    value <- as.double(dbh * (1 - h / ht))
    if (any(dbh == 13)) attr(value, "status") <- rep(54L, length(value))
    value
  }
  register_test_model("private.profile_failure", dib = failed_dib)
  on.exit(unregister_taper_model("private.profile_failure"), add = TRUE)
  profile <- stem_profile(
    13, 20, "private.profile_failure", step = 120, status = TRUE
  )
  expect_true(all(profile$status == 54L))
  expect_true(all(is.na(profile$dib)))
  expect_true(all(is.na(profile$dob)))
  expect_true(all(is.na(profile$cum_volume_ib)))
  expect_true(all(is.na(profile$cum_volume_ob)))
})
