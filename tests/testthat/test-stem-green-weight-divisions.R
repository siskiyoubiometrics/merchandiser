test_that("green weight follows the REF_SPECIES constituent formulas", {
  wood_green <- 28.08 * (1 + 35.33 / 100)
  bark_green <- 27.46 * (1 + 89.39 / 100)
  ratio <- 17.3 / 100

  expect_equal(
    (green_weight(volume = 1, spcd = 202, component = "wood")$value *
       2000), wood_green,
    tolerance = 1e-14
  )
  expect_equal((green_weight(
    volume = 1, spcd = 202, component = "wood",
    moisture = "dry"
  )$value *
    2000), 28.08, tolerance = 0)
  expect_equal((green_weight(volume = 1, spcd = 202, component = "bark")$value *
                  2000), ratio *
                 bark_green, tolerance = 1e-14)
  expect_equal((green_weight(volume = 1, spcd = 202)$value * 2000), wood_green +
      ratio * bark_green,
    tolerance = 1e-14
  )

  outside <- (green_weight(
    volume = 1 + ratio, spcd = 202, inside_bark = FALSE,
    component = "stem"
  )$value *
    2000)
  expect_equal(outside, wood_green + ratio * bark_green, tolerance = 1e-14)
  expect_equal((green_weight(
    volume = 1 + ratio, spcd = 202,
    inside_bark = FALSE, component = "wood"
  )$value *
    2000), wood_green, tolerance = 1e-14)
  expect_equal((green_weight(
    volume = 1, spcd = 202, component = c("stem", "bark"),
    moisture = "dry"
  )$value *
    2000), c(28.08 + ratio * 27.46, ratio * 27.46), tolerance = 1e-14)
})

test_that("green weight applies explicit physical-property overrides", {
  wood_green <- 0.5 * 62.4 * (1 + 100 / 100)
  bark_green <- 0.3 * 62.4 * (1 + 50 / 100)
  ratio <- 20 / 100

  expect_equal((green_weight(
    volume = 1, spcd = 131, component = "wood", specific_gravity = 0.5,
    moisture_pct = 100
  )$value * 2000), wood_green, tolerance = 0)
  expect_equal((green_weight(
    volume = 1, spcd = 131, component = "bark", bark_specific_gravity = 0.3,
    bark_moisture_pct = 50, bark_volume_pct = 20
  )$value * 2000), ratio * bark_green, tolerance = 1e-14)
  expect_equal(
    (green_weight(
      volume = 1, spcd = 131, specific_gravity = 0.5, moisture_pct = 100,
      bark_specific_gravity = 0.3, bark_moisture_pct = 50, bark_volume_pct = 20
    )$value * 2000),
    wood_green + ratio * bark_green,
    tolerance = 1e-14
  )
  expect_equal((green_weight(
    volume = 1 + ratio, spcd = 131, inside_bark = FALSE, specific_gravity = 0.5,
    moisture_pct = 100, bark_specific_gravity = 0.3, bark_moisture_pct = 50,
    bark_volume_pct = 20
  )$value *
    2000), wood_green + ratio * bark_green, tolerance = 1e-14)
  expect_equal((green_weight(
    volume = 1, spcd = 131, component = "wood", moisture = "dry",
    specific_gravity = 0.5, moisture_pct = 300
  )$value * 2000), 0.5 * 62.4, tolerance = 0)
  expect_equal((green_weight(volume = 1, spcd = 131, bark_volume_pct = 0)$value *
                  2000), (green_weight(
                 volume = 1,
                 spcd = 131, component = "wood"
               )$value * 2000), tolerance = 0)
  expect_equal(
    (green_weight(
      volume = 1, spcd = 5155, specific_gravity = 0.5, moisture_pct = 100,
      bark_specific_gravity = 0.3, bark_moisture_pct = 50, bark_volume_pct = 20
    )$value * 2000),
    wood_green + ratio * bark_green,
    tolerance = 1e-14
  )
})

test_that("green weight override vectors follow common-size recycling", {
  expected <- c(0.4 * 62.4 * 2, 2 * 0.5 * 62.4 * 2)
  expect_equal((green_weight(
    volume = c(1, 2), spcd = 131, component = "wood",
    specific_gravity = c(
      0.4,
      0.5
    ), moisture_pct = 100
  )$value * 2000), expected, tolerance = 0)
  expect_equal((green_weight(
    volume = 1, spcd = c(131, 131), component = "wood",
    specific_gravity = c(
      0.4,
      0.5
    ), moisture_pct = 100
  )$value * 2000), c(0.4, 0.5) * 62.4 * 2, tolerance = 0)
  expect_error((green_weight(
    volume = 1:2, spcd = 131, specific_gravity = 0.4,
    moisture_pct = c(
      80,
      90, 100
    )
  )$value * 2000), "size-one")
})

test_that("green weight rejects invalid overrides", {
  expect_error(
    (green_weight(volume = 1, spcd = 131, specific_gravity = "0.47")$value * 2000),
    "numeric"
  )
  expect_error((green_weight(volume = 1, spcd = 131, specific_gravity = 0)$value *
                  2000), "specific_gravity")
  expect_error((green_weight(volume = 1, spcd = 131, bark_specific_gravity = -0.1)$value *
                  2000), "bark_specific_gravity")
  expect_error((green_weight(volume = 1, spcd = 131, moisture_pct = -1)$value *
                  2000), "moisture_pct")
  expect_error((green_weight(volume = 1, spcd = 131, moisture_pct = 301)$value *
                  2000), "moisture_pct")
  expect_error(
    (green_weight(volume = 1, spcd = 131, bark_moisture_pct = 301)$value * 2000),
    "bark_moisture_pct"
  )
  expect_error((green_weight(volume = 1, spcd = 131, bark_volume_pct = -1)$value *
                  2000), "bark_volume_pct")
  expect_error(
    (green_weight(volume = 1, spcd = 131, bark_volume_pct = 101)$value * 2000),
    "bark_volume_pct"
  )
  expect_error((green_weight(volume = c(1, 1), spcd = 131, specific_gravity = c(0, -
                                                                                  0.1))$value *
                  2000), "2 out-of-domain values")

  missing <- green_weight(volume = 1, spcd = 131, specific_gravity = NA_real_)
  expect_identical(missing$status, 1L)
  expect_true(is.na(missing$value))
})

test_that("documented loblolly values stay synchronized with the function", {
  combinations <- data.frame(
    basis = rep(c("inside", "outside"), each = 2L),
    component = rep(c(
      "wood",
      "stem"
    ), 2L)
  )
  actual <- (green_weight(
    volume = 1, spcd = 131, inside_bark = combinations$basis == "inside",
    component = combinations$component
  )$value *
    2000)
  expected <- c(53, 59.8, 45.5, 51.3)
  expect_equal(round(actual, 1), expected, tolerance = 0)
  expect_equal(round((green_weight(volume = 1, spcd = 131)$value * 2000), 1), 59.8,
    tolerance = 0
  )

  moisture <- (green_weight(volume = 1, spcd = 131, component = "wood", moisture = c(
    "green",
    "dry"
  ))$value * 2000)
  expect_equal(moisture, c((green_weight(
    volume = 1, spcd = 131, component = "wood",
    moisture = "green"
  )$value *
    2000), (green_weight(
    volume = 1, spcd = 131, component = "wood",
    moisture = "dry"
  )$value *
    2000)), tolerance = 0)
})

test_that("green weight is size stable and converts at the metric boundary", {
  imperial <- (green_weight(volume = c(1, 2), spcd = 202)$value * 2000)
  expect_length((green_weight(volume = 1, spcd = c(202, 263, 300))$value * 2000), 3L)
  expect_identical((green_weight(volume = numeric(), spcd = numeric())$value *
                      2000), numeric())
  expect_equal(nrow(green_weight(volume = numeric(), spcd = numeric())), 0L)
  expect_error((green_weight(volume = 1:2, spcd = 202:204)$value * 2000), "size-one")
  expect_error(
    (green_weight(volume = 1:2, spcd = 202, inside_bark = rep(TRUE, 3L))$value * 2000),
    "size-one"
  )
  expect_error(
    (green_weight(volume = 1, spcd = 202, component = "foliage")$value * 2000),
    "component"
  )
  expect_error(green_weight(volume = 1, spcd = 202, inside_bark = "inside"), "logical")
  expect_equal(
    (green_weight(volume = 1, spcd = 202, inside_bark = c(TRUE, FALSE), component = c(
      "s",
      "w"
    ), moisture = c("g", "d"))$value * 2000), (green_weight(
      volume = 1, spcd = 202,
      inside_bark = c(TRUE, FALSE), component = c("stem", "wood"), moisture = c("green", "dry")
    )$value * 2000),
    tolerance = 0
  )
  expect_equal(
    (green_weight(
      volume = 1, spcd = 202, inside_bark = NULL, component = NULL,
      moisture = NULL
    )$value * 2000), (green_weight(volume = 1, spcd = 202)$value * 2000),
    tolerance = 0
  )
  expect_silent(missing_choices <- green_weight(
    volume = rep(1, 3L), spcd = 202,
    inside_bark = c(NA, TRUE, TRUE), component = c("stem", NA, "stem"), moisture = c(
      "green", "green",
      NA
    )
  ))
  expect_identical(missing_choices$status, rep(1L, 3L))
  expect_true(all(is.na(missing_choices$value)))
  expect_error((green_weight(volume = -1, spcd = 202)$value * 2000), "nonnegative")
  expect_error(
    (green_weight(volume = 1, spcd = 202, region = 6)$value * 2000),
    "unused argument"
  )
})

test_that("green weight diagnoses species and missing reference values", {
  result <- green_weight(volume = c(1, 1, NA), spcd = c(202, 10000, 202))
  expect_identical(result$status, c(0L, 7L, 1L))
  expect_true(is.na(result$value[[2L]]))
  expect_true(is.na(result$value[[3L]]))

  expect_silent(missing <- (green_weight(volume = 1, spcd = 5155)$value * 2000))
  expect_true(is.na(missing))
  expect_identical(green_weight(volume = 1, spcd = 5155)$status, 1L)
  expect_silent((green_weight(volume = 1, spcd = 10000)$value * 2000))

  expect_equal((green_weight(volume = 1, spcd = 715, component = "wood")$value *
                  2000), (green_weight(
                 volume = 1,
                 spcd = 715, component = "wood", moisture = "dry"
               )$value * 2000), tolerance = 0)
})

test_that("regional NVEL weight factors remain internally accessible", {
  expect_equal(
    merchandiser:::nvel_weight_factor(c(202, 202),
      region = 6,
      forest = c(16, 0)
    ),
    c(61, 60),
    tolerance = 0
  )
  expect_equal(merchandiser:::nvel_weight_factor(c(25, 204), region = 6, forest = c(6, 16)),
    c(63, 61),
    tolerance = 0
  )
  expect_equal(merchandiser:::nvel_weight_factor(202, region = 1, product = "08"),
    55.77,
    tolerance = 0
  )
  expect_equal(merchandiser:::nvel_weight_factor(202), 47, tolerance = 0)
  expect_identical(merchandiser:::nvel_weight_factor(10000, status = TRUE)$status, 7L)
  expect_lt(
    merchandiser:::nvel_weight_factor(202, region = 1, live = FALSE),
    merchandiser:::nvel_weight_factor(202,
      region = 1
    )
  )
  expect_error(merchandiser:::nvel_weight_factor(202, product = "1"), "two-digit")
  expect_error(merchandiser:::nvel_weight_factor(202, live = 1), "logical")
})

test_that("division points from the source fixture resolve exactly", {
  points <- utils::read.csv(test_path("fixtures", "nsvb_division_points.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(nsvb_division_xy(x = points$x, y = points$y)$value, as.integer(
    points$division
  ))
  expect_true(all(points$method %in% c("centroid", "point_on_surface")))
})

test_that("division lookup handles outside, missing, and vector inputs", {
  result <- nsvb_division_xy(x = c(-122.25012, 0, NA), y = c(45.72507, 0, 40))
  expect_identical(result$value, c(1240L, NA_integer_, NA_integer_))
  expect_identical(result$status, c(0L, 8L, 1L))
  expect_silent(nsvb_division_xy(x = 0, y = 0)$value)
  expect_identical(nsvb_division_xy(x = numeric(), y = numeric())$value, integer())
  expect_error(nsvb_division_xy(x = 1:2, y = 1:3)$value, "size-one")
  expect_error(nsvb_division_xy(x = 0, y = 0, crs = NA)$value, "one coordinate")
  expect_error(nsvb_division_xy(
    x = numeric(), y = numeric(),
    crs = "not-a-crs"
  )$value, "valid coordinate")
})

test_that("compiled division lookup applies holes and boundary rules", {
  polygon_x <- c(0, 3, 3, 0, 0, 1, 2, 2, 1, 1, 3, 4, 4, 3, 3)
  polygon_y <- c(0, 0, 3, 3, 0, 1, 1, 2, 2, 1, 0, 0, 3, 3, 0)
  actual <- merchandiser:::tv_cpp_nsvb_division_xy_impl(
    c(0.5, 1.5, 1, 3, 3.5), c(
      0.5, 1.5,
      1.5, 1.5, 1.5
    ), c(rep(10L, 10), rep(20L, 5)), c(rep(1L, 5), rep(2L, 5), rep(3L, 5)),
    c(rep(FALSE, 5), rep(TRUE, 5), rep(FALSE, 5)), polygon_x, polygon_y, 2L
  )
  expect_identical(actual, c(10L, NA_integer_, 10L, 10L, 20L))
})

test_that("compiled division lookup validates its table contract", {
  square_x <- c(0, 1, 1, 0, 0)
  square_y <- c(0, 0, 1, 1, 0)
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, c(0, 1), rep(10L, 5), rep(
    1L,
    5
  ), rep(FALSE, 5), square_x, square_y, 1L), "coordinate inputs")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, rep(10L, 4), rep(
    1L, 5
  ), rep(
    FALSE,
    5
  ), square_x, square_y, 1L), "polygon columns")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, rep(10L, 5), rep(
    1L, 5
  ), rep(
    FALSE,
    5
  ), square_x, square_y, 0L), "threads")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, rep(NA_integer_, 5), rep(
    1L,
    5
  ), rep(FALSE, 5), square_x, square_y, 1L), "identifiers")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, rep(10L, 5), rep(
    1L, 5
  ), rep(
    FALSE,
    5
  ), c(NA, square_x[-1]), square_y, 1L), "coordinates")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, c(10L, rep(20L, 4)), rep(
    1L,
    5
  ), rep(FALSE, 5), square_x, square_y, 1L), "metadata")
  expect_error(merchandiser:::tv_cpp_nsvb_division_xy_impl(0, 0, rep(10L, 3), rep(
    1L, 3
  ), rep(
    FALSE,
    3
  ), square_x[1:3], square_y[1:3], 1L), "four vertices")
  expect_true(is.na(merchandiser:::tv_cpp_nsvb_division_xy_impl(NA_real_, 0, rep(
    10L, 5
  ), rep(
    1L,
    5
  ), rep(FALSE, 5), square_x, square_y, 1L)))
})

test_that("division lookup is thread invariant and transforms through sf", {
  points <- utils::read.csv(test_path("fixtures", "nsvb_division_points.csv"),
    stringsAsFactors = FALSE
  )
  x <- rep(points$x, 20)
  y <- rep(points$y, 20)
  one <- with_threads(1, nsvb_division_xy(x = x, y = y)$value)
  eight <- with_threads(8, nsvb_division_xy(x = x, y = y)$value)
  expect_identical(one, eight)
  projected <- sf::st_transform(sf::st_as_sf(points,
                                  coords = c("x", "y"),
                                  crs = 4326
                                ), 5070)
  coordinates <- sf::st_coordinates(projected)
  expect_identical(
    nsvb_division_xy(x = coordinates[, "X"], y = coordinates[, "Y"], crs = 5070)$value,
    as.integer(points$division)
  )
})

test_that("division package data retain regeneration evidence", {
  polygons <- merchandiser:::nsvb_division_polygons
  expect_s3_class(polygons, "data.frame")
  expect_identical(names(polygons), c("division", "ring_id", "hole", "x", "y"))
  expect_equal(nrow(polygons), 55823L)
  expect_equal(attr(polygons, "simplification_tolerance_m"), 500)
  expect_lte(attr(polygons, "maximum_area_difference_pct"), 0.5)

  provenance <- utils::read.csv(system.file("extdata",
    "nsvb_division_provenance.csv",
    package = "merchandiser"
  ))
  expect_identical(provenance$source_feature_count, 36L)
  expect_identical(provenance$division_count, 19L)
  expect_identical(provenance$simplification_tolerance_m, 500L)
  expect_lte(provenance$maximum_area_difference_pct, 0.5)
  expect_lt(provenance$compressed_data_bytes, 2e+06)
})
