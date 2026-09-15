for (operation in c("dib", "stem_volume")) {
  test_that(paste("coverage public", operation, "remaps a real NVEL library error"), {
    args <- list(dbh = 12, ht = 80, spcd = 202, model = "200CZ3W202")
    if (operation == "dib") args$h <- 40
    x <- do.call(operation, args)
    # Source: public library-error mapping in R/stem-status.R, status 54.
    expect_identical(x$status, 54L)
    expect_true(is.na(x$value))
  })
}

test_that("coverage biomass converts pounds to tonnes and carbon to carbon dioxide", {
  tree <- example_trees[1, ]
  # Source: exact biomass call in R/stem-nsvb.R:1002-1005, consistency exemption.
  pounds <- merchandiser:::.biomass_impl(
    tree$dbh, tree$ht, tree$spcd, 0, "nsvb", NULL, list(), "imperial", TRUE
  )
  x <- biomass(tree$dbh, tree$ht, tree$spcd, division = 0)
  # Source: the valid shipped tree has positive finite masses and success status.
  expect_identical(x$status, 0L)
  masses <- unlist(x[c("dry_stem_wood", "carbon", "tco2e")])
  expect_true(length(masses) == 3 && all(is.finite(masses) & masses > 0))
  # Sources: international avoirdupois pound = 0.45359237 kg, tonne = 1000 kg.
  expect_equal(x$dry_stem_wood, pounds$dry_stem_wood * 0.45359237 / 1000, tolerance = 1e-12)
  # Source: stoichiometric molecular mass ratio CO2 / C = 44 / 12.
  expect_equal(x$tco2e, x$carbon * 44 / 12, tolerance = 1e-12)
})

test_that("coverage biomass division zero selects national coefficients", {
  tree <- example_trees[1, ]
  national <- biomass(tree$dbh, tree$ht, tree$spcd, division = 0)
  expect_identical(national$status, 0L)
  expect_identical(biomass(tree$dbh, tree$ht, tree$spcd), national)
  # Source: shipped NSVB division fixture contains division 240, Marine.
  fixture <- utils::read.csv(testthat::test_path("fixtures", "nsvb_division_points.csv"))
  expect_true(240 %in% fixture$division)
  regional <- biomass(tree$dbh, tree$ht, tree$spcd, division = 240)
  expect_identical(regional$status, 0L)
  wood <- c(national$dry_stem_wood, regional$dry_stem_wood)
  expect_true(length(wood) == 2 && all(is.finite(wood) & wood > 0))
  expect_false(isTRUE(all.equal(national$dry_stem_wood, regional$dry_stem_wood)))
})

test_that("coverage Flewelling below-breast-height correlation matches compiled Fortran", {
  compile_correlation <- function() {
    env <- new.env(parent = globalenv())
    include <- system.file("include", package = "merchandiser", mustWork = TRUE)
    previous <- Sys.getenv("PKG_CPPFLAGS", unset = NA_character_)
    on.exit({
      if (is.na(previous)) Sys.unsetenv("PKG_CPPFLAGS") else Sys.setenv(PKG_CPPFLAGS = previous)
    })
    Sys.setenv(PKG_CPPFLAGS = paste0("-I", shQuote(include)))
    Rcpp::cppFunction(
      code = "double black_hills_correlation() {
        treevolume::flewelling::Profile profile{};
        profile.ht = 80;
        profile.equation.jsp = 22;
        return treevolume::flewelling::conditioning_correlation(profile, 4, 4.001);
      }",
      includes = "#include <treevolume/flewelling.hpp>", env = env
    )
    env$black_hills_correlation()
  }
  # Source: commit e70e5eb, test-simplify-migration.R, lines 36 through 58.
  # Independently compiled double-precision Fortran SF_CORR regression value.
  expect_equal(compile_correlation(), 0.99966306879629308, tolerance = 1e-15)
})
