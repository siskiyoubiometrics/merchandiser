test_that("fixed legacy units survive construction and combination until the call", {
  for (unit in c("cubic_ft_ib", "cubic_m_ib", "green_short_ton", "green_metric_ton")) {
    p <- suppressMessages(product("log", 1, lengths = 16, min_sed = 4, sold_by = unit))
    matching <- if (unit %in% c("cubic_ft_ib", "green_short_ton")) "imperial" else "metric"
    opposite <- if (matching == "imperial") "metric" else "imperial"
    renamed <- p
    renamed$product <- "renamed"
    for (specification in list(p, validate_products(p), products(p),
                               renamed, validate_products(renamed), products(renamed))) {
      expect_error(merchandise(40, 80, "demo.paraboloid", specification, units = opposite),
                   "scale_unit = .*the product fields cannot express it.*report_also")
      result <- merchandise(40, 80, "demo.paraboloid", specification, units = matching,
                            spcd = 202, status = TRUE)
      expect_gt(nrow(result$logs), 0)
    }
  }
})

test_that("removed procedures and custom rounding name the rejected setting", {
  for (rule in c("smalian", "huber", "scribner_decimal_c_allocated_20",
                 "scribner_factor_whole", "scribner_factor_split_20")) {
    old <- list(product = "log", priority = 1, lengths = 16, min_sed = 4,
                scale_rule = rule, diameter_basis = "ib", scale_bark_basis = "ib",
                measurement_quantity = "board_foot", scale_unit = "board_foot")
    expect_error(suppressMessages(do.call(product, old)),
                 paste0(rule, ": the product fields cannot express it.*report_also"))
  }
  for (field in c("diameter_round", "length_round", "volume_round")) {
    old <- list(product = "log", priority = 1, lengths = 16, min_sed = 4, sold_by = "doyle")
    old[[field]] <- "custom"
    expect_error(suppressMessages(do.call(product, old)),
                 paste0(field, " = custom: the product fields cannot express it.*report_also"))
  }
})

test_that("Black Hills correlation below breast height retains the uncapped exponential", {
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
  # Independently compiled double-precision Fortran SF_CORR, recorded in the fixture provenance.
  expect_equal(compile_correlation(), 0.99966306879629308, tolerance = 1e-15)
})
