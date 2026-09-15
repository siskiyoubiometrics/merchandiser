test_that("section type errors name the supplied public bound", {
  for (function_name in c("stem_volume", "stem_profile")) {
    inputs <- list(dbh = 24, ht = 120, spcd = 202, model = "F00FW2W202")
    if (function_name == "stem_profile")
      inputs$tree_id <- 1
    for (bound in c("from", "to", "from_dib", "from_dob", "to_dib", "to_dob")) {
      supplied <- inputs
      supplied[[bound]] <- "invalid"
      expect_error(
        do.call(function_name, supplied),
        paste0("^", bound, " must be numeric\\.$")
      )
    }
  }
})
