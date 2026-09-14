test_that("the package loads inside a callr subprocess", {
  package_path <- find.package("merchandiser")
  result <- callr::r(function(package_path) {
    if (file.exists(file.path(package_path, "R", "stem-zzz.R"))) {
      pkgload::load_all(package_path, quiet = TRUE)
    } else {
      library(merchandiser)
    }
    "ok"
  }, args = list(package_path = package_path))

  expect_identical(result, "ok")
})
