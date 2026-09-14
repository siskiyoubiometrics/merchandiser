test_that("package models are present and sealed", {
  expect_true(has_taper_model("demo.paraboloid"))
  expect_true(has_taper_model("demo.paraboloid.r"))
  expect_false(has_taper_model(NA_character_))
  expect_false(has_taper_model(""))
  expect_false(has_taper_model("missing.model"))
  expect_error(unregister_taper_model("demo.paraboloid"), "cannot be unregistered")
  expect_error(register_taper_model(get_taper_model("demo.paraboloid")), "already")
})

test_that("package demonstration model definitions are reproducible", {
  models <- merchandiser:::.demo_models()
  expect_length(models, 2L)
  expect_identical(vapply(models, `[[`, character(1), "id"), c(
    "demo.paraboloid", "demo.paraboloid.r"
  ))
  expect_silent(lapply(models, validate_taper_model))
})

test_that("private registration requires a namespace and explicit removal", {
  model <- new_taper_model("private.registry", "user", linear_dib)
  expect_identical(register_taper_model(model), model)
  on.exit(unregister_taper_model("private.registry"), add = TRUE)

  expect_identical(get_taper_model("private.registry"), model)
  expect_error(register_taper_model(model), "already")
  expect_error(get_taper_model("missing.model"), "unknown")
  expect_error(register_taper_model(new_taper_model("unqualified", "user", linear_dib)),
    "namespaced"
  )
})

test_that("registry listing and capabilities have stable shapes", {
  models <- taper_models(family = "demo")
  expect_named(models, c(
    "id", "family", "kernel", "species_scope", "has_dob", "has_inverse",
    "has_integral", "oracle_verified", "units", "stump_ht", "owner_package",
    "source", "generation"
  ))
  expect_identical(models$id, sort(models$id))
  species_models <- taper_models(species = 999L)
  expect_true(all(models$id %in% species_models$id))
  expect_true(all(
    species_models$species_scope == "unrestricted" |
      species_models$species_scope == "999"
  ))
  empty <- taper_models(family = "missing")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, names(models))
  expect_error(taper_models(species = 1e20), "integer-valued")

  all_models <- taper_models()
  unverified <- all_models$id[all_models$oracle_verified %in% FALSE]
  expect_setequal(unverified, merchandiser:::.nvel_unverified_ids)

  unverified_r10 <- setdiff(unverified, "I15FW2W017")
  first <- dib(12, 80, 30, unverified_r10, status = TRUE)
  second <- dib(12, 80, 30, unverified_r10, status = TRUE)
  expect_identical(second, first)
  expect_identical(first$status, integer(length(unverified_r10)))
  expect_true(all(is.finite(first$value)))

  capabilities <- model_capabilities(c("demo.paraboloid", "missing.model"))
  expect_named(capabilities, c("id", "has_dob", "has_inverse", "has_integral"))
  expect_true(capabilities$has_inverse[[1L]])
  expect_true(is.na(capabilities$has_inverse[[2L]]))
})

test_that("registry manifests round trip and report changes", {
  manifest <- taper_manifest()
  expect_equal(nrow(check_taper_manifest(manifest)), 0L)

  altered <- manifest
  altered$generation[[1L]] <- altered$generation[[1L]] + 1L
  expect_identical(check_taper_manifest(altered)$problem, "changed")

  without_first <- manifest[-1L, , drop = FALSE]
  expect_identical(check_taper_manifest(without_first)$problem, "added")
  extra <- rbind(
    manifest,
    data.frame(
      id = "private.absent", owner_package = "test", package_version = NA_character_,
      generation = 1L, stringsAsFactors = FALSE
    )
  )
  expect_identical(tail(check_taper_manifest(extra)$problem, 1L), "missing")
  expect_error(check_taper_manifest(manifest[, -1L]), "manifest")
})

test_that("check_taper_models finds mapping problems", {
  inputs <- list(
    required = "site_index",
    optional = c("upper_ht1", "upper_d1"),
    pairs = list(c("upper_ht1", "upper_d1"))
  )
  register_test_model(
    "private.mapping", inputs = inputs, species = 122L
  )
  on.exit(unregister_taper_model("private.mapping"), add = TRUE)

  clean <- check_taper_models(
    "private.mapping", spcd = 122L,
    aux = list(site_index = 80, upper_ht1 = 20, upper_d1 = 8)
  )
  expect_equal(nrow(clean), 0L)

  bad <- check_taper_models(
    c("private.mapping", "private.mapping", "missing.model"),
    spcd = c(999L, 122L, 122L),
    aux = list(upper_ht1 = c(20, 20, 20))
  )
  expect_true(all(c(50L, 51L, 52L) %in% bad$code))
  expect_error(check_taper_models(letters[1:2], spcd = 1:3), "size-one")

  missing_values <- check_taper_models(
    c("private.mapping", "private.mapping", NA_character_),
    spcd = c(Inf, 1e20, 122),
    aux = list(site_index = c(80, Inf, 80))
  )
  expect_identical(missing_values$code, c(1L, 7L, 1L, 1L))
  expect_identical(
    missing_values$input,
    c("spcd", "spcd", "site_index", "id")
  )
  expect_error(
    check_taper_models("private.mapping", aux = list(site_index = list(80))),
    "atomic vectors"
  )
  expect_error(
    check_taper_models("private.mapping", aux = list(id = 1)),
    "reserved auxiliary"
  )
  expect_error(
    check_taper_models("private.mapping", aux = list(site_index = 0)),
    "site_index has 1 out-of-domain value"
  )
  expect_equal(nrow(check_taper_models(character())), 0L)
  expect_equal(
    nrow(check_taper_models(factor("demo.paraboloid"))),
    0L
  )
})

test_that("registry query input branches have stable results", {
  expect_error(has_taper_model(1), "character")
  expect_error(model_capabilities(1), "character")
  expect_equal(nrow(model_capabilities(character())), 0L)
})

test_that("the registry cannot change from inside a callback", {
  intruder <- new_taper_model("private.intruder", "user", linear_dib)
  mutating <- function(dbh, ht, h, aux) {
    if (any(dbh == 13)) register_taper_model(intruder)
    as.double(dbh * (1 - h / ht))
  }
  register_test_model("private.mutating", dib = mutating)
  on.exit(unregister_taper_model("private.mutating"), add = TRUE)

  result <- dib(13, 80, 20, "private.mutating", status = TRUE)
  expect_identical(result$status, 54L)
  expect_false(has_taper_model("private.intruder"))
})
