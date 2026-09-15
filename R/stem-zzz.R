.demo_r_dib <- function(dbh, ht, h, aux) {
  bark_ratio <- if (!is.null(aux$bark_ratio))
    aux$bark_ratio else rep(0.9, length(dbh))
  as.double(dbh * bark_ratio * sqrt((ht - h) / (ht - 4.5)))
}

.thread_count_from_env <- function(value) {
  if (!grepl("^[1-9][0-9]*$", value)) {
    return(1L)
  }
  parsed <- suppressWarnings(as.double(value))
  if (is.infinite(parsed))
    parsed <- .Machine$double.xmax
  .validate_thread_count(parsed, "MERCHANDISER_THREADS")
}

.demo_models <- function() {
  common_inputs <- list(required = character(), optional = "bark_ratio", pairs = list())
  compiled <- new_stem_model_unchecked(
    id = "demo.paraboloid", form = "demo", kernel = list(
      type = "compiled",
      key = "demo_paraboloid", has_dob = FALSE, has_inverse = TRUE, has_integral = TRUE
    ), inputs = common_inputs,
    measurement_system = "imperial", spcd = integer(), stump_ht = 1, bark_ratio = 0.9,
    source = "demonstration kernel",
    notes = "Quadratic paraboloid with a constant bark ratio."
  )
  r_model <- new_stem_model_unchecked(
    id = "demo.paraboloid.r", form = "demo", kernel = list(
      type = "r",
      key = NA_character_, has_dob = FALSE, has_inverse = FALSE, has_integral = FALSE
    ), dib = .demo_r_dib,
    inputs = common_inputs, measurement_system = "imperial", spcd = integer(), stump_ht = 1,
    bark_ratio = 0.9,
    source = "demonstration kernel",
    notes = "R-kernel copy used to exercise numerical inversion and integration."
  )
  list(compiled, r_model)
}

.onLoad <- function(libname, pkgname) {
  value <- Sys.getenv("MERCHANDISER_THREADS", unset = Sys.getenv(
    "TREEVOLUME_THREADS",
    unset = ""
  ))
  .tv_runtime$threads <- .thread_count_from_env(value)
  .tv_registry$patterns <- rbind(
    .flewelling_patterns(), .clark_patterns(), .r10r4_patterns(),
    .smalltaper_patterns(), .nsvb_patterns()
  )
  for (model in c(
    .demo_models(), .flewelling_models(), .clark_models(), .r10r4_models(),
    .smalltaper_models(),
    .nsvb_models()
  )) {
    .register_package_model(model)
  }
}
