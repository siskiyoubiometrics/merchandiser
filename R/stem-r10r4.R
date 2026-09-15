.r10r4_source <- paste(
  "US Forest Service National Volume Estimator Library,",
  "NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.r10r4_model <- function(id, family, species) {
  r10 <- identical(family, "r10_taper")
  species_scope <- as.integer(species)
  if (r10 && length(species_scope) && !is.na(species_scope) && species_scope == 0L) {
    species_scope <- 98L
  } else if (!length(species_scope) || is.na(species_scope) || species_scope == 0L) {
    species_scope <- integer()
  }
  new_stem_model_unchecked(
    id = id, form = family, kernel = list(type = "compiled", key = paste0(
      if (r10) "r10:" else "r4mat:",
      id
    ), has_dob = FALSE, has_inverse = TRUE, has_integral = TRUE), inputs = list(
      required = character(),
      optional = "bark_ratio", pairs = list()
    ), measurement_system = "imperial", spcd = species_scope,
    stump_ht = 1, bark_ratio = NA_real_, source = .r10r4_source,
    oracle_verified = .nvel_oracle_verified(id),
    notes = if (r10) {
      paste(
        "Region 10 R10TAP profile with R10TC, FSTGRO, and SECGRO total",
        "cubic volume. The library call path has no outside-bark model.",
        "dob() requires an explicit constant bark_ratio approximation."
      )
    } else {
      paste(
        "Region 4 Mathis profile and CF0 total cubic volume.",
        "The library call path has no outside-bark model. dob() requires an",
        "explicit constant bark_ratio approximation."
      )
    }
  )
}

.r10r4_models <- function() {
  path <- system.file("extdata", "r10r4_models.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing_ids <- setdiff(.nvel_unverified_ids[-1L], metadata$id)
  if (length(missing_ids)) {
    metadata <- rbind(metadata, data.frame(
      id = missing_ids, family = "r10_taper", species = as.integer(substr(
        missing_ids,
        8L, 10L
      )), source_file = "not generated",
      upstream_commit = "38548071d5aa652bb90c7f111f86b427f798a1c9",
      oracle_tested = FALSE
    ))
  }
  Map(.r10r4_model, metadata$id, metadata$family, metadata$species)
}

.r10r4_patterns <- function() {
  data.frame(id = c("???DEM????", "???CUR????", "???BRU????", "???MAT????"), family = c(rep(
    "r10_taper",
    3L
  ), "r4_driver"), stringsAsFactors = FALSE)
}

.r10r4_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    return(NULL)
  }
  if (grepl("^.{3}(DEM|CUR|BRU).[0-9]{3}$", id)) {
    family <- "r10_taper"
  } else if (grepl("^.{3}MAT.[0-9]{3}$", id)) {
    family <- "r4_driver"
  } else {
    return(NULL)
  }
  .r10r4_model(id, family, as.integer(substr(id, 8L, 10L)))
}

NULL
