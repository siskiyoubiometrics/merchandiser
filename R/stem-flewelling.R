.flewelling_source <- paste(
  "US Forest Service National Volume Estimator Library,",
  "NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.flewelling_model <- function(id, family, species) {
  three_point <- identical(family, "flewelling_3pt")
  inputs <- if (three_point) {
    list(required = c("upper_ht1", "upper_d1"), optional = c(
      "upper_ht2", "upper_d2", "upper_bark",
      "bark_ratio"
    ), pairs = list(c("upper_ht1", "upper_d1"), c("upper_ht2", "upper_d2")))
  } else {
    list(required = character(), optional = "bark_ratio", pairs = list())
  }
  new_stem_model_unchecked(
    id = id, form = family, kernel = list(type = "compiled", key = paste0(
      "flewelling:",
      id
    ), has_dob = TRUE, has_inverse = FALSE, has_integral = TRUE), inputs = inputs,
    measurement_system = "imperial",
    spcd = as.integer(species), stump_ht = 1, bark_ratio = NA_real_,
    source = .flewelling_source,
    oracle_verified = .nvel_oracle_verified(id), notes = if (three_point) {
      paste(
        "Conditioned by one or two upper-stem measurements.",
        "The two-point Flewelling form is recommended."
      )
    } else {
      "Two-point variable-shape stem profile."
    }
  )
}

.flewelling_models <- function() {
  path <- system.file("extdata", "flewelling_models.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!"I15FW2W017" %in% metadata$id) {
    metadata <- rbind(metadata, data.frame(
      species = 17L, id = "I15FW2W017", family = "flewelling_2pt",
      source_file = "not generated",
      upstream_commit = "38548071d5aa652bb90c7f111f86b427f798a1c9"
    ))
  }
  Map(.flewelling_model, metadata$id, metadata$family, metadata$species)
}

.flewelling_patterns <- function() {
  data.frame(id = c("???FW2????", "???FW3????", "???F32????", "???F33????"), family = c(
    "flewelling_2pt",
    rep("flewelling_3pt", 3L)
  ), stringsAsFactors = FALSE)
}

.flewelling_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl(
    "^.{3}(FW2|FW3|F32|F33)W[0-9]{3}$",
    id
  )) {
    return(NULL)
  }
  form <- substr(id, 4L, 6L)
  family <- if (identical(form, "FW2")) {
    "flewelling_2pt"
  } else {
    "flewelling_3pt"
  }
  .flewelling_model(id, family, as.integer(substr(id, 8L, 10L)))
}

NULL
