.smalltaper_source <- paste(
  "US Forest Service National Volume Estimator Library,",
  "NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.smalltaper_inputs <- function(family, id) {
  if (identical(family, "r2_taper") && substr(id, 4L, 6L) == "CZ3") {
    return(list(
      required = character(), optional = c("upper_ht1", "upper_d1", "bark_ratio"),
      pairs = list(c("upper_ht1", "upper_d1"))
    ))
  }
  if (family %in% c("r12_taper", "blm_taper", "behre_taper")) {
    return(list(
      required = character(), optional = c("form_class", "bark_ratio"),
      pairs = list()
    ))
  }
  list(required = character(), optional = "bark_ratio", pairs = list())
}

.smalltaper_notes <- function(family, id) {
  switch(family,
    r1_taper = paste(
      "Region 1 Byrne profile. CALCDIA dispatch evaluates the lodgepole",
      "profile for every JB2 species. Total volume retains species routing."
    ),
    r2_taper = if (substr(
      id,
      4L, 6L
    ) == "CZ3") {
      paste(
        "Region 2 Czaplewski three-point profile. Missing upper_ht1 and",
        "upper_d1 reproduce NVEL ERRFLAG 9."
      )
    } else {
      "Region 2 Max-Burkhart profile with Czaplewski second-stage correction."
    },
    r5_taper = paste(
      "Region 5 Wensel and Krumland profile, dispatched by NVEL's WO2",
      "Wensel/Olsen mnemonic."
    ),
    r12_taper = paste(
      "Region 12 Sharpneck volume family. CALCDIA does not dispatch R12TAP.",
      "It uses Raile stump DIB below 4.5 feet and zero above it."
    ),
    blm_taper = paste(
      "BLM profile-group Behre hyperbola. CALCDIA and HT2TOPD default",
      "form_class. Volume without form_class reproduces NVEL ERRFLAG 2."
    ),
    behre_taper = paste(
      "Genuine fixed-A Behre taper from BEHTAP. CALCDIA and HT2TOPD default",
      "form_class. Volume without form_class reproduces NVEL ERRFLAG 2."
    )
  )
}

.smalltaper_model <- function(id, family, species) {
  species <- as.integer(species)
  if (!is.finite(species) || species <= 0L)
    species <- integer()
  new_stem_model_unchecked(
    id = id, form = family, kernel = list(type = "compiled", key = paste0(
      "smalltapers:",
      id
    ), has_dob = FALSE, has_inverse = TRUE, has_integral = TRUE),
    inputs = .smalltaper_inputs(
      family,
      id
    ), measurement_system = "imperial", spcd = species, stump_ht = 1, bark_ratio = NA_real_,
    source = .smalltaper_source,
    oracle_verified = .nvel_oracle_verified(id), notes = paste(.smalltaper_notes(
      family,
      id
    ), "This NVEL path has no outside-bark profile. dob() requires bark_ratio.")
  )
}

.smalltaper_models <- function() {
  path <- system.file("extdata", "smalltaper_models.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE)
  Map(.smalltaper_model, metadata$id, metadata$family, metadata$species)
}

.smalltaper_patterns <- function() {
  data.frame(id = c(
    "???JB2????", "???CZ2????", "???CZ3????", "???WO2????", "???SN2????", "B??BEH????",
    "???BEH????"
  ), family = c(
    "r1_taper", "r2_taper", "r2_taper", "r5_taper", "r12_taper",
    "blm_taper", "behre_taper"
  ), stringsAsFactors = FALSE)
}

.smalltaper_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) != 10L) {
    return(NULL)
  }
  mnemonic <- substr(id, 4L, 6L)
  family <- if (mnemonic == "JB2") {
    "r1_taper"
  } else if (mnemonic %in% c("CZ2", "CZ3")) {
    "r2_taper"
  } else if (mnemonic == "WO2") {
    "r5_taper"
  } else if (mnemonic == "SN2") {
    "r12_taper"
  } else if (mnemonic == "BEH" && substr(id, 1L, 1L) == "B") {
    "blm_taper"
  } else if (mnemonic == "BEH") {
    "behre_taper"
  } else {
    return(NULL)
  }
  if (!grepl("^[[:alnum:]]{10}$", id) || substr(id, 7L, 7L) != "W" || !grepl(
    "^[0-9]{3}$",
    substr(id, 8L, 10L)
  )) {
    return(NULL)
  }
  model <- .smalltaper_model(id, family, as.integer(substr(id, 8L, 10L)))
  if (!tv_cpp_kernel_exists(model$kernel$key))
    return(NULL)
  model
}

NULL
