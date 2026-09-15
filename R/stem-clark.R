.clark_source <- paste(
  "US Forest Service National Volume Estimator Library,",
  "NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.clark_model <- function(id, family, species) {
  old_region8 <- identical(family, "clark_r8")
  new_stem_model_unchecked(
    id = id, form = family, kernel = list(type = "compiled", key = paste0(
      "clark:",
      id
    ), has_dob = FALSE, has_inverse = TRUE, has_integral = TRUE), inputs = list(
      required = character(),
      optional = c("upper_ht1", "site_index", "basal_area", "bark_ratio"), pairs = list()
    ),
    measurement_system = "imperial",
    spcd = as.integer(species), stump_ht = 1, bark_ratio = NA_real_,
    source = .clark_source, oracle_verified = .nvel_oracle_verified(id),
    notes = if (old_region8) {
      paste(
        "Region 8 Clark profile. upper_ht1 is the height to the top code",
        "in character 3. When omitted, NVEL derives it from total height."
      )
    } else {
      paste(
        "Region 9 Clark form, including the Region 8 top-code-1 route.",
        "NVEL does not provide a Clark outside-bark profile through CALCDIA."
      )
    }
  )
}

.clark_models <- function() {
  path <- system.file("extdata", "clark_models.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(
    id = "character"
  ))
  Map(.clark_model, metadata$id, metadata$family, metadata$species)
}

.clark_patterns <- function() {
  path <- system.file("extdata", "clark_patterns.csv",
    package = "merchandiser",
    mustWork = TRUE
  )
  metadata <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(
    id = "character"
  ))
  metadata[c("id", "family")]
}

.clark_pattern_model <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    return(NULL)
  }
  region9 <- grepl("^9[0-9]{2}CLK.[0-9]{3}$", id)
  region8 <- grepl("^8[0-9][014789]CLK.[0-9]{3}$", id)
  if (!region9 && !region8) {
    return(NULL)
  }
  species <- as.integer(substr(id, 8L, 10L))
  if (!is.finite(species) || species <= 0L) {
    return(NULL)
  }
  family <- if (region9 || substr(id, 3L, 3L) == "1") {
    "clark_r9"
  } else {
    "clark_r8"
  }
  .clark_model(id, family, species)
}

NULL
