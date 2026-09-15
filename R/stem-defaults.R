.nvel_defaults_source <- paste(
  "US Forest Service National Volume Estimator Library",
  "commit 38548071d5aa652bb90c7f111f86b427f798a1c9"
)

.nvel_reference <- local({
  cache <- new.env(parent = emptyenv())
  function(name) {
    value <- cache[[name]]
    if (!is.null(value))
      return(value)
    path <- system.file("extdata", name, package = "merchandiser", mustWork = TRUE)
    value <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
    cache[[name]] <- value
    value
  }
})

.nvel_array <- local({
  cache <- new.env(parent = emptyenv())
  function(routine, name, type = c("numeric", "character")) {
    type <- match.arg(type)
    key <- paste(routine, name, type, sep = ":")
    value <- cache[[key]]
    if (!is.null(value))
      return(value)
    data <- .nvel_reference("nvel_default_arrays.csv")
    target_match <- grepl(paste0("(^|[(])", name, "([(]|$)"), data$target,
      ignore.case = TRUE
    )
    selected <- data$routine == routine & target_match
    rows <- data[selected, , drop = FALSE]
    rows <- rows[order(rows$source_line_start, rows$ordinal), , drop = FALSE]
    if (!nrow(rows))
      stop("missing generated NVEL array: ", routine, "/", name)
    value <- if (type == "numeric")
      rows$value_numeric else rows$value_character
    cache[[key]] <- value
    value
  }
})

.nvel_eq <- function(routine) .nvel_array(routine, "EQNUM", "character")
.nvel_fia <- function(routine) .nvel_array(routine, "FIA", "numeric")

.nvel_default_variant <- function(region, forest, district) {
  if (region == 8L)
    return("SN")
  if (region == 1L) {
    return(if (forest %in% c(3L, 4L, 5L, 14L, 16L, 17L)) "IE" else "EM")
  }
  if (region == 5L) {
    if (forest %in% c(5L, 6L, 8L, 11L, 14L))
      return("CA")
    if (forest == 9L)
      return("SO")
    if (forest %in% c(3L, 13L, 15L, 16L, 17L))
      return("WS")
    return("")
  }
  if (region == 6L) {
    if (forest %in% c(4L, 7L, 14L, 16L))
      return("BM")
    if (forest %in% c(8L, 17L) || (forest == 3L && district == 3L) || (
                                                                       forest == 6L && district %in%
                                                                         c(1L, 2L, 6L)))
      return("EC")
    if (forest %in% c(1L, 2L, 20L))
      return("SO")
    if (forest %in% c(3L, 5L, 6L, 10L, 15L, 18L))
      return("WC")
    if (forest %in% c(9L, 12L))
      return("PN")
    if (forest == 11L)
      return("NC")
    if (forest == 21L)
      return("IE")
    return("")
  }
  if (region == 7L) {
    if (forest == 2L)
      return("WC")
    if (forest == 3L)
      return("NC")
    return("SO")
  }
  if (region == 9L) {
    if (forest %in% c(2L, 3L, 4L, 6L, 7L, 9L, 10L, 13L))
      return("LS")
    if (forest %in% c(5L, 8L, 12L))
      return("CS")
    if (forest %in% c(14L, 19L, 20L, 21L, 22L))
      return("NE")
  }
  ""
}

.nvel_lookup <- function(fia, equations, species, fallback) {
  index <- match(species, fia)
  if (is.na(index))
    equations[[fallback]] else equations[[index]]
}

.nvel_r1_default <- function(forest, species, variant) {
  fia <- .nvel_fia("R1_EQN")
  equations <- .nvel_eq("R1_EQN")
  species <- switch(as.character(species),
    `70` = 73L,
    `90` = 93L,
    `260` = 263L,
    species
  )
  if (species == 122L && forest == 8L)
    return(equations[[39L]])
  if (species == 101L) {
    if (variant %in% c("EM", "IE", "CI"))
      return(equations[[1L]])
    return(NA_character_)
  }
  if (variant == "EM" && species %in% c(745L, 747L, 749L)) {
    return(equations[[40L]])
  }
  .nvel_lookup(fia, equations, species, if (species < 300L)
                 26L else 36L)
}

.nvel_r2_default <- function(forest, species) {
  equations <- .nvel_eq("R2_EQN")
  if (species == 122L && forest == 3L)
    return(equations[[43L]])
  if (species == 122L && forest == 13L)
    return(equations[[44L]])
  if (species == 108L && forest %in% c(2L, 14L))
    return(equations[[45L]])
  .nvel_lookup(.nvel_fia("R2_EQN"), equations, species, 42L)
}

.nvel_r3_default <- function(species) {
  .nvel_lookup(.nvel_fia("R3_EQN"), .nvel_eq("R3_EQN"), species, 45L)
}

.nvel_r4_default <- function(forest, species) {
  equations <- .nvel_eq("R4_EQN")
  if (species == 64L && !forest %in% c(
    1L, 2L, 4L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 17L,
    18L, 19L
  ))
    return(NA_character_)
  if (species == 106L && !forest %in% c(1L, 4L, 7L, 8L, 9L, 10L, 17L, 18L, 19L))
    return(NA_character_)
  index <- if (species == 15L) {
    if (forest %in% c(2L, 6L, 12L, 13L))
      28L else if (forest %in% c(9L, 17L))
      29L else 30L
  } else if (species == 17L) {
    if (forest %in% c(2L, 12L, 13L))
      31L else 30L
  } else if (species == 19L) {
    if (forest == 5L)
      32L else 33L
  } else if (species == 64L) {
    49L
  } else if (species == 65L) {
    if (forest %in% c(3L, 5L, 15L, 16L))
      34L else 35L
  } else if (species %in% c(93L, 96L)) {
    if (forest %in% c(2L, 12L, 13L))
      36L else if (forest == 7L)
      37L else if (forest == 8L)
      56L else 38L
  } else if (species %in% c(101L, 108L, 113L, 142L)) {
    if (forest %in% c(9L, 17L))
      39L else 40L
  } else if (species == 106L) {
    50L
  } else if (species == 122L) {
    if (forest %in% c(2L, 12L, 13L))
      41L else if (forest == 1L)
      42L else if (forest %in% c(7L, 8L, 10L, 18L, 19L))
      43L else if (forest %in% c(9L, 17L))
      44L else 45L
  } else if (species == 202L) {
    if (forest %in% c(2L, 12L, 13L))
      46L else if (forest == 5L)
      47L else 48L
  } else if (species == 998L) {
    if (forest %in% c(2L, 6L, 12L, 13L, 14L))
      17L else 21L
  } else {
    match(species, .nvel_fia("R4_EQN"))
  }
  if (is.na(index))
    equations[[26L]] else equations[[index]]
}

.nvel_r5_default <- function(species, variant) {
  equations <- .nvel_eq("R5_EQN")
  if (species == 60L)
    species <- 62L
  index <- if (species == 101L) {
    if (variant == "SO")
      30L else 40L
  } else if (species %in% c(290L, 299L)) {
    if (variant %in% c("SO", "NC"))
      30L else 40L
  } else if (species == 998L) {
    if (variant %in% c("SO", "NC"))
      70L else if (variant == "WS")
      67L else 60L
  } else {
    match(species, .nvel_fia("R5_EQN"))
  }
  if (is.na(index))
    equations[[72L]] else equations[[index]]
}

.nvel_r6_set <- function(forest, district, species, west) {
  i <- f <- rd <- 0L
  if (west) {
    if (forest == 3L) {
      if (species == 11L) {
        i <- 26L
      } else if (species == 19L) {
        i <- 6L
      } else if (species %in% c(260L, 263L)) {
        f <- if (district == 1L)
          21L else if (district == 5L)
          15L else 3L
      } else if (species == 202L) {
        f <- if (district == 1L)
          22L else 10L
      } else if (species == 351L) {
        rd <- 1L
      }
    } else if (forest == 6L) {
      i <- switch(as.character(species),
        `11` = 26L,
        `17` = 38L,
        `93` = 17L,
        `108` = 18L,
        `122` = 32L,
        `260` = 23L,
        `263` = 23L,
        `22` = 38L,
        0L
      )
      if (species == 202L)
        f <- 10L
    } else if (forest == 5L) {
      if (species %in% c(260L, 263L))
        f <- 12L else if (species == 202L)
        f <- 25L
    } else if (forest %in% c(10L, 11L)) {
      i <- switch(as.character(species),
        `15` = 5L,
        `122` = 4L,
        0L
      )
      f <- switch(as.character(species),
        `202` = 19L,
        `263` = 21L,
        0L
      )
    } else if (forest == 12L) {
      f <- switch(as.character(species),
        `202` = 1L,
        `263` = 12L,
        0L
      )
      if (species == 351L)
        rd <- 2L
    } else if (forest == 9L) {
      f <- switch(as.character(species),
        `202` = 10L,
        `98` = 12L,
        `263` = 3L,
        0L
      )
    } else if (forest == 15L) {
      i <- switch(as.character(species),
        `15` = 2L,
        `20` = 1L,
        `21` = 1L,
        `81` = 4L,
        `93` = 5L,
        `108` = 6L,
        `122` = 4L,
        `242` = 1L,
        `263` = 23L,
        `264` = 10L,
        `11` = 76L,
        `103` = 11L,
        0L
      )
      if (species == 202L)
        f <- 1L
    } else if (forest == 18L) {
      i <- switch(as.character(species),
        `22` = 6L,
        `17` = 64L,
        `81` = 68L,
        0L
      )
      f <- switch(as.character(species),
        `202` = 16L,
        `263` = 12L,
        0L
      )
    }
  } else {
    if (forest == 1L) {
      i <- if (species %in% c(11L, 15L, 17L, 21L))
        2L else switch(as.character(species),
        `73` = 16L,
        `108` = 18L,
        `122` = 20L,
        `202` = 21L,
        `81` = 22L,
        `117` = 44L,
        0L
      )
    } else if (forest %in% c(2L, 20L)) {
      i <- if (species %in% c(15L, 17L))
        14L else switch(as.character(species),
        `81` = 9L,
        `108` = 6L,
        `122` = 8L,
        `202` = 2L,
        0L
      )
    } else if (forest == 3L) {
      if (species == 11L)
        i <- 26L else if (species %in% c(260L, 263L) && district == 3L)
        f <- 3L else if (species == 202L && district == 3L)
        i <- 10L
    } else if (forest == 4L) {
      i <- if (species %in% c(15L, 17L))
        26L else switch(as.character(species),
        `108` = 30L,
        `122` = 32L,
        `202` = 33L,
        0L
      )
    } else if (forest == 6L) {
      i <- switch(as.character(species),
        `11` = 26L,
        `17` = 38L,
        `93` = 17L,
        `108` = 18L,
        `122` = 32L,
        `260` = 23L,
        `263` = 23L,
        `22` = 38L,
        0L
      )
      if (species == 202L)
        f <- if (district %in% c(1L, 6L))
          22L else 16L
    } else if (forest == 7L) {
      i <- if (species %in% c(15L, 17L))
        26L else switch(as.character(species),
        `73` = 28L,
        `122` = 32L,
        `202` = 33L,
        `108` = 30L,
        0L
      )
    } else if (forest == 14L) {
      i <- if (species %in% c(15L, 17L))
        38L else switch(as.character(species),
        `19` = 3L,
        `73` = 45L,
        `93` = 5L,
        `108` = 6L,
        `122` = 44L,
        `202` = 38L,
        0L
      )
    } else if (forest == 16L) {
      i <- if (species %in% c(15L, 17L))
        14L else switch(as.character(species),
        `73` = 40L,
        `93` = 17L,
        `108` = 6L,
        `122` = 20L,
        `202` = 21L,
        0L
      )
    } else if (forest %in% c(8L, 17L)) {
      i <- switch(as.character(species),
        `17` = 14L,
        `202` = 33L,
        `108` = 30L,
        `93` = 17L,
        `122` = 32L,
        `73` = 32L,
        0L
      )
      if (species %in% c(73L, 122L) && district %in% c(2L, 3L, 5L, 7L))
        i <- 20L
    } else if (forest == 21L) {
      i <- if (species %in% c(17L, 260L, 263L, 264L))
        14L else switch(as.character(species),
        `19` = 21L,
        `73` = 16L,
        `93` = 41L,
        `108` = 18L,
        `119` = 7L,
        `122` = 32L,
        `202` = 21L,
        `242` = 22L,
        0L
      )
    }
  }
  c(i = i, f = f, rd = rd)
}

.nvel_r6_default <- function(forest, district, species, variant) {
  west <- variant %in% c("PN", "WC", "NC", "CA", "OC", "OP")
  selected <- .nvel_r6_set(forest, district, species, west)
  if (selected[["i"]] > 0L) {
    return(.nvel_array("R6_EQN", "EQNUMI", "character")[[selected[["i"]]]])
  }
  if (selected[["f"]] > 0L) {
    return(.nvel_array("R6_EQN", "EQNUMF", "character")[[selected[["f"]]]])
  }
  if (selected[["rd"]] > 0L) {
    return(.nvel_array("R6_EQN", "EQNUMRD", "character")[[selected[["rd"]]]])
  }
  fia <- .nvel_fia("R6_EQN")
  if (species %in% fia)
    sprintf("616BEHW%03d", species) else "616BEHW000"
}

.nvel_r7_default <- function(forest, species, variant) {
  equations <- .nvel_eq("R7_EQN")
  index <- 0L
  if (species == 202L) {
    northwest <- forest == 12L && variant %in% c("PN", "NC", "CA", "OC", "OP")
    index <- if (northwest)
      42L else 44L
  } else if (species == 41L && variant %in% c("CA", "OC")) {
    index <- 13L
  } else if (species == 263L && variant %in% c("CA", "PN", "OC", "OP")) {
    index <- 45L
  } else if (species %in% c(109L, 113L, 124L, 127L) && variant %in% c("CA", "OC")) {
    index <- 18L
  } else if (species == 92L && variant %in% c("CA", "OC")) {
    index <- 14L
  } else if (species == 212L && variant %in% c("CA", "OC")) {
    index <- 24L
  } else if (species %in% c(801L, 805L, 807L, 811L, 818L, 821L, 839L, 333L, 730L) &&
               variant %in%
                 c("CA", "OC")) {
    index <- 38L
  } else if (species == 818L && variant == "NC") {
    index <- 38L
  } else if (species == 542L && variant %in% c("CA", "OC")) {
    index <- 30L
  } else if (species == 251L && variant %in% c("CA", "OC")) {
    index <- 25L
  } else if (species == 981L && variant %in% c("CA", "OC")) {
    index <- 36L
  }
  if (!index)
    index <- match(species, .nvel_fia("R7_EQN"), nomatch = 0L)
  if (!index)
    "B00BEHW999" else equations[[index]]
}

.nvel_r8_default <- function(forest, district, species) {
  geo <- NA_integer_
  if (forest == 1L) {
    geo <- if (district == 3L)
      1L else 4L
  } else if (forest %in% c(2L, 4L, 8L, 60L)) {
    geo <- 3L
  } else if (forest == 3L) {
    geo <- if (district == 8L)
      2L else 3L
  } else if (forest %in% c(5L, 36L)) {
    geo <- 1L
  } else if (forest %in% c(6L, 13L)) {
    geo <- 5L
  } else if (forest == 7L) {
    if (district == 6L)
      geo <- 7L else if (district %in% c(7L, 17L))
      geo <- 4L else geo <- 5L
  } else if (forest == 9L) {
    geo <- 6L
  } else if (forest == 10L) {
    geo <- if (district == 7L)
      7L else 6L
  } else if (forest == 11L) {
    if (district == 3L)
      geo <- 1L else if (district == 10L)
      geo <- 2L else geo <- 3L
  } else if (forest == 12L) {
    if (district == 2L)
      geo <- 3L else if (district == 5L)
      geo <- 1L else geo <- 2L
  }
  fia <- .nvel_array("R8_CEQN", "SNFIA", "numeric")
  codes <- .nvel_array("R8_CEQN", "SNSP", "character")
  index <- match(species, fia)
  if (is.na(index))
    index <- if (species < 300L)
      22L else 110L
  if (is.na(geo))
    NA_character_ else sprintf("8%d1CLKE%s", geo, codes[[index]])
}

.nvel_r10_default <- function(forest, species) {
  fia <- .nvel_array("R10_EQN", "FIA", "numeric")
  name <- if (forest == 4L)
    "CHUEQN" else "TONEQN"
  equations <- .nvel_array("R10_EQN", name, "character")
  index <- match(species, fia)
  if (is.na(index))
    index <- 13L
  equations[[index]]
}

.nvel_default_one <- function(region, forest, district, species, variant, product) {
  if (anyNA(c(region, forest, district, species)))
    return(NA_character_)
  if (is.na(variant))
    variant <- ""
  variant <- toupper(trimws(variant))
  if (!nzchar(variant)) {
    variant <- .nvel_default_variant(region, forest, district)
  } else {
    choices <- switch(as.character(region),
      `1` = c("EM", "IE", "CI"),
      `5` = c("CA", "SO", "WS", "NC"),
      `6` = c("BM", "EC", "SO", "WC", "PN", "NC", "IE", "CA", "OC", "OP"),
      `7` = c("WC", "NC", "SO", "PN", "CA", "OC", "OP"),
      `8` = "SN",
      `9` = c("LS", "CS", "NE", "SN"),
      character()
    )
    if (!variant %in% choices) {
      stop("Unknown variant '", variant, "' for region ", region, ".", call. = FALSE)
    }
  }
  switch(as.character(region),
    `1` = .nvel_r1_default(forest, species, variant),
    `2` = .nvel_r2_default(
      forest,
      species
    ),
    `3` = .nvel_r3_default(species),
    `4` = .nvel_r4_default(forest, species),
    `5` = .nvel_r5_default(
      species,
      variant
    ),
    `6` = .nvel_r6_default(forest, district, species, variant),
    `7` = .nvel_r7_default(
      forest,
      species, variant
    ),
    `8` = .nvel_r8_default(forest, district, species),
    `9` = sprintf(
      "900CLKE%03d",
      species
    ),
    `10` = .nvel_r10_default(forest, species),
    NA_character_
  )
}

.nvel_default_recycle <- function(values) {
  if (any(lengths(values) == 0L)) {
    return(lapply(values, rep, length.out = 0L))
  }
  size <- .common_size(values)
  lapply(values, rep, length.out = size)
}

.nvel_integer_code <- function(value, name, minimum, maximum) {
  if (!is.numeric(value) || !is.null(dim(value))) {
    stop(name, " must contain numeric whole numbers.", call. = FALSE)
  }
  text <- trimws(as.character(value))
  number <- suppressWarnings(as.double(text))
  supplied <- !is.na(value)
  invalid <- supplied & (!is.finite(number) | number != floor(number) | number < minimum |
                           number > maximum)
  if (any(invalid)) {
    stop(name, " must be an integer from ", minimum, " through ", maximum, ".",
      call. = FALSE
    )
  }
  as.integer(number)
}

#' Look up a source equation from species and geographic codes
#'
#' @param region The Forest Service region selects the source defaults. Numeric vector,
#'   whole-number region codes. Required, with no default.
#' @param forest The national forest code selects local source rules. Numeric vector, whole-number
#'   forest codes. Required, with no default.
#' @param district The ranger district code selects local source rules. Numeric vector,
#'   whole-number district codes. Required, with no default.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param variant Select the geographic variant for default equation lookup. Character vector,
#'   unitless, default `NULL`. Missing and blank values request the source geographic mapping.
#'   The source uses regional branches rather than one complete choice list.
#' @return A character vector of source equation identifiers in input order.   Identifiers are
#'   unitless. A selected source equation can be unsupported   by the package, so check
#'   availability before computing.
#' @usage
#' nvel_default_equation(
#'   region,
#'   forest,
#'   district,
#'   spcd,
#'   variant = NULL
#' )
#' @export
#' @examples
#' ## Find the source default for the Pacific Northwest example species.
#' nvel_default_equation(region = 6,
#'                       forest = 12,
#'                       district = 0,
#'                       spcd = example_trees$spcd[1])
nvel_default_equation <- function(
  region,
  forest,
  district,
  spcd,
  variant = NULL
) {
  if (!is.numeric(spcd)) {
    stop("spcd must contain numeric species codes.", call. = FALSE)
  }
  if (is.null(variant))
    variant <- ""
  values <- .nvel_default_recycle(list(
    region = region, forest = forest, district = district,
    spcd = spcd, variant = variant
  ))
  values$variant <- as.character(values$variant)
  values$region <- .nvel_integer_code(values$region, "region", 1L, 10L)
  values$forest <- .nvel_integer_code(values$forest, "forest", 0L, 99L)
  values$district <- .nvel_integer_code(values$district, "district", 0L, 99L)
  values$spcd <- .nvel_integer_code(values$spcd, "spcd", 1L, 9999L)
  vapply(seq_along(values$region), function(index) {
    .nvel_default_one(
      values$region[[index]], values$forest[[index]], values$district[[index]],
      values$spcd[[index]], values$variant[[index]], "01"
    )
  }, character(1))
}

.nvel_round_positive <- function(value) floor(value + 0.5)

.nvel_crosswalk_one <- function(fia_code, species, geosub, primary_top, compat) {
  result <- list(
    model = NA_character_, volume_type = NA_character_, primary_top = primary_top,
    errflag = 0L
  )
  if (is.na(fia_code) || is.na(species) || is.na(geosub) || is.na(primary_top)) {
    result$errflag <- 1L
    return(result)
  }
  code <- toupper(fia_code)
  crosswalk <- .nvel_reference("nvel_fia_crosswalk.csv")
  index <- match(code, crosswalk$fia_code)
  if (is.na(index)) {
    result$errflag <- 1L
    return(result)
  }
  model <- crosswalk$nvel_template[[index]]
  type <- crosswalk$volume_type[[index]]
  if (primary_top >= 99) {
    result$model <- model
    result$volume_type <- type
    return(result)
  }
  if (species == 7503L && code %in% c("CU000050", "CU000051", "BD000040")) {
    model <- "H01SN2W510"
  } else if (code %in% c("CU000116", "BD000090", "BD000091") && species %in% c(98L, 263L) &&
               grepl("^[Aa]", geosub)) {
    substr(model, 7L, 7L) <- "A"
  } else if (code %in% c(
    "CU000119", "CU000120", "CU042001", "CU042002", "BD042002", "CU242002",
    "BD242002", "BD000095"
  ) && grepl("^[Aa]", geosub)) {
    substr(model, 7L, 7L) <- "A"
  }
  if (substr(model, 7L, 10L) == "****") {
    model <- paste0(substr(model, 1L, 6L), sprintf("%04d", species))
  } else if (substr(model, 8L, 10L) == "***") {
    if (species > 999L) {
      result$errflag <- 6L
      return(result)
    }
    model <- paste0(substr(model, 1L, 7L), sprintf("%03d", species))
  }
  geo <- trimws(geosub)
  if (substr(model, 1L, 3L) == "8**") {
    replacement <- "825"
    if (compat == "port" && grepl("^[1-9]$", geo)) {
      replacement <- paste0("80", geo)
    } else if (compat == "port" && grepl("^(0[1-9]|[12][0-9]|3[0-2])$", geo)) {
      replacement <- paste0("8", geo)
    }
    substr(model, 1L, 3L) <- replacement
  } else if (substr(model, 1L, 3L) == "9**") {
    replacement <- "900"
    if (compat == "port" && grepl("^[1-6]$", geo)) {
      replacement <- paste0("90", geo)
    } else if (compat == "port" && grepl("^(0[1-6]|11|12)$", geo)) {
      replacement <- paste0("9", geo)
    }
    substr(model, 1L, 3L) <- replacement
  }
  if (substr(model, 1L, 2L) == "8*") {
    replacement <- "89"
    if (compat == "port" && grepl("^[1-7]$", geo)) {
      replacement <- paste0("8", geo)
    } else if (compat == "port" && nchar(geo) == 2L && substr(geo, 1L, 1L) >= "0" && grepl(
      "[1-7]$",
      geo
    )) {
      replacement <- paste0("8", substr(geo, 2L, 2L))
    }
    substr(model, 1L, 2L) <- replacement
  }
  if (substr(type, 1L, 3L) == "CV*") {
    if (primary_top == 0.1)
      type <- "CVT" else if (primary_top <= 4) {
      primary_top <- 4
      type <- "CV4"
    } else {
      type <- paste0("CV", .nvel_round_positive(primary_top))
    }
  } else if (substr(type, 3L, 5L) == "7/9") {
    primary_top <- if (species < 300L)
      7 else 9
    type <- paste0(substr(type, 1L, 2L), primary_top)
  } else if (substr(type, 3L, 5L) == "6/8") {
    if (primary_top < 6)
      primary_top <- 6
    if (primary_top > 9)
      primary_top <- 8
    type <- paste0(substr(type, 1L, 2L), .nvel_round_positive(primary_top))
  } else if (substr(type, 1L, 5L) == "CV1.5") {
    primary_top <- 1.5
  } else if (substr(type, 1L, 3L) == "SV*" || type == "IV*") {
    if (primary_top < 6)
      primary_top <- 6
    type <- paste0(substr(type, 1L, 2L), .nvel_round_positive(primary_top))
  } else if (substr(type, 1L, 3L) == "TIP") {
    primary_top <- as.double(substr(type, 4L, 4L))
  } else if (grepl("^[A-Z]{2}[1-9]", type)) {
    primary_top <- as.double(sub("^[A-Z]{2}([0-9]{1,2}).*$", "\\1", type))
  }
  result$model <- model
  result$volume_type <- type
  result$primary_top <- primary_top
  result
}

#' Translate inventory equation codes to source equation names
#'
#' @param fia_code Identify the inventory volume equation to translate. Character vector,
#'   unitless, required. Surrounding spaces are removed, letters are made uppercase, and the
#'   first eight characters are used. Missing or unrecognized codes give source error flag 1.
#' @param spcd Numeric inventory species code. Numeric
#'   vector, species codes. Required, with no default.
#' @param geosub Select the geographic substitution for the equation. Character vector,
#'   unitless, default `''` uses the source's unqualified substitution. Missing gives flag 1.
#' @param primary_top Set the primary product's top diameter. Numeric vector, inches,
#'   default `0` uses the source volume type's diameter convention. Missing gives flag 1.
#'   Values at least 99 preserve the equation template and volume type without substitutions.
#' @return A data frame with `model` (equation identifier), `volume_type` (source volume code),
#'   `primary_top` (inches), and `errflag` (unitless source error code, zero for success).
#' @usage
#' nvel_from_fia_code(
#'   fia_code,
#'   spcd,
#'   geosub = '',
#'   primary_top = 0
#' )
#' @details
#' Each nonmissing `fia_code` must have exactly eight characters.
#' @export
#' @examples
#' ## Translate a source inventory equation for the example species.
#' nvel_from_fia_code(fia_code = 'BD000006',
#'                    spcd = 202)
nvel_from_fia_code <- function(fia_code, spcd, geosub = "", primary_top = 0) {
  compat <- .treevolume_compat()
  values <- .nvel_default_recycle(list(
    fia_code = fia_code, spcd = spcd,
    geosub = geosub, primary_top = primary_top
  ))
  if (!is.character(values$fia_code) || !is.character(values$geosub) || !is.numeric(
    values$spcd
  ) ||
    !is.numeric(values$primary_top)) {
    stop("fia_code and geosub must be character. ",
      "The spcd and primary_top arguments must be numeric.",
      call. = FALSE
    )
  }
  if (any(!is.na(values$fia_code) & nchar(values$fia_code) != 8L)) {
    stop("fia_code must contain exactly eight characters.", call. = FALSE)
  }
  invalid_spcd <- !is.na(values$spcd) & (!is.finite(values$spcd) |
                                           values$spcd != floor(values$spcd) |
                                           values$spcd < 1 | values$spcd > 9999)
  if (any(invalid_spcd)) {
    stop("spcd must contain integer FIA codes from 1 through 9999.", call. = FALSE)
  }
  invalid_top <- !is.na(values$primary_top) & (!is.finite(values$primary_top) |
                                                 values$primary_top < 0 | values$primary_top > 99)
  if (any(invalid_top)) {
    stop("primary_top must be finite from zero through 99 or NA.", call. = FALSE)
  }
  rows <- lapply(seq_along(values$fia_code), function(index) {
    .nvel_crosswalk_one(
      values$fia_code[[index]], as.integer(values$spcd[[index]]), values$geosub[[index]],
      as.double(values$primary_top[[index]]), compat
    )
  })
  data.frame(
    model = vapply(rows, `[[`, character(1), "model"), volume_type = vapply(
      rows,
      `[[`, character(1), "volume_type"
    ), primary_top = vapply(rows, `[[`, double(1), "primary_top"),
    errflag = vapply(rows, `[[`, integer(1), "errflag"), stringsAsFactors = FALSE
  )
}

.nvel_r8_beqn_valid <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl(
    "^8([0-2][0-9]|3[0-3])DVEE[0-9]{3}$",
    id
  ))
    return(FALSE)
  substr(id, 8L, 10L) %in% .nvel_array("R8_BEQN", "SNSP", "character")
}

.nvel_r8_ceqn_valid <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl(
    "^8[1-7][14789]CLKE[0-9]{3}$",
    id
  ))
    return(FALSE)
  substr(id, 8L, 10L) %in% .nvel_array("R8_CEQN", "SNSP", "character")
}

.nvel_r9_valid <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !(substr(
    id, 1L,
    7L
  ) == "900CLKE" ||
    substr(id, 4L, 7L) == "DVEE")) {
    return(FALSE)
  }
  suffix <- substr(id, 8L, 10L)
  any(vapply(c("LSSP", "CSSP", "NESP", "SNSP"), function(name) {
    suffix %in% .nvel_array("R9_EQN", name, "character")
  }, logical(1)))
}
