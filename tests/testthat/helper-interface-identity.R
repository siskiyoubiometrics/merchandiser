# Geometry follows the segment rulings, independent of the cutting
# implementation.
interface_defect_expectation <- function(effect, recording) {
  top <-
    recording$defects[[effect]]$residuals[["from"]][recording$defects[[effect]]$residuals$cause ==
                                                    "top"]
  if (effect %in% c("cull", "pulp", "exclude")) {
    start <- c(20, 52.5, 85)
    length <- c(32, 32, 16)
    trim <- c(0.5, 0.5, 0)
    product <- c("saw", "saw", "pulp")
    residuals <- data.frame(
      start_height = c(0, 1, 10, 52, 84.5, 101, top), end_height = c(
        1,
        10,
        20,
        52.5,
        85,
        top,
        120
      ),
      cause = c(
        "stump",
        "short_remainder",
        if (
            effect ==
              "pulp") "restricted" else "cull",
        "trim",
        "trim",
        "short_remainder",
        "top"
      )
    )
  } else if (effect == "fork") {
    start <- c(10, 26, 42, 58, 74, 90)
    length <- rep(16, 6)
    trim <- rep(0, 6)
    product <- rep("pulp", 6)
    residuals <- data.frame(
      start_height = c(0, 1, 106, top), end_height = c(1, 10, top, 120),
      cause = c("stump", "short_remainder", "restricted", "top")
    )
  } else if (effect == "break") {
    start <- length <- trim <- numeric()
    product <- character()
    residuals <- data.frame(start_height = c(0, 1, 10), end_height = c(1, 10, 120), cause = c(
      "stump",
      "short_remainder",
      "end"
    ))
  } else {
    start <- c(1, 33.5, 66, 82)
    length <- c(32, 32, 16, 16)
    trim <- c(0.5, 0.5, 0, 0)
    product <- c("saw", "saw", "pulp", "pulp")
    residuals <- data.frame(
      start_height = c(0, 33, 65.5, 98, top), end_height = c(
        1, 33.5, 66,
        top,
        120
      ),
      cause = c(
        "stump",
        "trim",
        "trim",
        "short_remainder",
        "top"
      )
    )
  }
  end <- start + length + trim
  diameter <- function(h) dib(24, 120, h, 202, model = "F00FW2W202")$value
  volume <- function(h) {
    stem_volume(
      24,
      120,
      202,
      model = "F00FW2W202",
      from = 0,
      to = h
    )$value
  }
  logs <- data.frame(
    tree_id = rep(1, length(start)), log = seq_along(start), product = product,
    start_height = start,
    end_height = end, length = length, scaling_length = length,
    sed = diameter(end), led = diameter(start), scaling_diameter = rep(
      NA_real_,
      length(
        start
      )
    ),
    inside_bark = rep(
      TRUE,
      length(
        start
      )
    ),
    scale = volume(
      start +
        length
    ) -
      volume(
        start
      ),
    volume_unit = rep(
      "cubic",
      length(
        start
      )
    )
  )
  residuals <- data.frame(tree_id = 1, residuals)
  list(logs = logs, residuals = residuals)
}
