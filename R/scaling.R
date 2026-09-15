.mc_round_dimension <- function(x, operator, dimension, measurement_system) {
  if (operator %in% c("rule_default", "none"))
    return(x)
  truncate <- startsWith(operator, "truncate")
  if (dimension == "diameter") {
    if (grepl("in", operator, fixed = TRUE)) {
      native <- if (measurement_system == "imperial")
        x else x / 2.54
      increment <- if (grepl("0.5in", operator, fixed = TRUE))
        0.5 else 1
      rounded <- if (truncate)
        floor(native / increment) * increment else floor(native / increment + 0.5) *
        increment
      return(if (measurement_system == "imperial") rounded else rounded * 2.54)
    }
    native <- if (measurement_system == "metric")
      x else x * 2.54
    rounded <- if (truncate)
      floor(native) else floor(native + 0.5)
    return(if (measurement_system == "metric") rounded else rounded / 2.54)
  }
  if (dimension == "length") {
    if (grepl("ft", operator, fixed = TRUE)) {
      native <- if (measurement_system == "imperial")
        x else x / 0.3048
      rounded <- if (truncate)
        floor(native) else floor(native + 0.5)
      return(if (measurement_system == "imperial") rounded else rounded * 0.3048)
    }
    native <- if (measurement_system == "metric")
      x else x * 0.3048
    increment <- 0.1
    rounded <- if (truncate)
      floor(native / increment) * increment else floor(native / increment + 0.5) * increment
    return(if (measurement_system == "metric") rounded else rounded / 0.3048)
  }
  increment <- if (operator == "nearest_10_board_feet_half_up")
    10 else 1
  if (truncate)
    floor(x / increment) * increment else floor(x / increment + 0.5) * increment
}

.mc_numlog <- function(
  option, evod, merchant_length, maximum_length,
  minimum_length, trim
) {
  number <- trunc(merchant_length / (maximum_length + trim))
  leftover <- merchant_length - (maximum_length + trim) * number
  if (!(number > 0 || leftover >= minimum_length))
    return(0L)
  if (option < 20) {
    if (leftover >= trim + 0.5)
      number <- number + 1
  } else if (option %in% c(21, 22)) {
    threshold <- if (evod == 1L)
      trim + 0.5 else trim + 1
    if (leftover >= threshold)
      number <- number + 1
  } else if (option == 23L) {
    if (leftover >= trim + minimum_length)
      number <- number + 1
  } else if (option == 24L) {
    if (leftover >= (maximum_length + trim) / 4)
      number <- number + 1
  }
  as.integer(min(number, 20))
}

.mc_segmnt <- function(
  option, evod, merchant_length, maximum_length,
  minimum_length, trim, number
) {
  if (!number)
    return(numeric())
  length <- merchant_length - number * trim
  length <- if (evod == 1L)
    trunc(length + 0.5) else trunc((length + 1) / 2) * 2
  length <- min(length, number * maximum_length)
  logs <- rep(0, number)
  if (number == 1L) {
    if (option == 24L) {
      logs[1L] <- if (length < maximum_length * 0.25)
        0 else if (length <= maximum_length * 0.75)
        maximum_length / 2 else maximum_length
    } else if (length >= minimum_length) {
      logs[1L] <- min(length, maximum_length)
    }
    return(logs[logs > 0])
  }
  if (option < 20L) {
    average <- trunc(length / number)
    leftover <- length - average * number
    logs[] <- average
    if (average > trunc(average / 2) * 2) {
      for (i in seq_len(number)) {
        if (number - 2 * i + 1 >= 1) {
          logs[i] <- logs[i] + 1
          logs[number - i + 1L] <- logs[number - i + 1L] - 1
        }
      }
    }
    if (leftover > 0 && number > trunc(number / 2) * 2) {
      for (i in seq_len(number)) {
        if (leftover > 0 && logs[i] > trunc(logs[i] / 2) * 2) {
          logs[i] <- logs[i] + 1
          leftover <- leftover - 1
        }
      }
    }
    iterations <- 0L
    while (leftover > 0 && iterations <= 500L) {
      for (i in seq_len(number)) {
        if (leftover <= 0 || logs[i] >= maximum_length)
          next
        next_log <- if (i == number)
          logs[number] else logs[i + 1L]
        target <- if (logs[i] == logs[number]) {
          i
        } else if (logs[i] > next_log) {
          i + 1L
        } else {
          NA_integer_
        }
        if (!is.na(target)) {
          addition <- if (leftover >= 2)
            2 else 1
          logs[target] <- logs[target] + addition
          leftover <- leftover - addition
        }
      }
      iterations <- iterations + 1L
    }
    return(logs[logs > 0])
  }
  leftover <- length - trunc(maximum_length) * (number - 1L)
  logs[] <- maximum_length
  if (option == 21L) {
    if (leftover >= maximum_length / 2) {
      logs[number] <- leftover
    } else {
      logs[number] <- trunc((maximum_length + leftover) / 2)
      logs[number - 1L] <- maximum_length + leftover - logs[number]
      if (logs[number] == logs[number - 1L] && logs[number] %% 2 == 1) {
        logs[number] <- logs[number] - 1
        logs[number - 1L] <- logs[number - 1L] + 1
      }
    }
  } else if (option == 22L) {
    logs[number] <- trunc((maximum_length + leftover) / 2)
    logs[number - 1L] <- maximum_length + leftover - logs[number]
    if (logs[number] < minimum_length) {
      logs[number] <- 0
      logs[number - 1L] <- maximum_length
    } else if (logs[number] == logs[number - 1L] && logs[number] %% 2 == 1) {
      logs[number] <- logs[number] - 1
      logs[number - 1L] <- logs[number - 1L] + 1
    }
  } else if (option == 23L) {
    logs[number] <- if (leftover >= minimum_length)
      leftover else 0
  } else if (option == 24L) {
    logs[number] <- if (leftover < maximum_length * 0.25)
      0 else if (leftover <= maximum_length * 0.75)
      trunc(maximum_length * 0.5 + 0.5) else maximum_length
  }
  logs[logs > 0]
}

.mc_nvel_segments <- function(length, convention, length_round = NULL) {
  settings <- switch(convention,
    split_20 = list(option = 12L, evod = 1L, maximum = 20),
    allocated_20 = list(
      option = 22L,
      evod = 2L, maximum = 20
    ),
    whole_40 = list(option = 22L, evod = 2L, maximum = 40)
  )
  if (!is.null(length_round))
    settings$evod <- if (length_round == 1) 1L else 2L
  number <- .mc_numlog(settings$option, settings$evod, length, settings$maximum, 2, 0)
  .mc_segmnt(settings$option, settings$evod, length, settings$maximum, 2, 0, number)
}

.mc_scribner <- function(diameter, length, corrected) {
  if (diameter < 1)
    return(0)
  diameter <- min(diameter, 120)
  index <- trunc(diameter)
  if (index > 5 && index <= 11) {
    if (length > 15 && length < 32)
      index <- index + 115
    if (length > 31 && length < 41)
      index <- index + 121
  }
  factor <- .mc_scribner_factor[index]
  if (corrected) {
    volume <- trunc((length * factor + 5) / 10)
    key <- length * 1000 + diameter
    comparable <- trunc(.mc_scribner_exception / 10)
    match <- match(key, comparable)
    if (!is.na(match)) {
      encoded <- .mc_scribner_exception[match]
      volume <- volume + if ((encoded / 2 - trunc(encoded / 2)) > 0)
        1 else -1
    }
    return(volume * 10)
  }
  trunc(length * factor + 0.5)
}

.mc_intl14 <- function(diameter, length) {
  if (diameter < 4)
    return(0)
  segments <- trunc(length / 4)
  fraction <- length / 4 - segments
  volume <- 0
  if (segments > 0)
    for (j in seq_len(segments)) {
      sed <- diameter + (segments - j) / 2
      volume <- volume + (.mc_intl14_constants[["quadratic"]] * sed^2 -
                            .mc_intl14_constants[["linear"]] *
                              sed) * .mc_intl14_constants[["adjustment"]]
    }
  if (fraction > 0) {
    volume <- volume + fraction * (.mc_intl14_constants[["quadratic"]] *
                                     diameter^2 - .mc_intl14_constants[["linear"]] *
                                     diameter) * .mc_intl14_constants[["adjustment"]]
  }
  if (volume < 7.5)
    return(5)
  tens <- trunc(volume / 10)
  remainder <- trunc((volume / 10 - tens) * 100)
  if (remainder < 25)
    tens * 10 else if (remainder >= 75)
    (tens + 1) * 10 else tens * 10 + 5
}
