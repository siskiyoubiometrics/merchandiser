.mc_validate_scaling <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.null(attr(x, "report_definitions", exact = TRUE))) return(x)
  if (!is.data.frame(x)) {
    stop(
      "scaling must be NULL or a data frame. ",
      "Supply scale_rule and scale_bark_basis columns.", call. = FALSE
    )
  }
  required <- c("scale_rule", "scale_bark_basis")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing scaling column: ", missing[[1L]],
         ". Add the required column and try again.", call. = FALSE)
  }
  allowed <- c(required, "diameter_round", "length_round", "volume_round")
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) {
    stop("Unknown scaling column: ", unknown[[1L]],
         ". Remove it or rename it to a documented scaling field.",
         call. = FALSE)
  }
  n <- nrow(x)
  for (name in setdiff(allowed, names(x))) x[[name]] <- rep("rule_default", n)
  x[] <- lapply(x, function(value) {
    if (is.factor(value)) as.character(value) else value
  })
  if (!all(vapply(x, is.character, logical(1L)))) {
    stop(
      "Every scaling column must be text. ",
      "Convert the scale settings to character values and try again.",
      call. = FALSE
    )
  }
  if (anyNA(x) || any(!x$scale_rule %in% .mc_scale_rules) ||
        any(!x$scale_bark_basis %in% c("ib", "ob"))) {
    stop(
      "scaling contains an invalid rule or bark basis. ",
      "Choose a registered rule and ib or ob.", call. = FALSE
    )
  }
  board <- grepl("^(scribner|international|doyle)", x$scale_rule)
  if (any(board & x$scale_bark_basis != "ib")) {
    stop(
      "Board-foot scaling requires inside bark. ",
      "Set scale_bark_basis to ib and try again.", call. = FALSE
    )
  }
  for (name in c("diameter_round", "length_round", "volume_round")) {
    if (any(!x[[name]] %in% .mc_rounding_operators)) {
      stop(
        name, " contains an unsupported operator. ",
        "Choose a documented rounding value and try again.", call. = FALSE
      )
    }
  }
  diameter_ops <- c(
    "rule_default", "none", "truncate_1in", "nearest_1in_half_up",
    "nearest_0.5in_half_up", "truncate_1cm", "nearest_1cm_half_up"
  )
  length_ops <- c(
    "rule_default", "none", "truncate_1ft", "nearest_1ft_half_up",
    "truncate_0.1m", "nearest_0.1m_half_up"
  )
  volume_ops <- c(
    "rule_default", "none", "truncate_board_foot",
    "nearest_board_foot_half_up", "nearest_10_board_feet_half_up"
  )
  if (any(!x$diameter_round %in% diameter_ops) ||
        any(!x$length_round %in% length_ops) ||
        any(!x$volume_round %in% volume_ops)) {
    stop(
      "A rounding operator was supplied in the wrong dimension column. ",
      "Move it to the matching dimension field.", call. = FALSE
    )
  }
  if (any(!board & !x$volume_round %in% c("rule_default", "none"))) {
    stop(
      "Board-foot volume rounding is incompatible with a cubic scale. ",
      "Use none or rule_default for cubic volume.", call. = FALSE
    )
  }
  exact <- x$scale_rule == "cubic"
  if (any(exact & (!x$diameter_round %in% c("rule_default", "none") |
                     !x$length_round %in% c("rule_default", "none")))) {
    stop(
      "cubic does not accept dimension rounding. ",
      "Use none or rule_default for diameter and length.", call. = FALSE
    )
  }
  nvel <- grepl("^(scribner|international)", x$scale_rule)
  if (any(nvel & (x$length_round != "rule_default" |
                    x$volume_round != "rule_default"))) {
    stop(
      "nvel_internal rounding is not overrideable. ",
      "Leave every rounding field at rule_default.", call. = FALSE
    )
  }
  exact <- !duplicated(x)
  x <- x[exact, allowed, drop = FALSE]
  key <- paste(x$scale_rule, x$scale_bark_basis, sep = "\034")
  if (anyDuplicated(key)) {
    stop(
      "Conflicting scaling rows share one rule and bark basis. ",
      "Keep one setting for each rule and basis pair.", call. = FALSE
    )
  }
  rownames(x) <- NULL
  x
}

.mc_round_dimension <- function(x, operator, dimension, units) {
  if (operator %in% c("rule_default", "none")) return(x)
  truncate <- startsWith(operator, "truncate")
  if (dimension == "diameter") {
    if (grepl("in", operator, fixed = TRUE)) {
      native <- if (units == "imperial") x else x / 2.54
      increment <- if (grepl("0.5in", operator, fixed = TRUE)) 0.5 else 1
      rounded <- if (truncate) floor(native / increment) * increment else
        floor(native / increment + 0.5) * increment
      return(if (units == "imperial") rounded else rounded * 2.54)
    }
    native <- if (units == "metric") x else x * 2.54
    rounded <- if (truncate) floor(native) else floor(native + 0.5)
    return(if (units == "metric") rounded else rounded / 2.54)
  }
  if (dimension == "length") {
    if (grepl("ft", operator, fixed = TRUE)) {
      native <- if (units == "imperial") x else x / 0.3048
      rounded <- if (truncate) floor(native) else floor(native + 0.5)
      return(if (units == "imperial") rounded else rounded * 0.3048)
    }
    native <- if (units == "metric") x else x * 0.3048
    increment <- 0.1
    rounded <- if (truncate) floor(native / increment) * increment else
      floor(native / increment + 0.5) * increment
    return(if (units == "metric") rounded else rounded / 0.3048)
  }
  increment <- if (operator == "nearest_10_board_feet_half_up") 10 else 1
  if (truncate) floor(x / increment) * increment else floor(x / increment + 0.5) * increment
}

.mc_numlog <- function(option, evod, merchant_length, maximum_length,
                       minimum_length, trim) {
  number <- trunc(merchant_length / (maximum_length + trim))
  leftover <- merchant_length - (maximum_length + trim) * number
  if (!(number > 0 || leftover >= minimum_length)) return(0L)
  if (option < 20) {
    if (leftover >= trim + 0.5) number <- number + 1
  } else if (option %in% c(21, 22)) {
    threshold <- if (evod == 1L) trim + 0.5 else trim + 1
    if (leftover >= threshold) number <- number + 1
  } else if (option == 23L) {
    if (leftover >= trim + minimum_length) number <- number + 1
  } else if (option == 24L) {
    if (leftover >= (maximum_length + trim) / 4) number <- number + 1
  }
  as.integer(min(number, 20))
}

.mc_segmnt <- function(option, evod, merchant_length, maximum_length,
                       minimum_length, trim, number) {
  if (!number) return(numeric())
  length <- merchant_length - number * trim
  length <- if (evod == 1L) trunc(length + 0.5) else trunc((length + 1) / 2) * 2
  length <- min(length, number * maximum_length)
  logs <- rep(0, number)
  if (number == 1L) {
    if (option == 24L) {
      logs[1L] <- if (length < maximum_length * 0.25) 0 else
        if (length <= maximum_length * 0.75) maximum_length / 2 else maximum_length
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
        if (leftover <= 0 || logs[i] >= maximum_length) next
        next_log <- if (i == number) logs[number] else logs[i + 1L]
        target <- if (logs[i] == logs[number]) {
          i
        } else if (logs[i] > next_log) {
          i + 1L
        } else {
          NA_integer_
        }
        if (!is.na(target)) {
          addition <- if (leftover >= 2) 2 else 1
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
    logs[number] <- if (leftover >= minimum_length) leftover else 0
  } else if (option == 24L) {
    logs[number] <- if (leftover < maximum_length * 0.25) 0 else
      if (leftover <= maximum_length * 0.75) trunc(maximum_length * 0.5 + 0.5) else
        maximum_length
  }
  logs[logs > 0]
}

.mc_nvel_segments <- function(length, convention) {
  settings <- switch(convention,
    split_20 = list(option = 12L, evod = 1L, maximum = 20),
    allocated_20 = list(option = 22L, evod = 2L, maximum = 20),
    whole_40 = list(option = 22L, evod = 2L, maximum = 40)
  )
  number <- .mc_numlog(
    settings$option, settings$evod, length, settings$maximum, 2, 0
  )
  .mc_segmnt(
    settings$option, settings$evod, length, settings$maximum, 2, 0, number
  )
}

.mc_scribner <- function(diameter, length, corrected) {
  if (diameter < 1) return(0)
  diameter <- min(diameter, 120)
  index <- trunc(diameter)
  if (index > 5 && index <= 11) {
    if (length > 15 && length < 32) index <- index + 115
    if (length > 31 && length < 41) index <- index + 121
  }
  factor <- .mc_scribner_factor[index]
  if (corrected) {
    volume <- trunc((length * factor + 5) / 10)
    key <- length * 1000 + diameter
    comparable <- trunc(.mc_scribner_exception / 10)
    match <- match(key, comparable)
    if (!is.na(match)) {
      encoded <- .mc_scribner_exception[match]
      volume <- volume + if ((encoded / 2 - trunc(encoded / 2)) > 0) 1 else -1
    }
    return(volume * 10)
  }
  trunc(length * factor + 0.5)
}

.mc_intl14 <- function(diameter, length) {
  if (diameter < 4) return(0)
  segments <- trunc(length / 4)
  fraction <- length / 4 - segments
  volume <- 0
  if (segments > 0) for (j in seq_len(segments)) {
    sed <- diameter + (segments - j) / 2
    volume <- volume +
      (.mc_intl14_constants[["quadratic"]] * sed^2 -
         .mc_intl14_constants[["linear"]] * sed) *
        .mc_intl14_constants[["adjustment"]]
  }
  if (fraction > 0) {
    volume <- volume + fraction *
      (.mc_intl14_constants[["quadratic"]] * diameter^2 -
         .mc_intl14_constants[["linear"]] * diameter) *
      .mc_intl14_constants[["adjustment"]]
  }
  if (volume < 7.5) return(5)
  tens <- trunc(volume / 10)
  remainder <- trunc((volume / 10 - tens) * 100)
  if (remainder < 25) tens * 10 else if (remainder >= 75) (tens + 1) * 10 else tens * 10 + 5
}

.mc_scale_specs <- function(logs, call) {
  product <- logs$product_index
  sale <- data.frame(
    log_row = seq_len(nrow(logs)),
    scale_rule = call$products$scale_rule[product],
    bark_basis = call$products$scale_bark_basis[product],
    diameter_round = call$products$diameter_round[product],
    length_round = call$products$length_round[product],
    volume_round = call$products$volume_round[product],
    is_product = TRUE, stringsAsFactors = FALSE
  )
  extra <- .mc_validate_scaling(call$scaling)
  if (is.null(extra) || !nrow(extra)) return(sale)
  rows <- lapply(seq_len(nrow(logs)), function(log) {
    candidate <- extra
    duplicate <- candidate$scale_rule == sale$scale_rule[log] &
      candidate$scale_bark_basis == sale$bark_basis[log]
    candidate <- candidate[!duplicate, , drop = FALSE]
    if (!nrow(candidate)) return(NULL)
    candidate$log_row <- log
    candidate$is_product <- FALSE
    candidate[c("log_row", "scale_rule", "scale_bark_basis",
                "diameter_round", "length_round", "volume_round", "is_product")]
  })
  extra_rows <- .mc_bind_rows(rows)
  if (is.null(extra_rows)) return(sale)
  names(extra_rows)[names(extra_rows) == "scale_bark_basis"] <- "bark_basis"
  rbind(sale, extra_rows)
}

.mc_scale_query_heights <- function(specs, logs, units) {
  queries <- vector("list", nrow(specs))
  segment_lengths <- vector("list", nrow(specs))
  for (row in seq_len(nrow(specs))) {
    log <- specs$log_row[row]
    rule <- specs$scale_rule[row]
    a <- logs$start_height[log]
    length <- logs$nominal_length[log]
    n <- logs$nominal_end_height[log]
    points <- c(a, n)
    if (rule == "huber") points <- c(a + length / 2)
    if (startsWith(rule, "scribner")) {
      convention <- sub("^scribner_(decimal_c|factor)_", "", rule)
      length_ft <- if (units == "imperial") length else length / 0.3048
      segments <- .mc_nvel_segments(length_ft, convention)
      segment_lengths[[row]] <- segments
      positions <- cumsum(segments) * if (units == "imperial") 1 else 0.3048
      points <- a + positions
    }
    queries[[row]] <- sort(unique(points))
  }
  list(queries = queries, segment_lengths = segment_lengths)
}

.mc_candidate_scale_points <- function(call, product, start, length) {
  nominal_end <- start + length
  rules <- call$products$scale_rule[product]
  if (!is.null(call$scaling) && nrow(call$scaling)) {
    rules <- unique(c(rules, call$scaling$scale_rule))
  }
  points <- c(start, nominal_end, nominal_end + call$products$trim[product])
  if ("huber" %in% rules) points <- c(points, start + length / 2)
  scribner <- rules[startsWith(rules, "scribner")]
  for (rule in scribner) {
    convention <- sub("^scribner_(decimal_c|factor)_", "", rule)
    length_ft <- if (call$units == "imperial") length else length / 0.3048
    segments <- .mc_nvel_segments(length_ft, convention)
    multiplier <- if (call$units == "imperial") 1 else 0.3048
    points <- c(points, start + cumsum(segments) * multiplier)
  }
  sort(unique(points))
}

.mc_convert_cubic <- function(value, from_units, unit) {
  if ((from_units == "imperial" && unit == "ft3") ||
        (from_units == "metric" && unit == "m3")) return(value)
  if (unit == "m3") value * 0.028316846592 else value / 0.028316846592
}

.mc_scale_pieces_calculation <- function(logs, call, profile) {
  specs <- .mc_scale_specs(logs, call)
  value_at <- function(tree, height, column) {
    .mc_profile_lookup(profile, tree, height, column)
  }
  zero_trim <- all(call$products$trim[unique(logs$product_index)] == 0)
  if (zero_trim) {
    logs$trim_cubic_ib <- 0
  } else {
    logs$trim_cubic_ib <- vapply(seq_len(nrow(logs)), function(log) {
      tree <- logs$tree[log]
      value_at(tree, logs$end_height[log], "cum_ib") -
        value_at(tree, logs$nominal_end_height[log], "cum_ib")
    }, numeric(1L))
  }
  specs$gross <- NA_real_
  specs$unit <- NA_character_
  native_sale <- specs$is_product &
    call$products$measurement_quantity[
      logs$product_index[specs$log_row]
    ] != "green_weight"
  if ("native_gross_scale" %in% names(logs) && any(native_sale)) {
    native_piece <- specs$log_row[native_sale]
    native_product <- logs$product_index[native_piece]
    specs$gross[native_sale] <- logs$native_gross_scale[native_piece]
    specs$unit[native_sale] <- call$products$scale_unit[native_product]
  } else {
    native_sale[] <- FALSE
  }
  fast_exact <- zero_trim && all(specs$scale_rule == "cubic")
  if (fast_exact) {
    log_rows <- specs$log_row
    specs$gross <- ifelse(
      specs$bark_basis == "ib",
      logs$log_gross_cubic_ib[log_rows],
      logs$log_gross_cubic_ob[log_rows]
    )
    specs$unit <- if (call$units == "imperial") "ft3" else "m3"
    native_sale[] <- FALSE
  } else {
    query_plan <- .mc_scale_query_heights(specs, logs, call$units)
    for (row in which(!native_sale)) {
      log <- specs$log_row[row]
      tree <- logs$tree[log]
      rule <- specs$scale_rule[row]
      basis <- specs$bark_basis[row]
      length <- .mc_round_dimension(
        logs$nominal_length[log], specs$length_round[row], "length",
        call$units
      )
      a <- logs$start_height[log]
      n <- a + logs$nominal_length[log]
      cum <- if (basis == "ib") "cum_ib" else "cum_ob"
      diameter <- if (basis == "ib") "dib" else "dob"
      cubic <- value_at(tree, n, cum) - value_at(tree, a, cum)
      if (rule == "cubic") {
        gross <- cubic
        unit <- if (call$units == "imperial") "ft3" else "m3"
      } else if (rule == "smalian") {
        led <- .mc_round_dimension(
          value_at(tree, a, diameter), specs$diameter_round[row],
          "diameter", call$units
        )
        sed <- .mc_round_dimension(
          value_at(tree, n, diameter), specs$diameter_round[row],
          "diameter", call$units
        )
        area <- function(d) {
          if (call$units == "imperial") {
            pi * (d / 12)^2 / 4
          } else {
            pi * (d / 100)^2 / 4
          }
        }
        gross <- length * (area(led) + area(sed)) / 2
        unit <- if (call$units == "imperial") "ft3" else "m3"
      } else if (rule == "huber") {
        midpoint <- a + logs$nominal_length[log] / 2
        middle <- .mc_round_dimension(
          value_at(tree, midpoint, diameter), specs$diameter_round[row],
          "diameter", call$units
        )
        area <- if (call$units == "imperial") pi * (middle / 12)^2 / 4 else
          pi * (middle / 100)^2 / 4
        gross <- length * area
        unit <- if (call$units == "imperial") "ft3" else "m3"
      } else if (rule == "doyle_formula") {
        sed <- .mc_round_dimension(
          value_at(tree, n, "dib"), specs$diameter_round[row],
          "diameter", call$units
        )
        sed_in <- if (call$units == "imperial") sed else sed / 2.54
        length_ft <- if (call$units == "imperial") length else length / 0.3048
        gross <- max(sed_in - 4, 0)^2 * length_ft / 16
        unit <- "board_foot"
      } else if (rule == "international_1_4_4ft") {
        sed <- .mc_round_dimension(value_at(tree, n, "dib"),
                                   specs$diameter_round[row], "diameter", call$units)
        sed_in <- floor(
          (if (call$units == "imperial") sed else sed / 2.54) + 0.5
        )
        length_ft <- if (call$units == "imperial") {
          logs$nominal_length[log]
        } else {
          logs$nominal_length[log] / 0.3048
        }
        gross <- .mc_intl14(sed_in, length_ft)
        unit <- "board_foot"
      } else {
        corrected <- startsWith(rule, "scribner_decimal_c")
        segments <- query_plan$segment_lengths[[row]]
        positions <- cumsum(segments) * if (call$units == "imperial") {
          1
        } else {
          0.3048
        }
        diameters <- vapply(a + positions, function(height) {
          .mc_round_dimension(value_at(tree, height, "dib"),
                              specs$diameter_round[row], "diameter", call$units)
        }, numeric(1L))
        diameters <- floor(
          (if (call$units == "imperial") diameters else diameters / 2.54) +
            0.5
        )
        gross <- sum(mapply(
          .mc_scribner, diameters, segments,
          MoreArgs = list(corrected = corrected)
        ))
        unit <- "board_foot"
      }
      specs$gross[row] <- .mc_round_dimension(
        gross, specs$volume_round[row], "volume", call$units
      )
      specs$unit[row] <- unit
    }
  }
  product <- logs$product_index
  sale_rows <- which(specs$is_product)
  sale_piece <- specs$log_row[sale_rows]
  sale_product <- product[sale_piece]
  quantity <- call$products$measurement_quantity[sale_product]
  cubic <- sale_rows[quantity == "cubic"]
  if (length(cubic)) {
    cubic_product <- product[specs$log_row[cubic]]
    target_unit <- call$products$scale_unit[cubic_product]
    convert <- !native_sale[cubic] & (
      (call$units == "imperial" & target_unit == "m3") |
        (call$units == "metric" & target_unit == "ft3")
    )
    if (any(convert)) {
      factor <- if (call$units == "imperial") {
        0.028316846592
      } else {
        1 / 0.028316846592
      }
      specs$gross[cubic[convert]] <- specs$gross[cubic[convert]] * factor
    }
    specs$unit[cubic] <- target_unit
  }
  cord <- sale_rows[quantity == "cord"]
  if (length(cord)) {
    convert <- cord[!native_sale[cord]]
    if (length(convert)) {
      cord_product <- product[specs$log_row[convert]]
      solid_ft3 <- specs$gross[convert]
      if (call$units == "metric") solid_ft3 <- solid_ft3 / 0.028316846592
      specs$gross[convert] <- solid_ft3 /
        (128 * call$products$cord_solid_fraction[cord_product])
    }
    specs$unit[cord] <- "cord"
  }
  weight_rows <- sale_rows[
    call$products$measurement_quantity[product[specs$log_row[sale_rows]]] ==
      "green_weight"
  ]
  scale_status <- integer(call$size)
  if (length(weight_rows)) {
    if (is.null(call$spcd)) {
      specs$gross[weight_rows] <- NA_real_
      scale_status[unique(logs$tree[specs$log_row[weight_rows]])] <- 411L
    } else {
      log_rows <- specs$log_row[weight_rows]
      weight <- mc_provider_green_weight(
        specs$gross[weight_rows], as.integer(call$spcd[logs$tree[log_rows]]),
        ifelse(specs$bark_basis[weight_rows] == "ob", 1L, 0L),
        if (call$units == "imperial") 1L else 2L
      )
      unavailable <- !weight$status %in% c(0L, 52L, 102L)
      specs$gross[weight_rows] <- weight$value / ifelse(
        call$products$scale_unit[product[log_rows]] == "green_short_ton",
        if (call$units == "imperial") 2000 else 2000 * 0.45359237,
        if (call$units == "metric") 1000 else 1000 / 0.45359237
      )
      specs$unit[weight_rows] <- call$products$scale_unit[product[log_rows]]
      if (any(unavailable)) {
        specs$gross[weight_rows[unavailable]] <- NA_real_
        for (failed in which(unavailable)) {
          tree <- logs$tree[log_rows[failed]]
          if (scale_status[tree] == 0L) {
            scale_status[tree] <- if (weight$status[failed] == 1L) {
              411L
            } else {
              weight$status[failed]
            }
          }
        }
      }
    }
  }
  ratio <- logs$located_deduction_cubic_ib / logs$log_gross_cubic_ib
  ratio[!is.finite(ratio)] <- 0
  specs$located_net <- specs$gross * (1 - ratio[specs$log_row])
  specs$net <- specs$located_net
  specs$located_deduction <- specs$gross - specs$located_net
  specs$posthoc_deduction <- 0
  specs$id <- logs$id[specs$log_row]
  specs$log <- logs$log[specs$log_row]
  specs$product <- logs$product[specs$log_row]
  scales <- specs[c(
    "id", "log", "product", "scale_rule", "unit", "bark_basis",
    "gross", "located_net", "located_deduction", "posthoc_deduction",
    "net", "is_product"
  )]
  sale_specs <- specs[sale_rows, , drop = FALSE]
  sale_specs <- sale_specs[match(seq_len(nrow(logs)), sale_specs$log_row), , drop = FALSE]
  logs$scale_rule <- sale_specs$scale_rule
  logs$measurement_quantity <- call$products$measurement_quantity[product]
  logs$scale_unit <- call$products$scale_unit[product]
  logs$scale_bark_basis <- sale_specs$bark_basis
  logs$volume_unit <- .mc_project_product_frame(call$products)$volume_unit[product]
  logs$gross_scale <- sale_specs$gross
  logs$located_net_scale <- sale_specs$located_net
  logs$net_scale <- sale_specs$net
  logs$native_gross_scale <- NULL
  attr(logs, "scales") <- scales
  attr(logs, "scale_status") <- scale_status
  logs
}

.mc_values <- function(logs, call) {
  priced <- nrow(logs) && any(!is.na(call$products$price))
  if (!priced) return(data.frame(
    id = .mc_empty_id(call$id), log = integer(), product = character(),
    currency = character(), gross_value = double(), located_net_value = double(),
    located_deduction_value = double(), posthoc_deduction_value = double(),
    net_value = double(), stringsAsFactors = FALSE
  ))
  price <- call$products$price[logs$product_index]
  unpriced <- is.na(price)
  price[unpriced] <- 0
  quantity <- call$products$price_quantity[logs$product_index]
  result <- data.frame(
    id = logs$id, log = logs$log, product = logs$product,
    currency = call$currency, gross_value = logs$gross_scale * price / quantity,
    located_net_value = logs$located_net_scale * price / quantity,
    net_value = logs$net_scale * price / quantity,
    stringsAsFactors = FALSE
  )
  result$located_deduction_value <-
    result$gross_value - result$located_net_value
  result$posthoc_deduction_value <- 0
  value_columns <- c(
    "gross_value", "located_net_value", "located_deduction_value",
    "posthoc_deduction_value", "net_value"
  )
  result[unpriced, value_columns] <- 0
  result[c(
    "id", "log", "product", "currency", "gross_value",
    "located_net_value", "located_deduction_value",
    "posthoc_deduction_value", "net_value"
  )]
}
