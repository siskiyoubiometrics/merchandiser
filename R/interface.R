.mc_interface_renames <- c(
  fallback = "allow_after_higher_product", pulp_product = "accepts_pulp_restriction",
  max_defect_pct = "max_rot_pct", price_quantity = "price_per",
  source_id = "specification_source", grade = "meta_grade",
  min_top_length = "min_boundary_length", continue_to_top = "allow_lower_products",
  max_pieces = "max_logs_per_segment"
)

.mc_interface_message <- function(old, replacement) {
  key <- paste0("interface:", old)
  if (!exists(key, .merge_deprecations, inherits = FALSE)) {
    assign(key, TRUE, .merge_deprecations)
    message(old, " is deprecated. Use ", replacement, ".")
  }
}

.mc_measurement_definitions <- function() {
  board <- c("scribner_decimal_c_whole_40", "scribner_decimal_c_allocated_20",
             "scribner_decimal_c_split_20", "international_1_4_4ft", "doyle_formula")
  answer <- data.frame(
    sold_by = board, scale_rule = board, measurement_quantity = "board_foot",
    scale_unit = "board_foot", scale_bark_basis = "ib"
  )
  for (rule in c("cubic", "smalian", "huber")) {
    for (unit in c("ft3", "m3")) for (basis in c("ib", "ob")) {
      label <- if (rule == "cubic") sub("3", "", unit) else unit
      answer <- rbind(answer, data.frame(
        sold_by = paste(rule, label, basis, sep = "_"), scale_rule = rule,
        measurement_quantity = "cubic", scale_unit = unit, scale_bark_basis = basis
      ))
    }
  }
  for (unit in c("green_short_ton", "green_metric_ton", "cord")) {
    for (basis in if (unit == "cord") c("ib", "ob") else "ib") {
      answer <- rbind(answer, data.frame(
        sold_by = if (unit == "cord") paste0("cord_", basis) else unit,
        scale_rule = "cubic", measurement_quantity = if (unit == "cord") "cord" else "green_weight",
        scale_unit = unit, scale_bark_basis = basis
      ))
    }
  }
  answer
}

.mc_sold_by <- function(x, units) {
  aliases <- c(
    scribner_whole = "scribner_decimal_c_whole_40",
    scribner_allocated = "scribner_decimal_c_allocated_20",
    scribner_split = "scribner_decimal_c_split_20",
    international = "international_1_4_4ft", doyle = "doyle_formula",
    cubic = if (units == "imperial") "cubic_ft_ib" else "cubic_m_ib",
    green_ton = if (units == "imperial") "green_short_ton" else "green_metric_ton",
    cord = "cord_ib"
  )
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x) || anyNA(x)) stop("sold_by must contain procedure names.", call. = FALSE)
  replace <- x %in% names(aliases)
  x[replace] <- unname(aliases[x[replace]])
  index <- match(x, .mc_measurement_definitions()$sold_by)
  if (anyNA(index)) {
    removed <- x[is.na(index) & startsWith(x, "scribner_factor")]
    if (length(removed)) .mc_unrepresentable(paste0("sold_by = ", removed[1]))
    stop("Unknown sold_by value: ", x[is.na(index)][1],
         ". Factor Scribner procedures belong in report_also.", call. = FALSE)
  }
  .mc_measurement_definitions()[index, , drop = FALSE]
}
# Resolve every public measurement combination in one place.
.mc_measurement_map <- function(units = "imperial") {
  grid <- expand.grid(
    volume_unit = c("scribner", "international", "doyle", "cubic", "green_ton", "cord"),
    inside_bark = c(TRUE, FALSE), split_scale = c(FALSE, TRUE),
    round = c("default", "down", "nearest", "none"), stringsAsFactors = FALSE
  )
  grid$scale_rule <- c(
    scribner = "scribner_decimal_c_whole_40", international = "international_1_4_4ft",
    doyle = "doyle_formula", cubic = "cubic", green_ton = "cubic", cord = "cubic"
  )[grid$volume_unit]
  split <- grid$volume_unit == "scribner" & grid$split_scale
  grid$scale_rule[split] <- "scribner_decimal_c_split_20"
  board <- grid$volume_unit %in% c("scribner", "international", "doyle")
  grid$measurement_quantity <- ifelse(board, "board_foot", grid$volume_unit)
  grid$measurement_quantity[grid$volume_unit == "green_ton"] <- "green_weight"
  grid$scale_unit <- ifelse(board, "board_foot", grid$volume_unit)
  grid$scale_unit[grid$volume_unit == "cubic"] <- if (units == "imperial") "ft3" else "m3"
  grid$scale_unit[grid$volume_unit == "green_ton"] <-
    if (units == "imperial") "green_short_ton" else "green_metric_ton"
  grid$diameter_basis <- ifelse(grid$inside_bark, "ib", "ob")
  grid$scale_bark_basis <- ifelse(board | grid$volume_unit == "green_ton", "ib",
                                  grid$diameter_basis)
  grid$diameter_round <- unname(c(default = "rule_default", down = "truncate_1in",
                                  nearest = "nearest_1in_half_up", none = "none")[grid$round])
  # Integrated profile measurements have no scaling diameter to round.
  grid$diameter_round[!board] <- "rule_default"
  grid$length_round <- "rule_default"
  grid$volume_round <- "rule_default"
  grid
}
.mc_measurement_key <- function(x) {
  paste(x$volume_unit, x$inside_bark, x$split_scale, x$round)
}

.mc_expand_products <- function(x, units) {
  if (!is.data.frame(x)) stop("x must be a data frame.", call. = FALSE)
  internal <- isTRUE(attr(x, "calculation_products", exact = TRUE))
  if (internal) return(x)
  policies <- attr(x, "source_policies", exact = TRUE)
  parity <- attr(x, "parity_input", exact = TRUE)
  parity_view <- attr(x, "parity_view", exact = TRUE)
  current <- .mc_plain_product_frame(x)
  normalized <- attr(x, "normalized_products", exact = TRUE)
  x <- .mc_product_raw_input(x)
  if (!is.null(parity)) {
    fields <- c("lengths", "min_length", "max_length", "length_step")
    for (i in seq_len(nrow(x))) {
      j <- match(x$product[i], parity$product)
      if (!is.na(j) && identical(current[i, fields], parity_view[j, fields])) {
        if (!"length_parity" %in% names(x)) x$length_parity <- rep("any", nrow(x))
        for (field in c(fields, "length_parity")) x[[field]][i] <- parity[[field]][j]
      }
    }
  }
  for (old in intersect(names(x), names(.mc_interface_renames))) {
    new <- .mc_interface_renames[[old]]
    if (new %in% names(x)) stop("Supply only ", new, ".", call. = FALSE)
    .mc_interface_message(old, new)
    names(x)[names(x) == old] <- new
  }
  for (old in names(.mc_interface_renames)[1:5]) {
    new <- .mc_interface_renames[[old]]
    names(x)[names(x) == new] <- old
  }
  if ("meta_grade" %in% names(x)) x$grade <- x$meta_grade
  if ("diameter_basis" %in% names(x) &&
        any(c("sold_by", "volume_unit") %in% names(x))) {
    if ("inside_bark" %in% names(x)) {
      stop("Supply only inside_bark, not diameter_basis as well.", call. = FALSE)
    }
    if (is.factor(x$diameter_basis)) x$diameter_basis <- as.character(x$diameter_basis)
    if (!is.character(x$diameter_basis) || anyNA(x$diameter_basis) ||
          any(!x$diameter_basis %in% c("ib", "ob"))) {
      stop("diameter_basis must contain ib or ob.", call. = FALSE)
    }
  }
  if ("sold_by" %in% names(x)) {
    if ("volume_unit" %in% names(x)) stop("Supply only volume_unit.", call. = FALSE)
    .mc_interface_message("sold_by", "volume_unit, inside_bark, and split_scale")
    old <- .mc_sold_by(x$sold_by, units)
    old$diameter_basis <- if ("diameter_basis" %in% names(x)) x$diameter_basis else
      old$scale_bark_basis
    if ("inside_bark" %in% names(x)) {
      old$diameter_basis <- ifelse(.mc_as_logical(x$inside_bark, "inside_bark", FALSE), "ib", "ob")
      x$inside_bark <- NULL
    }
    for (field in intersect(names(x), c("split_scale", "round"))) {
      .mc_unrepresentable(paste0(field, " = ", x[[field]][1]))
    }
    conflicting <- intersect(names(x), c("scale_rule", "measurement_quantity",
                                         "scale_unit", "scale_bark_basis"))
    if (length(conflicting)) stop("Supply sold_by alone for measurement.", call. = FALSE)
    attr(x, "dynamic_units") <- x$product[x$sold_by %in% c("cubic", "green_ton")]
    x[names(old)[names(old) != "sold_by"]] <- old[names(old) != "sold_by"]
    x$sold_by <- NULL
  }
  if ("diameter_basis" %in% names(x) && "volume_unit" %in% names(x)) {
    .mc_interface_message("diameter_basis", "inside_bark")
    if (!"inside_bark" %in% names(x)) x$inside_bark <- x$diameter_basis == "ib"
    x$diameter_basis <- NULL
  }
  if ("volume_unit" %in% names(x)) {
    forbidden <- intersect(names(x), c("scale_rule", "measurement_quantity", "scale_unit",
                                       "scale_bark_basis", "diameter_round",
                                       "length_round", "volume_round"))
    if (length(forbidden)) {
      stop("Use volume_unit, inside_bark, split_scale, and round for products. ",
           "Additional procedures belong in report_also.", call. = FALSE)
    }
    n <- nrow(x)
    if (!"inside_bark" %in% names(x)) x$inside_bark <- rep(TRUE, n)
    if (!"split_scale" %in% names(x)) x$split_scale <- rep(FALSE, n)
    if (!"round" %in% names(x)) x$round <- rep("default", n)
    for (field in c("inside_bark", "split_scale")) {
      x[[field]] <- .mc_as_logical(x[[field]], field, allow_na = FALSE)
    }
    mapping <- .mc_measurement_map(units)
    at <- match(.mc_measurement_key(x), .mc_measurement_key(mapping))
    if (anyNA(at)) {
      stop("Invalid volume_unit or round. See Scaling rules.", call. = FALSE)
    }
    definition <- mapping[at, , drop = FALSE]
    public <- c("volume_unit", "inside_bark", "split_scale", "round")
    attr(x, "measurement_fields") <- x[c("product", public)]
    x[public] <- NULL
    fields <- setdiff(names(mapping), public)
    x[fields] <- definition[fields]
  } else {
    .mc_interface_message("scale_rule", "volume_unit, inside_bark, split_scale, and round")
    if ("scale_rule" %in% names(x)) x$scale_rule[x$scale_rule == "exact_profile"] <- "cubic"
    .mc_check_authored_measurement(x)
  }
  if (!is.null(policies)) {
    index <- match(x$product, names(policies))
    if (!is.null(normalized)) {
      index[is.na(index)] <- match(x$priority[is.na(index)], normalized$priority)
    }
    x$segmentation_policy <- unname(policies[index])
  }
  x
}

.mc_project_product_frame <- function(x) {
  for (old in names(.mc_interface_renames)[1:5]) {
    names(x)[names(x) == old] <- .mc_interface_renames[[old]]
  }
  x$volume_unit <- ifelse(x$measurement_quantity == "green_weight", "green_ton",
                          x$measurement_quantity)
  for (rule in c("scribner", "international", "doyle")) {
    x$volume_unit[startsWith(x$scale_rule, rule)] <- rule
  }
  x$inside_bark <- x$diameter_basis == "ib"
  x$split_scale <- x$scale_rule == "scribner_decimal_c_split_20"
  x$round <- unname(c(rule_default = "default", none = "none", truncate_1in = "down",
                      nearest_1in_half_up = "nearest")[x$diameter_round])
  remembered <- attr(x, "measurement_fields", exact = TRUE)
  if (!is.null(remembered)) {
    at <- match(x$product, remembered$product)
    for (field in c("inside_bark", "split_scale", "round")) x[[field]] <- remembered[[field]][at]
  }
  drop <- c("grade", "segmentation_policy", "length_parity", "scale_rule",
            "measurement_quantity", "scale_unit", "scale_bark_basis", "diameter_basis",
            "diameter_round", "length_round", "volume_round", "sold_by")
  x[setdiff(names(x), drop)]
}

.mc_unrepresentable <- function(setting) {
  stop("Legacy setting ", setting, ": the product fields cannot express it. ",
       "Use the advanced reporting argument report_also for measurement forms ",
       "still available there. ",
       "The retired allocation is unavailable there.", call. = FALSE)
}

.mc_check_authored_measurement <- function(x, units = NULL) {
  if (any(x$segmentation_policy != "generic")) {
    stop("segmentation_policy belongs in products_from_nvel_rules() and recorded settings.",
         call. = FALSE)
  }
  supported <- .mc_measurement_map()$scale_rule
  bad <- which(!x$scale_rule %in% supported)
  if (length(bad)) .mc_unrepresentable(paste0("scale_rule = ", x$scale_rule[bad[1]]))
  allowed <- list(diameter_round = c("rule_default", "none", "truncate_1in",
                                     "nearest_1in_half_up"),
                  length_round = "rule_default", volume_round = "rule_default")
  for (field in intersect(names(allowed), names(x))) {
    bad <- which(!x[[field]] %in% allowed[[field]])
    if (length(bad)) .mc_unrepresentable(paste0(field, " = ", x[[field]][bad[1]]))
  }
  if (all(c("measurement_quantity", "diameter_round") %in% names(x))) {
    bad <- which(x$measurement_quantity != "board_foot" &
                   !x$diameter_round %in% c("rule_default", "none"))
    if (length(bad)) .mc_unrepresentable(paste0("diameter_round = ", x$diameter_round[bad[1]]))
  }
  if (all(c("measurement_quantity", "scale_bark_basis", "diameter_basis") %in% names(x))) {
    basis <- ifelse(x$measurement_quantity %in% c("board_foot", "green_weight"),
                    "ib", x$diameter_basis)
    bad <- which(x$scale_bark_basis != basis)
    if (length(bad)) {
      i <- bad[1]
      .mc_unrepresentable(paste0("scale_bark_basis = ", x$scale_bark_basis[i],
                                 " with diameter_basis = ", x$diameter_basis[i]))
    }
  }
  if (!is.null(units)) {
    expected <- if (units == "imperial") c("ft3", "green_short_ton") else
      c("m3", "green_metric_ton")
    bad <- which(x$scale_unit %in% setdiff(
      c("ft3", "m3", "green_short_ton", "green_metric_ton"), expected
    ))
    if (length(bad)) .mc_unrepresentable(paste0("scale_unit = ", x$scale_unit[bad[1]],
                                                " with units = ", units))
  }
  invisible(x)
}

.mc_publish_products <- function(x) {
  .mc_check_authored_measurement(x)
  policies <- stats::setNames(x$segmentation_policy, x$product)
  raw <- attr(x, "raw_products", exact = TRUE)
  parity <- x$length_parity != "any" & x$segmentation_policy == "generic"
  view <- x
  if (any(parity)) {
    q <- attr(x, "quantum")
    for (i in which(parity)) {
      if (is.null(x$lengths[[i]])) {
        modulus <- if (attr(x, "units") == "imperial") 24 else 10000
        numerator <- if (attr(x, "units") == "imperial") 1 else 127
        target <- if (x$length_parity[i] == "even") 0 else modulus / 2
        minimum <- attr(x, "min_length_tick")[i]
        step <- attr(x, "length_step_tick")[i]
        indices <- which((numerator * (minimum + (0:modulus) * step)) %% modulus == target) - 1L
        if (!length(indices)) stop("Length parity removes every range length.", call. = FALSE)
        view$min_length[i] <- (minimum + indices[1] * step) * q
        period <- if (length(indices) > 1) diff(indices)[1] else modulus
        view$length_step[i] <- step * period * q
      }
      message("length_parity is deprecated. Product ", x$product[i], " became ",
              if (!is.null(view$lengths[[i]])) {
                paste0("lengths = c(", paste(view$lengths[[i]], collapse = ", "), ")")
              } else {
                paste0("min_length = ", view$min_length[i],
                       ", length_step = ", view$length_step[i])
              }, ".")
    }
  }
  answer <- .mc_project_product_frame(view)
  public_raw <- .mc_project_product_frame(raw)
  fields <- c("lengths", "min_length", "max_length", "length_step")
  public_raw[parity, fields] <- answer[parity, fields]
  attr(answer, "raw_products") <- public_raw
  attr(answer, "normalized_products") <- .mc_plain_product_frame(answer)
  attr(answer, "source_policies") <- policies
  attr(answer, "calculation_products") <- NULL
  attr(answer, "measurement_aliases") <- attr(x, "measurement_aliases", exact = TRUE)
  attr(answer, "legacy_units") <- attr(x, "legacy_units", exact = TRUE)
  if (any(parity)) {
    attr(answer, "parity_input") <- raw
    attr(answer, "parity_view") <- .mc_plain_product_frame(answer)
  }
  answer
}

.validate_products_units <- function(x, units, check_units = TRUE) {
  internal <- isTRUE(attr(x, "calculation_products", exact = TRUE))
  constraints <- attr(x, "legacy_units", exact = TRUE)
  if (!is.null(constraints)) {
    at <- match(constraints$product, x$product)
    at[is.na(at)] <- match(constraints$priority[is.na(at)], x$priority)
    constraints$product <- x$product[at]
    constraints$priority <- x$priority[at]
    keep <- !is.na(at) & constraints$volume_unit == x$volume_unit[at]
    constraints <- constraints[keep, , drop = FALSE]
    if (check_units) .mc_check_authored_measurement(constraints, units)
  }
  expanded <- .mc_expand_products(x, units)
  measurement <- attr(expanded, "measurement_fields", exact = TRUE)
  answer <- .mc_validate_products_expanded(expanded, units)
  if (!internal) {
    .mc_check_authored_measurement(answer, if (check_units) units else NULL)
    if (is.null(measurement)) {
      fixed <- answer$measurement_quantity %in% c("cubic", "green_weight") &
        !answer$product %in% attr(expanded, "dynamic_units", exact = TRUE)
      constraints <- data.frame(product = answer$product[fixed],
                                priority = answer$priority[fixed],
                                volume_unit = .mc_project_product_frame(answer)$volume_unit[fixed],
                                scale_unit = answer$scale_unit[fixed])
    }
  }
  attr(answer, "legacy_units") <- constraints
  attr(answer, "calculation_products") <- TRUE
  attr(answer, "measurement_fields") <- measurement
  attr(attr(answer, "raw_products"), "measurement_fields") <- measurement
  answer
}

#' Define a product and its log specifications
#'
#' Describe accepted log dimensions and their measurement unit. Combine rows
#' with [products()] to define the products available within a tree.
#' @param product Required unique, nonempty character product label.
#' @param priority Required positive whole number, unique across the table.
#' Smaller priorities are considered first.
#' @param species Species selection, default `'all'`. Supply inventory species
#' codes, names, or the rules described in [product_schema].
#' @param ... Named specification fields described in [product_schema].
#' Supply `min_sed` and either `lengths` or a length range.
#' @details `volume_unit` is required and selects `'scribner'`, `'international'`,
#' `'doyle'`, `'cubic'`, `'green_ton'`, or `'cord'`. `inside_bark` defaults to
#' `TRUE`, `split_scale` to `FALSE`, and `round` to `'default'`.
#' [Scaling rules][scaling_rules] describes their mapping and additional reporting.
#'
#' Lengths and trim use feet under imperial calls and meters under metric calls.
#' Diameter limits use inches or centimeters. The unit system belongs to the
#' merchandising call. It does not convert supplied measurements.
#'
#' Acceptance uses physical endpoints. Nominal length excludes trim and defines
#' the measured log body. Optional `price` and `price_per` describe a hypothetical
#' or supplied quote in the product's measurement unit.
#'
#' Priced runs require a
#' currency label. An omitted price leaves the product unpriced.
#' @return A checked product row. Field types, defaults, and constraints are
#' described in [product_schema].
#' @section Status and missing values:
#' Invalid specifications stop the call. A product table carries no calculation
#' status and does not guarantee that a log fits the supplied tree.
#' @seealso [products()], [merchandise()], [scaling_rules], [product_schema].
#' @usage
#'
#' ## Call signatures
#' product(product, priority, species = 'all', ...)
#' @examples
#' ## Define a sawlog for the shipped species
#' product(product = 'saw',
#'         priority = 1,
#'         species = example_trees$spcd,
#'         lengths = c(16, 32),
#'         trim = 0.5,
#'         min_sed = 6,
#'         volume_unit = 'scribner',
#'         inside_bark = TRUE,
#'         round = 'down')
#' @export
product <- function(product, priority, species = "all", ...) {
  dots <- list(...)
  lengths <- dots$lengths
  dots$lengths <- NULL
  frame <- as.data.frame(c(list(product = product, priority = priority,
                                species = .mc_product_species_rule(species)), dots),
                         stringsAsFactors = FALSE, optional = TRUE)
  frame$lengths <- I(list(lengths))
  answer <- validate_products(frame)
  answer
}

.mc_report_also <- function(report_also, scaling, units) {
  if (!is.null(scaling)) {
    if (!is.null(report_also)) stop("Supply report_also or scaling, not both.", call. = FALSE)
    .mc_interface_message("scaling", "report_also")
    report_also <- scaling
  }
  if (is.null(report_also)) return(NULL)
  rules <- if (is.character(report_also)) {
    report_also
  } else if (is.data.frame(report_also)) {
    unlist(report_also[intersect(c("sold_by", "scale_rule"), names(report_also))])
  } else {
    character()
  }
  if (any(grepl("allocated", rules))) {
    stop("This retired allocation is not a reporting procedure.", call. = FALSE)
  }
  if (is.data.frame(report_also) && !"name" %in% names(report_also) &&
        !"sold_by" %in% names(report_also)) return(.mc_validate_scaling(report_also))
  if (is.character(report_also)) {
    report_also <- data.frame(name = report_also, sold_by = report_also)
  }
  allowed <- c("name", "sold_by", "scale_rule", "measurement_quantity", "scale_unit",
               "scale_bark_basis", "diameter_round", "length_round", "volume_round",
               "cord_solid_fraction")
  if (is.data.frame(report_also) && any(!names(report_also) %in% allowed)) {
    stop("Unknown report_also column.", call. = FALSE)
  }
  if (is.data.frame(report_also) && "sold_by" %in% names(report_also) &&
        any(c("scale_rule", "measurement_quantity", "scale_unit", "scale_bark_basis") %in%
              names(report_also))) {
    stop("Supply sold_by alone for reporting measurement.", call. = FALSE)
  }
  if (!is.data.frame(report_also) || !"name" %in% names(report_also) ||
        anyNA(report_also$name) || any(!nzchar(report_also$name)) ||
        anyDuplicated(report_also$name)) {
    stop("Named report_also definitions require distinct nonempty name values.", call. = FALSE)
  }
  definitions <- lapply(seq_len(nrow(report_also)), function(i) {
    row <- report_also[i, , drop = FALSE]
    if ("sold_by" %in% names(row)) {
      if (row$sold_by %in% .mc_scale_rules && row$sold_by != "cubic") {
        row$scale_rule <- row$sold_by
        row$scale_bark_basis <- "ib"
        row$measurement_quantity <- if (row$sold_by %in% c("smalian", "huber")) "cubic" else
          "board_foot"
        row$scale_unit <- if (row$measurement_quantity == "cubic") {
          if (units == "imperial") "ft3" else "m3"
        } else {
          "board_foot"
        }
      } else {
        definition <- .mc_sold_by(row$sold_by, units)
        row[names(definition)] <- definition
      }
    }
    fields <- c("scale_rule", "scale_bark_basis", "diameter_round", "length_round", "volume_round")
    normalized <- .mc_validate_scaling(row[intersect(fields, names(row))])
    row[names(normalized)] <- normalized
    if (!"measurement_quantity" %in% names(row)) {
      board <- grepl("^(scribner|international|doyle)", row$scale_rule)
      row$measurement_quantity <- if (board) "board_foot" else "cubic"
      row$scale_unit <- if (board) "board_foot" else if (units == "imperial") "ft3" else "m3"
    }
    if (!"cord_solid_fraction" %in% names(row)) row$cord_solid_fraction <- NA_real_
    invalid_fraction <- is.na(row$cord_solid_fraction) ||
      row$cord_solid_fraction <= 0 || row$cord_solid_fraction >= 1
    if (row$measurement_quantity == "cord" && invalid_fraction) {
      stop("Cord reporting requires cord_solid_fraction strictly between zero and one.",
           call. = FALSE)
    }
    do.call(.mc_legacy_product, c(list(
      product = as.character(row$name), priority = 1L, lengths = 1,
      min_sed = 0, diameter_basis = "ib"
    ), as.list(row[setdiff(names(row), c("name", "sold_by"))])))
    row[c("name", "scale_rule", "scale_bark_basis", "measurement_quantity", "scale_unit",
          "diameter_round", "length_round", "volume_round", "cord_solid_fraction")]
  })
  definitions <- do.call(rbind, definitions)
  queries <- unique(definitions[c("scale_rule", "scale_bark_basis")])
  queries <- .mc_validate_scaling(queries)
  attr(queries, "report_definitions") <- definitions
  queries
}

.mc_scale_pieces <- function(logs, call, profile) {
  definitions <- attr(call$scaling, "report_definitions", exact = TRUE)
  if (is.null(definitions)) return(.mc_scale_pieces_calculation(logs, call, profile))
  sale_call <- call
  sale_call$scaling <- NULL
  answer <- .mc_scale_pieces_calculation(logs, sale_call, profile)
  scales <- attr(answer, "scales")
  scales$name <- "product"
  for (i in seq_len(nrow(definitions))) {
    definition <- definitions[i, ]
    report_call <- sale_call
    fields <- setdiff(names(definition), "name")
    for (field in fields) report_call$products[[field]][] <- definition[[field]]
    report_logs <- logs
    report_logs$native_gross_scale <- NULL
    extra <- .mc_scale_pieces_calculation(report_logs, report_call, profile)
    status <- attr(answer, "scale_status")
    extra_status <- attr(extra, "scale_status")
    status[status == 0L] <- extra_status[status == 0L]
    attr(answer, "scale_status") <- status
    extra_scales <- attr(extra, "scales")
    extra_scales$is_product <- FALSE
    extra_scales$name <- definition$name
    scales <- rbind(scales, extra_scales)
  }
  rownames(scales) <- NULL
  attr(answer, "scales") <- scales
  answer
}

#' Use the current example product names
#'
#' Use [example_products()] to load a shipped product table and
#' [example_product_names()] to list available tables. These older names call
#' the replacements and return the same objects. Update new analyses to use the
#' current names.
#' @keywords internal
#' @param name One required character name, `'pnw'`, `'us_south'`, or `'douglas_fir'`,
#'   unitless. No default. Missing or unknown names are errors.
#'
#' Example: `'pnw'`.
#' @details A deprecation message names the replacement once per session.
#' @return The product table described in [Product specification schema][product_schema], or the
#'   example-name
#'   table described by [example_product_names()].
#' @section Status and missing values:
#' These aliases add no status codes. Invalid names stop the call. Missing
#' optional product bounds retain the meanings in [Product specification schema][product_schema].
#' @seealso [example_products()] for current loading and [example_product_names()]
#'   for the available names and geographic defaults.
#' @usage
#'
#' ## Call signatures
#' product_preset(name)
#'
#' product_presets()
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the example product measurements
#' example_products(name = 'pnw') %>%
#'   select(product, priority, volume_unit, inside_bark)
#' @export
product_preset <- function(name) {
  .mc_interface_message("product_preset()", "example_products()")
  example_products(name)
}

#' @rdname product_preset
#' @export
#' @usage NULL
product_presets <- function() {
  .mc_interface_message("product_presets()", "example_product_names()")
  example_product_names()
}
