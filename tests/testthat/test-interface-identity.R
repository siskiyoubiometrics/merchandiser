# Recorded with the installed 0.2.0 namespace before interface edits.
# Regeneration is deliberately separate from ordinary test execution.
# Known and intended difference from 0.2.0: products sold in cubic or cords
# with zero trim now convert gross, located net, and net scaled quantities to
# the requested unit, with corresponding value and summary corrections.
# These recordings use positive trim. test-review-interface.R covers zero trim
# in both input unit systems without changing the recorded identity baseline.
interface_identity_calls <- function(legacy = FALSE) {
  ns <- asNamespace("merchandiser")
  get_data <- function(name) getExportedValue("merchandiser", name)
  trees <- get_data("example_trees")
  regional <- if (legacy) c("pnw_west_trees", "south_loblolly_trees") else
    c("example_trees_pnw", "example_trees_south")
  for (name in regional) {
    tree <- get_data(name)[1, ]
    trees <- rbind(trees, data.frame(
      tree = if (grepl("pnw", name)) 7L else 8L,
      spcd = tree$spcd, species = tree$species, dbh = tree$dbh,
      ht = tree$ht_observed,
      model = if (tree$spcd == 202) "F00FW2W202" else "831CLKE131"
    ))
  }
  board <- c("scribner_decimal_c_whole_40", "scribner_decimal_c_allocated_20",
             "scribner_decimal_c_split_20", "international_1_4_4ft", "doyle_formula")
  definitions <- data.frame(
    sold_by = board, scale_rule = board, measurement_quantity = "board_foot",
    scale_unit = "board_foot", scale_bark_basis = "ib"
  )
  for (rule in c("cubic", "smalian", "huber")) {
    for (unit in c("ft3", "m3")) for (basis in c("ib", "ob")) {
      label <- if (rule == "cubic") sub("3", "", unit) else unit
      definitions <- rbind(definitions, data.frame(
        sold_by = paste(rule, label, basis, sep = "_"), scale_rule = rule,
        measurement_quantity = "cubic", scale_unit = unit, scale_bark_basis = basis
      ))
    }
  }
  for (unit in c("green_short_ton", "green_metric_ton", "cord")) {
    for (basis in if (unit == "cord") c("ib", "ob") else "ib") {
      definitions <- rbind(definitions, data.frame(
        sold_by = if (unit == "cord") paste0("cord_", basis) else unit,
        scale_rule = "cubic", measurement_quantity = if (unit == "cord") "cord" else "green_weight",
        scale_unit = unit, scale_bark_basis = basis
      ))
    }
  }
  extra <- data.frame(
    scale_rule = c("scribner_factor_split_20", "huber"),
    scale_bark_basis = c("ib", "ob")
  )
  answer <- list()
  for (units in c("imperial", "metric")) {
    length_factor <- if (units == "imperial") 1 else 0.3048
    diameter_factor <- if (units == "imperial") 1 else 2.54
    for (i in seq_len(nrow(definitions))) {
      definition <- definitions[i, ]
      for (method in c("merchandise", "optimize_bucking")) {
        fields <- list(
          product = "identity", priority = 1, lengths = c(8, 16) * length_factor,
          trim = 0.5 * length_factor, min_sed = 3 * diameter_factor,
          diameter_basis = "ib", price = if (i %% 2) 125 else NA_real_
        )
        # The mapping table in docs/waves/simplify/identity-mapping.md lists
        # public combinations and the private compatibility cases.
        fields$price_per <- 100
        if (definition$measurement_quantity == "cord") fields$cord_solid_fraction <- 0.5
        projected <- definition
        projected$diameter_basis <- "ib"
        projected$diameter_round <- "rule_default"
        projected <- get(".mc_project_product_frame", ns)(projected)
        mapping <- get(".mc_measurement_map", ns)(units)
        at <- match(get(".mc_measurement_key", ns)(projected),
                    get(".mc_measurement_key", ns)(mapping))
        expanded_fields <- c("scale_rule", "measurement_quantity", "scale_unit", "scale_bark_basis")
        representable <- !is.na(at) && identical(
          unname(unlist(definition[expanded_fields])),
          unname(unlist(mapping[at, expanded_fields]))
        )
        migration <- fields
        migration$lengths <- I(list(migration$lengths))
        migration <- as.data.frame(migration, stringsAsFactors = FALSE)
        legacy_fields <- migration
        legacy_fields[expanded_fields] <- definition[expanded_fields]
        migration$sold_by <- definition$sold_by
        specification <- migration
        args <- list(
          dbh = trees$dbh * diameter_factor, ht = trees$ht * length_factor,
          model = trees$model, products = specification, id = trees$tree, spcd = trees$spcd,
          bark_ratio = get_data("species_reference")$bark_ratio[
            match(trees$spcd, get_data("species_reference")$spcd)
          ],
          units = units, status = TRUE, quiet = TRUE, currency = "USD",
          defects = get("defect", ns)(trees$tree[1], 20 * length_factor,
                                      30 * length_factor, "rot", percent = 15)
        )
        args[[if (legacy) "scaling" else "report_also"]] <- extra
        if (method == "optimize_bucking") args$objective <- "net_cubic_ib"
        # Exercise both public legacy schemas before using the internal oracle path.
        saved_fields <- legacy_fields
        class(saved_fields) <- c("merch_products", "data.frame")
        for (input in list(migration, legacy_fields, saved_fields)) {
          args$products <- input
          deprecations <- get(".merge_deprecations", ns)
          rm(list = ls(deprecations), envir = deprecations)
          if (representable) {
            expect_message(public_result <- do.call(get(method, ns), args), "is deprecated")
          } else {
            expect_error(suppressMessages(do.call(get(method, ns), args)),
                         "Legacy setting .*the product fields cannot express it.*report_also")
          }
        }
        if (representable) {
          args$products <- migration
          result <- suppressMessages(do.call(get(method, ns), args))
          expect_identical(public_result, result)
        } else {
          fields$price_quantity <- fields$price_per
          fields$price_per <- NULL
          args$products <- do.call(get(".mc_legacy_product", ns),
                                   c(fields, as.list(definition[expanded_fields])))
          result <- do.call(get(method, ns), args)
        }
        key <- paste(definition$sold_by, units, method, sep = "/")
        if (!nrow(result$logs) || any(!result$trees$status %in% c(0L, 410L))) {
          print(result$trees[c("id", "model", "status")])
          stop("Invalid identity inputs for ", key)
        }
        answer[[key]] <- list(
          cuts = result$logs[c("id", "log", "product", "start_height",
                               "nominal_end_height", "end_height", "nominal_length")],
          sale = result$logs[c("gross_scale", "located_net_scale", "net_scale", "scale_unit")],
          scales = result$scales, values = result$values
        )
      }
    }
  }
  answer
}

if (identical(Sys.getenv("MERCHANDISER_RECORD_INTERFACE"), "true")) {
  stopifnot(as.character(utils::packageVersion("merchandiser")) == "0.2.0")
  recording <- interface_identity_calls(legacy = TRUE)
  saveRDS(recording, "tests/interface-identity-0.2.0.rds", version = 2)
} else {
  test_that("public migration preserves representable recordings and rejects removed settings", {
    expected <- readRDS(test_path("..", "interface-identity-0.2.0.rds"))
    # Adapt column and procedure labels in memory, without changing recorded numbers.
    previous <- unique(vapply(strsplit(names(expected), "/"), `[`, character(1), 1))
    current <- get(".mc_measurement_definitions", asNamespace("merchandiser"))()$sold_by
    labels <- stats::setNames(current, previous)
    previous_scales <- unique(unlist(lapply(expected, function(x) x$scales$scale_rule)))
    scale_labels <- stats::setNames(previous_scales, previous_scales)
    scale_labels[previous[1:5]] <- current[1:5]
    factor_rule <- previous_scales[startsWith(previous_scales, "scribner_factor")]
    scale_labels[factor_rule] <- "scribner_factor_split_20"
    renames <- c(sale_gross = "gross_scale", sale_located_net = "located_net_scale",
                 sale_net = "net_scale", sale_unit = "scale_unit", is_sale = "is_product")
    for (i in seq_along(expected)) {
      key <- strsplit(names(expected)[i], "/")[[1]]
      names(expected)[i] <- paste(c(labels[[key[1]]], key[-1]), collapse = "/")
      for (table in names(expected[[i]])) {
        frame <- expected[[i]][[table]]
        matched <- names(frame) %in% names(renames)
        names(frame)[matched] <- unname(renames[names(frame)[matched]])
        if ("scale_rule" %in% names(frame)) {
          frame$scale_rule <- unname(scale_labels[frame$scale_rule])
        }
        expected[[i]][[table]] <- frame
      }
    }
    actual <- interface_identity_calls()
    expect_identical(names(actual), names(expected))
    for (name in names(expected)) expect_identical(actual[[name]], expected[[name]], info = name)
  })
}
