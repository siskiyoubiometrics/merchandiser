interface_recording <- readRDS(test_path("..", "interface-identity-0.4.0.rds"))

test_that("cascade cuts and unaffected scales retain their recorded values", {
  trees <- interface_recording$trees
  for (key in names(interface_recording$cascade)) {
    parts <- strsplit(key, "/", fixed = TRUE)[[1]]
    specification <- product(
      product = "log", min_length = 16, max_length = 32,
      min_sed = 3,
      trim = as.numeric(
        parts[
          2
        ]
      ),
      volume_unit = parts[
        1
      ], cord_solid_fraction = if (parts[1] ==
                                     "cord")
        0.5 else NA_real_, price = if (parts[3] == "TRUE")
        125 else NA_real_, price_per = 100
    )
    result <- merchandise(
      tree_id = trees$tree, dbh = trees$dbh, ht = trees$ht,
      spcd = trees$spcd,
      model = trees$model, products = specification, quiet = TRUE
    )
    previous <- interface_recording$cascade[[key]]$logs
    expect_identical(result$logs$start_height, previous$start_height, info = key)
    expect_identical(result$logs$end_height, previous$end_height, info = key)
    expect_identical(result$logs$length, previous$nominal_length, info = key)
    expect_identical(result$logs$sed, previous$sed_ib, info = key)
    expect_identical(result$logs$led, previous$led_ib, info = key)
    expect_identical(result$logs$scaling_length, floor(previous$nominal_length),
      info = key
    )
    changed <-
      parts[1] %in% c("scribner", "international", "doyle") & previous$nominal_length %% 2 !=
      0
    expect_identical(result$logs$scale[!changed], previous$gross_scale[!changed],
      info = key
    )
    for (r in which(changed)) {
      at <- match(previous$id[r], trees$tree)
      length <- floor(previous$nominal_length[r])
      height <- if (parts[1] == "scribner")
        previous$start_height[r] + length else previous$nominal_end_height[r]
      diameter <- dib(trees$dbh[at], trees$ht[at], height, trees$spcd[at],
        model = trees$model[at]
      )$value
      expected <- switch(parts[1],
        scribner = .mc_scribner(
          floor(diameter +
                  0.5),
          length,
          TRUE
        ),
        international = .mc_intl14(
          floor(diameter +
                  0.5),
          length
        ),
        doyle = pmax(
          diameter -
            4, 0
        )^2 * length / 16
      )
      expect_identical(result$logs$scale[r], expected, info = key)
      # These recorded small-end diameters keep the rounded rule values
      # unchanged.
      expect_identical(result$logs$scale[r], previous$gross_scale[r], info = key)
    }
    if (parts[3] == "TRUE") {
      expect_identical(result$logs$value, result$logs$scale * 125 / 100, info = key)
    } else {
      expect_false("value" %in% names(result$logs), info = key)
    }
    tree_rows <- match(previous$id, trees$tree)
    volume_arguments <- list(
      dbh = trees$dbh[tree_rows], ht = trees$ht[tree_rows],
      spcd = trees$spcd[
        tree_rows
      ], model = trees$model[tree_rows], from = 0
    )
    for (inside in c(TRUE, FALSE)) {
      top <- do.call(stem_volume, c(volume_arguments, list(
        to = previous$nominal_end_height,
        inside_bark = inside
      )))
      bottom <- do.call(stem_volume, c(volume_arguments, list(
        to = previous$start_height,
        inside_bark = inside
      )))
      field <- if (inside)
        "log_gross_cubic_ib" else "log_gross_cubic_ob"
      if (parts[2] == "0") {
        expect_identical(top$value - bottom$value, previous[[field]], info = key)
      } else {
        basis <- if (inside)
          "inside" else "outside"
        expect_identical(
          top$value - bottom$value, interface_recording$rulings$body_volume[[key]][[basis]],
          info = key
        )
      }
    }
    trims <- result$residuals[result$residuals$cause == "trim", ]
    unchanged <- result$residuals[result$residuals$cause != "trim", ]
    old_residuals <- interface_recording$cascade[[key]]$residuals
    expect_identical(unchanged$tree_id, old_residuals$id, info = key)
    mapping <- c(start_height = "from", end_height = "to", cause = "cause")
    for (name in names(mapping)) {
      expect_identical(unchanged[[name]], old_residuals[[mapping[[name]]]], info = key)
    }
    expect_identical(nrow(trims), if (parts[2] == "0")
                       0L else nrow(previous), info = key)
    if (nrow(trims)) {
      expect_identical(trims$start_height, previous$nominal_end_height, info = key)
      expect_identical(trims$end_height, previous$end_height, info = key)
    }
  }
})

test_that("stem measurements and volumes preserve the recording exactly", {
  trees <- interface_recording$trees
  for (height in names(interface_recording$diameters)) {
    actual <- dib(trees$dbh, trees$ht, as.numeric(height), trees$spcd, model = trees$model)
    expect_identical(actual, interface_recording$diameters[[height]]$dib)
    actual <- dob(trees$dbh, trees$ht, as.numeric(height), trees$spcd, model = trees$model)
    expect_identical(actual, interface_recording$diameters[[height]]$dob)
  }
  expect_identical(
    stem_volume(trees$dbh, trees$ht, trees$spcd, model = trees$model),
    interface_recording$volumes$inside
  )
  expect_identical(stem_volume(trees$dbh, trees$ht, trees$spcd,
                     model = trees$model,
                     inside_bark = FALSE
                   ), interface_recording$volumes$outside)
  expect_identical(
    stem_volume(
      trees$dbh,
      trees$ht,
      trees$spcd,
      model = trees$model,
      from = 10,
      to = 40
    ),
    interface_recording$volumes$section
  )
})

test_that("biomass changes only at the metric tonne public boundary", {
  trees <- interface_recording$trees
  actual <- biomass(trees$dbh, trees$ht, trees$spcd)
  mass <- c(names(actual)[startsWith(names(actual), "dry_")], "carbon")
  for (name in mass) {
    recorded_name <- if (name == "dry_aboveground_no_foliage") "dry_agb_no_foliage" else name
    expect_identical(
      actual[[name]],
      interface_recording$biomass[[recorded_name]] * 0.45359237 / 1000
    )
  }
  expect_identical(actual$tco2e, actual$carbon * 44 / 12)
  expect_identical(actual$status, interface_recording$biomass$carbon_status)
  expect_false(any(startsWith(names(actual), "green_")))
})

test_that("height fitting and prediction retain the recording exactly", {
  trees <- interface_recording$height_inputs
  fit <- fit_height(trees$dbh, trees$ht_observed, trees$spcd, group = trees$plot)
  expect_identical(fit[names(interface_recording$height_fit)], interface_recording$height_fit)
  expect_identical(
    predict_height(fit, trees$dbh, trees$spcd, group = trees$plot),
    interface_recording$height_predictions
  )
})

test_that("the defect rulings determine each remapped segment", {
  p <- products(
    product("saw",
      min_length = 32, max_length = 32, trim = 0.5, min_sed = 8,
      volume_unit = "cubic"
    ),
    product("pulp",
      min_length = 16, max_length = 16,
      min_sed = 3, volume_unit = "cubic"
    )
  )
  effects <- c(
    cull = "cull", pulp = "restrict", exclude = "cull", rot = NA_character_,
    sweep = "sweep", crook = "sweep", fork = "restrict", `break` = "end"
  )
  for (old_effect in names(effects)) {
    effect <- effects[[old_effect]]
    record <- if (is.na(effect))
      NULL else defect(1, 10, if (old_effect %in% c("fork", "break"))
                         NA_real_ else 20, effect, product = if (effect == "restrict")
                         "pulp" else NULL, percent = if (effect == "sweep")
                         10 else NULL)
    result <- merchandise(1, 24, 120, 202, p,
      model = "F00FW2W202", defects = record,
      quiet = TRUE
    )
    previous <- interface_recording$defects[[old_effect]]
    expected <- interface_defect_expectation(old_effect, interface_recording)
    for (table in c("logs", "residuals")) {
      expect_identical(nrow(result[[table]]), nrow(expected[[table]]), info = old_effect)
      for (name in names(expected[[table]])) {
        expect_equal(result[[table]][[name]], expected[[table]][[name]],
          tolerance = 1e-12, info = paste(old_effect, table, name)
        )
      }
    }
    # Compare every surviving cell with its own recording when geometry is
    # unchanged.
    mapping <-
      c(
        tree_id = "id", log = "log", product = "product", start_height = "start_height",
        end_height = "end_height", length = "nominal_length", sed = "sed_ib",
        led = "led_ib", scale = "gross_scale"
      )
    for (name in names(mapping)) {
      old <- previous$logs[[mapping[[name]]]]
      new <- expected$logs[[name]]
      same <- seq_len(min(length(old), length(new)))
      same <- same[old[same] == new[same]]
      expect_identical(result$logs[[name]][same], old[same], info = old_effect)
    }
  }
})
