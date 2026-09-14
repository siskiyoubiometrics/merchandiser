.mc_felled_lanes <- function(labels, midpoints, distance, cex = 0.62) {
  occupied <- rep(-Inf, length(labels))
  positions <- heights <- numeric(length(labels))
  lane <- integer(length(labels))
  for (j in order(midpoints)) {
    width <- graphics::strwidth(labels[j], cex = cex)
    positions[j] <- max(width / 2, min(distance - width / 2, midpoints[j]))
    lane[j] <- which(occupied < positions[j] - width / 2 - distance * 0.01)[1]
    occupied[lane[j]] <- positions[j] + width / 2
    heights[j] <- graphics::strheight(labels[j], cex = cex)
  }
  lane_heights <- vapply(seq_len(max(c(0L, lane))), function(i) {
    max(heights[lane == i]) + graphics::strheight("M", cex = 0.3)
  }, numeric(1))
  list(x = positions, lane = lane, heights = lane_heights, labels = labels)
}

.mc_felled_note_plan <- function(logs, residuals, stump, distance, units) {
  trims <- logs[logs$trim_cubic_ib > 0, , drop = FALSE]
  residuals <- residuals[residuals$cause != "stump", , drop = FALSE]
  labels <- c("Stump left on site", gsub("_", " ", residuals$cause),
              if (nrow(trims)) paste("Trim", format(trims$physical_length - trims$nominal_length,
                                                    digits = 3, trim = TRUE), units$length))
  midpoints <- c(0, (residuals$from + residuals$to) / 2 - stump,
                 trims$end_height - stump)
  .mc_felled_lanes(labels, midpoints, distance, cex = 0.45)
}

.mc_product_colors <- function(products) {
  colors <- vapply(products, function(name) {
    code <- utf8ToInt(enc2utf8(name))
    hue <- sum(code * seq_along(code)) %% 360
    grDevices::hcl(hue, c = 65, l = 55)
  }, character(1L))
  fixed <- c("Large sawlog" = "#0072B2", "Medium sawlog" = "#E69F00",
             "Small sawlog" = "#009E73")
  shared <- intersect(products, names(fixed))
  colors[shared] <- fixed[shared]
  colors
}

.mc_felled_band <- function(profile, from, to, stump, center, col,
                            density = NULL, angle = 45) {
  if (!is.finite(from) || !is.finite(to) || to <= from) return(invisible(NULL))
  h <- sort(unique(c(from, profile$h[profile$h > from & profile$h < to], to)))
  r <- stats::approx(profile$h, profile$dib / 2, xout = h, rule = 2)$y
  graphics::polygon(c(h, rev(h)) - stump, center + c(r, -rev(r)),
                    col = col, border = NA, density = density, angle = angle)
}

.mc_plot_unit <- function(unit) {
  labels <- c(board_foot = "board feet", ft3 = "cubic feet", m3 = "cubic meters",
              green_short_ton = "green short tons", green_metric_ton = "green metric tons",
              cord = "cords")
  unname(labels[unit])
}

.mc_felled_label_plan <- function(logs, values, stump, distance, units, show_value = FALSE) {
  cells <- lapply(seq_len(nrow(logs)), function(j) {
    z <- logs[j, ]
    value <- values[values$log == z$log, , drop = FALSE]
    c(as.character(z$log), z$product,
      paste(format(round(z$nominal_length, 2), trim = TRUE), units$length),
      paste(format(round(z$sed_ib, 1), trim = TRUE), units$diameter),
      paste(format(signif(z$net_scale, 3), trim = TRUE), .mc_plot_unit(z$scale_unit)),
      if (show_value && nrow(value) && is.finite(value$net_value[1])) {
        paste(format(signif(value$net_value[1], 3), trim = TRUE), value$currency[1])
      } else {
        "Unpriced"
      })
  })
  labels <- vapply(cells, function(z) {
    paste0("Log ", z[1], ": ", z[2], "\n", z[3], ", ", z[4],
           " small end inside bark\n", z[5], if (z[6] != "Unpriced") paste0("\n", z[6]))
  }, character(1))
  midpoints <- (logs$start_height + logs$nominal_end_height) / 2 - stump
  plan <- .mc_felled_lanes(labels, midpoints, distance)
  # More than 12 logs or four full-label lanes uses the compact drawing and key.
  key <- length(labels) > 12L || length(plan$heights) > 4L ||
    any(graphics::strwidth(labels, cex = 0.62) > distance)
  if (key) {
    labels <- as.character(logs$log)
    plan <- .mc_felled_lanes(labels, midpoints, distance)
  }
  plan$labels <- labels
  plan$key <- key
  plan$cells <- cells
  plan$show_value <- show_value
  plan
}

.mc_felled_key <- function(plan, distance) {
  cells <- rbind(c("Number", "Product", "Length", "Small end (ib)", "Quantity", "Value"),
                 do.call(rbind, plan$cells))
  if (!plan$show_value) cells <- cells[, -6, drop = FALSE]
  cex <- 0.6
  widths <- apply(cells, 2, function(z) max(graphics::strwidth(z, cex = cex)))
  cex <- cex * min(1, distance / (sum(widths) * 1.12))
  widths <- apply(cells, 2, function(z) max(graphics::strwidth(z, cex = cex)))
  gaps <- (distance - sum(widths)) / (ncol(cells) - 1)
  list(cells = cells, cex = cex, x = c(0, utils::head(cumsum(widths + gaps), -1)),
       step = graphics::strheight("M", cex = cex) * 1.65)
}

.mc_felled_log_labels <- function(logs, plan, profile, stump, center, radius) {
  offsets <- c(0, cumsum(plan$heights))
  for (j in seq_len(nrow(logs))) {
    z <- logs[j, ]
    y <- center + radius + graphics::strheight("M", cex = 0.5) + offsets[plan$lane[j]]
    graphics::text(plan$x[j], y, plan$labels[j], cex = 0.62, adj = c(0.5, 0))
    midpoint <- mean(c(z$start_height, z$nominal_end_height)) - stump
    anchor <- stats::approx(profile$h, profile$dib / 2, xout = midpoint + stump)$y
    graphics::segments(midpoint, center + anchor, plan$x[j], y,
                       col = "grey40", lwd = 0.6)
  }
}

.mc_felled_defects <- function(profile, records, stump, center, radius, label_top) {
  for (j in seq_len(nrow(records))) {
    d <- records[j, ]
    if (!is.finite(d$from)) next
    interval <- is.finite(d$to) && d$to > d$from
    label <- switch(d$effect, exclude = "Exclusion: product eligibility",
                    cull = "Cull: residual wood", pulp = "Restricted to pulp", d$effect)
    if (is.finite(d$percent)) label <- paste0(label, " ", d$percent, "%")
    if (!is.na(d$category)) label <- paste(label, d$category)
    y <- label_top - (j - 1) * graphics::strheight("M", cex = 0.6) * 2.5
    if (interval) {
      if (d$effect %in% c("rot", "cull", "exclude")) {
        angles <- switch(d$effect, rot = 45, cull = c(45, -45), exclude = c(0, 90))
        for (angle in angles) {
          .mc_felled_band(profile, d$from, d$to, stump, center,
                          "firebrick", density = 15, angle = angle)
        }
      }
      ends <- c(d$from, d$to) - stump
      graphics::segments(ends[1], y, ends[2], y, col = "firebrick")
      graphics::segments(ends, y, ends, y + radius * 0.2, col = "firebrick")
      graphics::text(mean(ends), y - radius * 0.12, label,
                     adj = c(0.5, 1), cex = 0.6, col = "firebrick")
    } else {
      point <- d$from - stump
      if (d$effect == "break") {
        r <- stats::approx(profile$h, profile$dob / 2, xout = d$from, rule = 2)$y
        graphics::segments(point, center - r, point, center + r, lwd = 4, col = "white")
      }
      graphics::points(point, center, pch = if (d$effect == "fork") 8 else 4,
                       col = "firebrick", lwd = 2)
      graphics::text(point, y, label, cex = 0.6, col = "firebrick")
    }
  }
}

#' Draw a felled stem and its selected logs
#'
#' Draw selected logs, residual sections, and recorded defects from a merchandising result.
#'
#' @param x Required `merch_result` from [merchandise()] or [optimize_bucking()],
#'   retaining its recorded call. Missing is an error. Its tables and measurement
#'   units are described in [Result tables][result_tables].
#' @param tree One nonmissing atomic identifier matching `x$trees$id`, unitless.
#'   Default `NULL` selects the first tree when `trees` is also omitted.
#'   Unknown identifiers are errors.
#' @param trees Atomic vector of distinct, nonmissing identifiers matching
#'   `x$trees$id`, unitless. Default `NULL` uses `tree` or the first tree.
#'   Supply only `tree` or `trees`. Empty or unknown selections are errors.
#'
#' @param col Vector of base graphics colors, one per product in `x$logs`,
#'   optionally named by product. Default `NULL` selects colors by product name. Missing colors are
#' errors.
#'
#' Unitless. Example: `c(saw = 'steelblue')`.
#' @param legend One nonmissing logical value, default `TRUE` to draw the shared
#'   product and line key. Example: `legend = FALSE`. Unitless.
#' @param show_value One nonmissing logical value, default `FALSE`. Use `TRUE`
#'   to add priced log totals and their currency to labels or the key.
#' @param ... Named base graphics settings passed to [graphics::plot.default()]. Omission uses
#'   labeled axes and the result's units.
#'
#' Example: `main = 'volume'`. Missing settings follow base graphics behavior. Axis limits use
#' plotted
#'   coordinates.
#'
#' Other settings are unitless.
#' @details Solid outlines show inside bark and dashed outlines show outside bark. Colored log
#' bodies stop at nominal ends. Pale hatched trim ends at physical cuts.
#'
#' Gray residuals carry cause labels. Defects retain their recorded locations and
#' categories. Unlocated percentage deductions are labeled without invented locations.
#'
#' Missing profiles receive explanatory rows. Custom models must remain registered. Labels use
#' measured text extents.
#'
#' Crowded labels switch to numbered keys below the stem. Each key lists product, length,
#' inside-bark small-end diameter, and quantity. `show_value` adds priced totals.
#'
#' Row heights include all lanes and key rows. Text and stems shrink together when the active panel
#' cannot hold their preferred
#' size. The drawing occupies one panel and respects an existing `par(mfrow)` setting.
#' @return The original result, invisibly, with tables described in [Result tables][result_tables].
#' @section Status and missing values:
#' The drawing adds no status codes. Missing profiles receive a labeled row. Review tree status
#' before interpreting partial logs or missing quantities.
#'
#' `no_feasible_log` means a valid result with no logs. Other nonzero codes require review
#' using [status_codes()]. Invalid identifiers or colors stop the call.
#' @seealso [Result tables][result_tables] for plotted quantities, [assumptions()] for saved
#'   choices, and [plot.stem_profile()] for an uncut profile.
#' @usage
#'
#' ## Call signatures
#' \method{plot}{merch_result}(x, tree = NULL, trees = NULL, col = NULL, legend = TRUE,
#'   show_value = FALSE, ...)
#' @examples
#' ## Calculate logs for the shipped tree list
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       model = example_trees$model,
#'                       species = example_trees$species,
#'                       products = example_products(name = 'pnw'),
#'                       status = TRUE)
#'
#' ## Inspect the result
#' plot(result, tree = 5)
#' @export
plot.merch_result <- function(x, tree = NULL, trees = NULL, col = NULL,
                              legend = TRUE, show_value = FALSE, ...) {
  ids <- .mc_plot_ids(x$trees$id, tree, trees)
  legend <- .validate_status(legend)
  show_value <- .validate_status(show_value)
  products <- unique(x$logs$product)
  if (is.null(col)) col <- .mc_product_colors(products)
  if (!is.null(names(col))) col <- col[products]
  if (length(col) != length(products) || anyNA(col)) {
    stop("col must provide a color for every product in x$logs.", call. = FALSE)
  }
  colors <- stats::setNames(col, products)
  rows <- match(ids, x$trees$id)
  selected <- x$trees[rows, , drop = FALSE]
  profiles <- lapply(rows, function(row) .mc_plot_profile(x, row))
  radii <- unlist(lapply(profiles, function(p) c(p$dib, p$dob) / 2))
  radius <- max(c(1, radii[is.finite(radii)]))
  distance <- max(c(1, selected$ht - selected$stump_height), na.rm = TRUE)
  imperial <- identical(x$run_metadata$units, "imperial")
  units <- list(length = if (imperial) "feet" else "meters",
                diameter = if (imperial) "inches" else "centimeters")
  old_par <- graphics::par(c("mar", "cex"))
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(4.5, 1, 3, 1))
  defaults <- list(x = 0, y = 0, type = "n", axes = FALSE,
                   xlim = c(-0.02 * distance, distance * 1.02),
                   ylim = c(0, graphics::par("pin")[2]), xaxs = "i", yaxs = "i",
                   xlab = "", ylab = "")
  do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
  specifications <- x$run_metadata$call$products
  priced <- specifications$product[!is.na(specifications$price)]
  log_rows <- lapply(ids, function(id) x$logs[x$logs$id == id, , drop = FALSE])
  residual_rows <- lapply(ids, function(id) x$residuals[x$residuals$id == id, , drop = FALSE])
  value_rows <- lapply(ids, function(id) {
    x$values[x$values$id == id & x$values$product %in% priced, , drop = FALSE]
  })
  records <- x$run_metadata$call$defects
  calls <- x$run_metadata$posthoc_calls
  # Lay out in device inches, then fit all rows into this panel without adding panels.
  scale <- 1
  repeat {
    graphics::par(cex = old_par$cex * scale)
    radial <- 0.3 * scale
    gap <- graphics::strheight("M", cex = 0.6)
    plans <- lapply(seq_along(ids), function(i) {
      .mc_felled_label_plan(log_rows[[i]], value_rows[[i]],
                            selected$stump_height[i], distance, units, show_value)
    })
    notes <- lapply(seq_along(ids), function(i) {
      .mc_felled_note_plan(log_rows[[i]], residual_rows[[i]],
                           selected$stump_height[i], distance, units)
    })
    note_height <- vapply(notes, function(note) sum(note$heights), numeric(1))
    keys <- lapply(plans, function(plan) if (plan$key) .mc_felled_key(plan, distance))
    above <- vapply(plans, function(plan) radial + sum(plan$heights) + 3.5 * gap, numeric(1))
    below <- vapply(seq_along(ids), function(i) {
      defects <- if (is.null(records)) 0 else sum(records$id == ids[i])
      posthoc <- if (!is.null(calls) && nrow(calls)) 2 else 0
      radial + note_height[i] + (defects * 2.5 + posthoc) * gap + 2 * gap
    }, numeric(1))
    key_height <- vapply(keys, function(key) {
      if (is.null(key)) 0 else (nrow(key$cells) + 1) * key$step
    }, numeric(1))
    legend_labels <- c(products, "Inside bark", "Outside bark",
                       "Nominal end", "Physical cut", "Trim")
    legend_args <- list(legend = legend_labels, col = c(colors, rep("grey30", 5)),
                        pch = c(rep(15, length(products)), rep(NA, 4), 15),
                        lty = c(rep(NA, length(products)), 1, 2, 3, 1, NA),
                        bty = "n", cex = 0.6, ncol = length(legend_labels))
    repeat {
      legend_size <- do.call(graphics::legend, c(list(x = 0, y = 0, plot = FALSE), legend_args))
      if (legend_size$rect$w <= distance || legend_args$ncol == 1L) break
      legend_args$ncol <- legend_args$ncol - 1L
    }
    legend_height <- if (legend) legend_size$rect$h + gap else 0
    heights <- above + below + key_height
    total <- sum(heights) + legend_height + gap
    available <- diff(graphics::par("usr")[3:4])
    if (total <= available) break
    scale <- scale * min(0.9, available / total)
  }
  # Convert the layout back to diameter units so the outlines retain their geometry.
  radius_factor <- radial / radius
  gap <- gap / radius_factor
  above <- above / radius_factor
  below <- below / radius_factor
  note_height <- note_height / radius_factor
  heights <- heights / radius_factor
  total <- total / radius_factor
  legend_height <- legend_height / radius_factor
  plans <- lapply(plans, function(plan) {
    plan$heights <- plan$heights / radius_factor
    plan
  })
  notes <- lapply(notes, function(note) {
    note$heights <- note$heights / radius_factor
    note
  })
  keys <- lapply(keys, function(key) {
    if (!is.null(key)) key$step <- key$step / radius_factor
    key
  })
  graphics::plot.window(xlim = graphics::par("usr")[1:2], ylim = c(0, total),
                        xaxs = "i", yaxs = "i")
  top <- total - legend_height - gap / 2
  centers <- top - c(0, utils::head(cumsum(heights), -1)) - above
  graphics::axis(1, at = pretty(c(0, distance)))
  graphics::mtext(paste0("Distance from butt cut (", units$length, ")"),
                  side = 1, line = 2, cex = 0.8)
  user <- graphics::par("usr")
  inches <- graphics::par("pin")
  exaggeration <- (inches[2] / diff(user[3:4])) / (inches[1] / diff(user[1:2])) *
    if (imperial) 12 else 100
  graphics::mtext(paste0("Radius mirrored, vertical exaggeration ",
                         format(round(exaggeration, 1), trim = TRUE), " times"),
                  side = 1, line = 3.5, cex = 0.65)
  for (i in seq_along(ids)) {
    p <- profiles[[i]]
    t <- selected[i, ]
    center <- centers[i]
    species <- if ("spcd" %in% names(t)) {
      species_reference$common[match(t$spcd, species_reference$spcd)]
    } else {
      "Species unavailable"
    }
    label <- paste0(t$id, " | ", species, " | ", t$dbh, " ", units$diameter,
                    " diameter | ", t$ht, " ", units$length, " height")
    header_cex <- 0.7 * min(1, distance / graphics::strwidth(label, cex = 0.7))
    graphics::text(0, center + above[i] - gap, label, adj = c(0, 1), cex = header_cex)
    if (sum(is.finite(p$h) & is.finite(p$dib)) < 2) {
      graphics::text(distance / 2, center, "Stem profile unavailable. Check tree status.")
      next
    }
    stump <- t$stump_height
    band <- function(a, b, color, density = NULL, angle = 45) {
      .mc_felled_band(p, a, b, stump, center, color, density, angle)
    }
    band(0, t$ht, "grey94")
    residuals <- x$residuals[x$residuals$id == t$id, , drop = FALSE]
    for (j in seq_len(nrow(residuals))) {
      r <- residuals[j, ]
      band(r$from, r$to, "grey80")

    }
    band(0, stump, "grey60")
    logs <- x$logs[x$logs$id == t$id, , drop = FALSE]
    for (j in seq_len(nrow(logs))) {
      z <- logs[j, ]
      band(z$start_height, z$nominal_end_height, colors[[z$product]])
      band(z$nominal_end_height, z$end_height, "grey95")
      band(z$nominal_end_height, z$end_height, "grey50", 15, 90)
      ends <- c(z$start_height, z$nominal_end_height, z$end_height)
      r <- stats::approx(p$h, p$dib / 2, xout = ends, rule = 2)$y
      graphics::segments(ends - stump, center - r, ends - stump, center + r, lty = c(1, 3, 1))
    }
    for (side in c(-1, 1)) {
      graphics::lines(p$h - stump, center + side * p$dib / 2)
      if (any(is.finite(p$dob))) {
        graphics::lines(p$h - stump, center + side * p$dob / 2, lty = 2, col = "grey40")
      }
    }
    .mc_felled_log_labels(logs, plans[[i]], p, stump, center, radius)
    note <- notes[[i]]
    offsets <- c(0, cumsum(note$heights))
    for (j in seq_along(note$labels)) {
      graphics::text(note$x[j], center - radius - gap / 2 - offsets[note$lane[j]],
                     note$labels[j], adj = c(0.5, 1), cex = 0.45)
    }
    key <- keys[[i]]
    if (!is.null(key)) {
      for (j in seq_len(nrow(key$cells))) for (k in seq_len(ncol(key$cells))) {
        graphics::text(key$x[k], center - below[i] - (j - 1) * key$step,
                       key$cells[j, k], cex = key$cex, adj = c(0, 1))
      }
    }
    if (!is.null(records)) {
      .mc_felled_defects(p, records[records$id == t$id, , drop = FALSE], stump, center, radius,
                         label_top = center - radius - note_height[i] - gap)
    }
    if (!is.null(calls) && nrow(calls)) {
      percentages <- ifelse(is.na(calls$percent), "varies by log",
                            paste0(calls$percent, "%"))
      graphics::text(distance, center - below[i] + gap,
                     paste("Unlocated percent deductions:", paste(percentages, collapse = ", ")),
                     adj = 1, cex = 0.6)
    }
  }
  if (legend) {
    do.call(graphics::legend, c(list(x = 0, y = graphics::par("usr")[4]), legend_args))
  }
  invisible(x)
}
