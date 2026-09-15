.mc_product_colors <- function(products) {
  colors <- vapply(products, function(name) {
    code <- utf8ToInt(enc2utf8(name))
    hue <- sum(code * seq_along(code)) %% 360
    grDevices::hcl(hue, c = 65, l = 55)
  }, character(1L))
  fixed <- c(
    `Large sawlog` = "#0072B2", `Medium sawlog` = "#E69F00",
    `Small sawlog` = "#009E73"
  )
  shared <- intersect(products, names(fixed))
  colors[shared] <- fixed[shared]
  colors
}

.mc_felled_band <- function(
  profile, from, to, stump, center, col, density = NULL,
  angle = 45
) {
  if (!is.finite(from) || !is.finite(to) || to <= from)
    return(invisible(NULL))
  h <- sort(unique(c(from, profile$h[profile$h > from & profile$h < to], to)))
  r <- stats::approx(profile$h, profile$dib / 2, xout = h, rule = 2)$y
  graphics::polygon(c(h, rev(h)) - stump, center + c(r, -rev(r)),
    col = col, border = NA, density = density,
    angle = angle
  )
}

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

#' @export
plot.merch_result <- function(x, tree_id = NULL, col = NULL, ...) {
  ids <- if (is.null(tree_id))
    head(x$call$tree_id, 1) else tree_id
  if (anyNA(ids) || any(!ids %in% x$call$tree_id)) {
    stop("tree_id must identify trees in this result.", call. = FALSE)
  }
  if (is.null(col))
    col <- .mc_product_colors(x$call$products$product)
  if (is.null(names(col)))
    names(col) <- x$call$products$product
  for (id in ids) {
    i <- match(id, x$call$tree_id)
    aux <- x$call[setdiff(names(x$call), names(formals(merchandise)))]
    aux <- lapply(aux, function(z) {
      if (length(z) == 1) z else z[i]
    })
    profile <- do.call(stem_profile, c(
      list(
        tree_id = id, dbh = x$call$dbh[i], ht = x$call$ht[i],
        spcd = x$call$spcd[i], model = x$call$model[i], step = 0.5, from = 0
      ),
      aux
    ))
    profile <- profile[is.finite(profile$h) & is.finite(profile$dib), ]
    if (nrow(profile) < 2) {
      graphics::plot.new()
      graphics::title(main = paste("Tree", id))
      graphics::text(0.5, 0.5, "Stem profile unavailable. Review tree status.")
      next
    }
    logs <- x$logs[x$logs$tree_id == id, ]
    residuals <- x$residuals[x$residuals$tree_id == id, ]
    records <- x$call$defects[x$call$defects$tree_id == id, ]
    height <- x$call$ht[i]
    radius <- max(profile$dib) / 2
    defaults <- list(x = 0, y = 0, type = "n", xlim = c(0, height), ylim = c(
      -radius * 5,
      radius * 8
    ), xlab = "Height above ground (feet)", ylab = "", yaxt = "n", main = paste(
      "Tree",
      id
    ))
    do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
    .mc_felled_band(profile, 0, height, 0, 0, "grey90")
    for (r in seq_len(nrow(logs))) {
      .mc_felled_band(
        profile, logs$start_height[r], logs$start_height[r] + logs$length[r],
        0, 0, col[logs$product[r]]
      )
    }
    for (r in seq_len(nrow(residuals))) {
      z <- residuals[r, ]
      color <- if (z$cause == "trim")
        "gold" else if (z$cause == "stump")
        "grey55" else "grey85"
      .mc_felled_band(profile, z$start_height, z$end_height, 0, 0, color)
      if (z$cause == "stump")
        graphics::text(mean(c(z$start_height, z$end_height)), -radius * 1.5, "stump",
          adj = c(0, 1),
          cex = 0.7
        )
    }
    if (nrow(logs)) {
      unit <- c(
        scribner = "board feet", international = "board feet", doyle = "board feet",
        cubic = "cubic feet", green_ton = "green short tons", cord = "cords"
      )
      labels <- paste0(
        logs$product, "\n", logs$length, " feet, ", format(round(
          logs$sed,
          1
        ), trim = TRUE), " inches small end\n", format(signif(logs$scale, 4), trim = TRUE),
        " ", unit[logs$volume_unit]
      )
      if ("value" %in% names(logs)) {
        priced <- !is.na(logs$value)
        labels[priced] <- paste0(labels[priced], "\nValue ", format(signif(
          logs$value[priced],
          4
        ), trim = TRUE))
      }
      midpoint <- logs$start_height + logs$length / 2
      plan <- .mc_felled_lanes(labels, midpoint, height, cex = 0.6)
      offsets <- c(0, cumsum(plan$heights))
      for (r in seq_len(nrow(logs))) {
        y <- radius * 1.5 + offsets[plan$lane[r]]
        graphics::segments(midpoint[r], radius, plan$x[r], y, col = "grey40")
        graphics::text(plan$x[r], y, labels[r], adj = c(0.5, 0), cex = 0.6)
      }
    }
    for (r in seq_len(nrow(records))) {
      z <- records[r, ]
      upper <- if (is.na(z$end_height))
        height else z$end_height
      y <- -radius * (2 + (r %% 3) * 0.8)
      if (z$effect %in% c("cull", "restrict")) {
        angles <- if (z$effect == "cull")
          c(45, -45) else c(0, 90)
        for (angle in angles) .mc_felled_band(profile, z$start_height, upper, 0, 0, "firebrick",
          density = 15, angle = angle
        )
        label <- if (z$effect == "restrict")
          paste("restricted to", z$product) else "cull"
        graphics::text(mean(c(z$start_height, upper)), y, label, cex = 0.65, col = "firebrick")
      } else if (z$effect == "end") {
        graphics::points(z$start_height, 0, pch = 4, lwd = 2, col = "firebrick")
        graphics::text(z$start_height, y, "end", cex = 0.65, col = "firebrick")
      } else if (z$effect == "sweep") {
        graphics::segments(z$start_height, y, upper, y, col = "firebrick")
        graphics::segments(c(z$start_height, upper), y, c(z$start_height, upper), y + radius * 0.2,
          col = "firebrick"
        )
        graphics::text(mean(c(z$start_height, upper)), y - radius * 0.2, paste0(
          "sweep ", z$percent,
          "%"
        ), cex = 0.65, col = "firebrick")
      }
    }
  }
  invisible(x)
}
