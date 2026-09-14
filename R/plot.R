.mc_plot_ids <- function(ids, tree, trees) {
  if (!is.null(tree) && !is.null(trees)) {
    stop("Supply tree or trees, not both.", call. = FALSE)
  }
  if (!is.null(tree) && length(tree) != 1) {
    stop("tree must identify exactly one tree.", call. = FALSE)
  }
  selected <- if (!is.null(trees)) trees else if (!is.null(tree)) tree else ids[1]
  if (!length(selected) || anyNA(selected) || anyDuplicated(selected) ||
        any(!selected %in% ids)) {
    stop("Select distinct, nonmissing tree ids present in x.", call. = FALSE)
  }
  selected
}

.mc_plot_grid <- function(n) {
  columns <- ceiling(sqrt(n))
  c(ceiling(n / columns), columns)
}

.mc_profile_band <- function(profile, from, to, col, density = NULL) {
  if (!is.finite(from) || !is.finite(to) || to <= from) return(invisible(NULL))
  height <- sort(unique(c(from, profile$h[profile$h > from & profile$h < to], to)))
  diameter <- stats::approx(profile$h, profile$dib, xout = height, rule = 2)$y
  graphics::polygon(c(0, diameter, 0), c(from, height, to),
                    col = col, border = NA, density = density, angle = 45)
}

.mc_profile_frame <- function(profile, units, panel_title, ...) {
  finite <- is.finite(profile$h) & is.finite(profile$dib)
  if (sum(finite) < 2) {
    graphics::plot.new()
    graphics::title(main = panel_title)
    graphics::text(0.5, 0.5, "Stem profile unavailable. Check tree status.")
    return(FALSE)
  }
  imperial <- identical(units, "imperial")
  defaults <- list(
    x = 0, y = 0, type = "n", main = panel_title,
    xlim = c(0, max(c(profile$dib, profile$dob), na.rm = TRUE) * 1.05),
    ylim = range(c(0, profile$h), na.rm = TRUE),
    xlab = if (imperial) "Diameter (inches)" else "Diameter (centimeters)",
    ylab = if (imperial) "Height above ground (feet)" else "Height above ground (meters)"
  )
  do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
  TRUE
}

.mc_profile_lines <- function(profile) {
  graphics::lines(profile$dib, profile$h, lwd = 2)
  if (any(is.finite(profile$dob))) {
    graphics::lines(profile$dob, profile$h, lty = 2, lwd = 2)
  }
}

.mc_plot_profile <- function(x, row) {
  call <- x$run_metadata$call
  aux <- call[setdiff(names(call), names(formals(merchandise)))]
  if (!is.null(call$spcd)) aux$spcd <- call$spcd
  aux <- lapply(aux, function(value) if (length(value) > 1) value[row] else value)
  do.call(stem_profile, c(list(
    dbh = call$dbh[row], ht = call$ht[row], model = call$model[row],
    id = call$id[row], step = if (call$units == "imperial") 6 else 15.24,
    lower = 0, lower_type = "height", units = call$units, status = TRUE
  ), aux))
}

#' Draw a stem profile
#'
#' Draw inside- and outside-bark profiles from a [stem_profile()] result.
#'
#' @param x One data frame returned by [stem_profile()], retaining its class
#'   and units attribute. Required. Missing is an error.
#'
#' Columns `id`, `h`, `dib`, `dob`, and `status` supply the drawing. Height uses feet or meters,
#' and diameters use inches or centimeters. Identifiers and statuses are unitless.
#'
#' Cumulative volumes are unused.
#' @param trees Atomic vector of distinct, nonmissing identifiers matching `x$id`. Default `NULL`
#'   uses `tree` or the first tree. Supply only one selector.
#'
#' Empty or unknown selections are errors. Unitless.
#' @param tree One nonmissing identifier matching `x$id`, for example
#'   `example_trees$tree[1]`. Unitless. Default `NULL` selects the first tree
#'   when `trees` is also omitted.
#'
#' Unknown identifiers are errors.
#' @param ... Named base graphics arguments passed to [graphics::plot.default()]. Default axes use
#'   the saved units and the title is the tree identifier.
#'
#' Example: `main = 'Stem dimensions'`. Axis limits use plotted units. Other settings are unitless.
#'
#' Missing settings follow base graphics.
#' @section Status and missing values:
#'   Missing outside-bark diameters omit that line. A tree without enough
#'   finite inside-bark measurements receives a labeled empty panel. Inspect
#'   `x$status` with [status_codes()] and repair the original model inputs.
#' @return The profile data frame, invisibly. Draws one panel per selected tree.
#' @seealso [stem_profile()] to calculate the displayed measurements,
#'   [plot.merch_result()] to add product cuts and located conditions.
#' @export
#' @usage
#'
#' ## Call signatures
#' \method{plot}{stem_profile}(x, tree = NULL, trees = NULL, ...)
#' @examples
#' ## Calculate profiles for the example trees
#' profile <- stem_profile(dbh = example_trees$dbh,
#'                         ht = example_trees$ht,
#'                         model = example_trees$model,
#'                         id = example_trees$tree)
#'
#' ## Draw the selected tree
#' plot(profile, tree = 5)
#' @section Graphical settings:
#' `main` is one character panel title, with the tree or species label used when omitted.
#' `xlim` and `ylim` are numeric pairs in the displayed axis units, with ranges determined from
#' plotted data when omitted. `cex` and `cex.main` are numeric size multipliers, unitless, for
#' points or titles.
#'
#' `pch` selects a base graphics point symbol, for example `16` for a solid circle. Missing or
#' invalid settings follow [graphics::plot.default()]. Other named base graphics settings are
#' accepted by that function.
#' @inheritSection stem_profile Profile table fields
plot.stem_profile <- function(x, tree = NULL, trees = NULL, ...) {
  ids <- .mc_plot_ids(unique(x$id), tree, trees)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  if (length(ids) > 1) graphics::par(mfrow = .mc_plot_grid(length(ids)))
  for (index in seq_along(ids)) {
    id <- ids[index]
    profile <- x[x$id == id, , drop = FALSE]
    if (!.mc_profile_frame(profile, attr(x, "units"), as.character(id), ...)) next
    .mc_profile_lines(profile)
    outside <- any(is.finite(profile$dob))
    graphics::legend("topright", c("Inside bark", if (outside) "Outside bark"),
                     lty = if (outside) c(1, 2) else 1, bty = "n", cex = 0.8)
  }
  invisible(x)
}

#' Inspect measured heights and fitted height curves
#'
#' Draw retained height observations and fitted curves from a [fit_height()] result.
#'
#' @param x One [fit_height()] result. Required, with retained measured data
#'   (`data`) containing diameter `dbh` in inches, height `ht` in feet,
#'   species code `spcd` and grouping label `group` (both unitless). Missing
#'   or older fits without measured data are errors.
#' @param units One character value, `'imperial'` (default) for inches and
#'   feet or `'metric'` for centimeters and meters. Omission uses imperial
#'   display even if fitting used metric inputs. Missing or unknown values
#'   error.
#'
#' Example: `units = 'metric'`.
#' @param ... Named base graphics arguments passed to [graphics::plot.default()]. Omission uses
#'   labeled axes and species codes as panel titles.
#'
#'  Limits use displayed units, other settings
#'   are unitless. Missing settings follow base graphics behavior.
#' @details Curves use population predictions without group effects, over
#'   each species' measured diameter range. Points are the valid measured
#'   trees retained by the fit, not completed heights. The dashed pooled
#'   curve is labeled when a species used pooling.
#' @section Status and missing values:
#'   Input records rejected during fitting are absent. Pooling is disclosed
#'   in each panel. The plot does not estimate uncertainty or validate the
#'   fit against independent measurements.
#'
#' Refit older saved fits that do
#'   not retain measured data.
#' @return The height fit, invisibly. Draws one panel per measured species.
#' @seealso [fit_height()] to estimate a relationship,
#'   [predict_height()] for numeric predictions, [complete_heights()] to fill
#'   missing heights in an inventory.
#' @export
#' @usage
#'
#' ## Call signatures
#' \method{plot}{height_fit}(x, units = 'imperial', ...)
#' @examples
#' ## Fit the retained height observations
#' fit <- fit_height(dbh = example_trees_pnw$dbh,
#'                   ht = example_trees_pnw$ht_observed,
#'                   spcd = example_trees_pnw$spcd)
#'
#' ## Draw measured heights and fitted curves
#' plot(fit)
#' @section Graphical settings:
#' `main` is one character panel title, with the tree or species label used when omitted.
#' `xlim` and `ylim` are numeric pairs in the displayed axis units, with ranges determined from
#' plotted data when omitted. `cex` and `cex.main` are numeric size multipliers, unitless, for
#' points or titles.
#'
#' `pch` selects a base graphics point symbol, for example `16` for a solid circle. Missing or
#' invalid settings follow [graphics::plot.default()]. Other named base graphics settings are
#' accepted by that function.
#' @inheritSection fit_height Fitted height fields
plot.height_fit <- function(x, units = "imperial", ...) {
  units <- .validate_units(units)
  data <- x$data
  if (is.null(data)) stop("Refit with fit_height() to retain measured data.", call. = FALSE)
  species <- unique(data$spcd)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = .mc_plot_grid(length(species)))
  for (spcd in species) {
    measured <- data[data$spcd == spcd, ]
    diameter <- .diameter_from_native(measured$dbh, units, "imperial")
    height <- .height_from_native(measured$ht, units, "imperial")
    defaults <- list(
      x = diameter, y = height, main = paste("Species", spcd), pch = 16,
      xlab = if (units == "imperial") "Diameter at breast height (inches)" else
        "Diameter at breast height (centimeters)",
      ylab = if (units == "imperial") "Measured height (feet)" else "Measured height (meters)"
    )
    do.call(graphics::plot.default, utils::modifyList(defaults, list(...)))
    grid <- seq(min(diameter), max(diameter), length.out = 100)
    pooled <- x$model_map[[as.character(spcd)]] == "pooled"
    fitted <- predict_height(x, grid, spcd, re_form = "population", units = units)
    graphics::lines(grid, fitted, lwd = 2, lty = if (pooled) 2 else 1)
    graphics::legend("topleft", if (pooled) "Pooled curve" else "Species curve",
                     lty = if (pooled) 2 else 1, bty = "n", cex = 0.8)
  }
  invisible(x)
}
