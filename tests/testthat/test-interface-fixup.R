test_that("sold_by normalization handles factors and rejects malformed values", {
  for (units in c("imperial", "metric")) {
    expect_identical(.mc_sold_by(factor(c("cubic", "green_ton")), units),
                     .mc_sold_by(c("cubic", "green_ton"), units))
  }
  for (bad in list(1, NA_character_, list("cubic"))) {
    expect_error(.mc_sold_by(bad, "imperial"), "procedure names")
  }
  expect_error(.mc_sold_by("unknown", "metric"), "Unknown sold_by")
  expect_error(.mc_expand_products(list(), "imperial"), "data frame")
  frame <- data.frame(product = "saw", priority = 1, sold_by = "cubic", scale_rule = "cubic")
  expect_error(validate_products(frame), "Supply sold_by alone")
})

test_that("the adapter rejects retired authoring and malformed report definitions", {
  p <- .mc_legacy_product("saw", 1, lengths = 16, min_sed = 4,
                          diameter_basis = "ib", scale_rule = "cubic",
                          measurement_quantity = "cubic",
                          scale_unit = "ft3", scale_bark_basis = "ib")
  p$segmentation_policy <- "nvel"
  expect_error(.mc_check_authored_measurement(p), "segmentation_policy belongs")
  p$segmentation_policy <- "generic"
  p$scale_rule <- "scribner_factor_split_20"
  expect_error(.mc_check_authored_measurement(p), "report_also")
  p$scale_rule <- "cubic"
  p$measurement_quantity <- "cubic"
  p$scale_unit <- "ft3"
  p$diameter_round <- "truncate_1in"
  expect_error(.mc_validate_product_values(p), "does not accept dimension rounding")
  expect_error(.mc_report_also("cubic", "cubic", "imperial"), "not both")
  expect_error(.mc_report_also(data.frame(name = "a", extra = 1), NULL, "imperial"),
               "Unknown report_also column")
  for (bad in list(list(name = "a"), data.frame(sold_by = "cubic"),
                   data.frame(name = NA_character_), data.frame(name = ""),
                   data.frame(name = c("a", "a")))) {
    expect_error(.mc_report_also(bad, NULL, "imperial"), "distinct nonempty")
  }
})

test_that("named expanded and short reporting procedures preserve their units", {
  for (units in c("imperial", "metric")) {
    for (rule in c("huber", "smalian", "doyle_formula")) {
      short <- .mc_report_also(data.frame(name = "check", sold_by = rule), NULL, units)
      expanded <- .mc_report_also(data.frame(name = "check", scale_rule = rule,
                                             scale_bark_basis = "ib"), NULL, units)
      a <- attr(short, "report_definitions")
      b <- attr(expanded, "report_definitions")
      expect_identical(a, b)
      expected_unit <- if (rule == "doyle_formula") "board_foot" else
        if (units == "imperial") "ft3" else "m3"
      expect_identical(a$scale_unit, expected_unit)
    }
    p <- product("saw", 1, lengths = 8, trim = 0.5, min_sed = 1,
                 diameter_basis = "ib", sold_by = "cubic")
    args <- list(dbh = 20, ht = 80, model = "demo.paraboloid", products = p,
                 units = units, status = TRUE)
    base <- do.call(merchandise, args)
    args$report_also <- data.frame(name = "measured", scale_rule = "huber",
                                   scale_bark_basis = "ib")
    extra <- do.call(merchandise, args)
    expect_identical(.mc_plain_product_frame(extra$logs), .mc_plain_product_frame(base$logs))
    expect_identical(extra$values, base$values)
    rows <- extra$scales[!extra$scales$is_product, ]
    expect_true(all(is.finite(rows$net)))
    expect_true(all(rows$unit == if (units == "imperial") "ft3" else "m3"))
  }
})

# Capture text rectangles in the actual active panel, including font scaling.
# This catches clipped glyphs, lane collisions, and row or legend overlap.
fixup_drawn_text <- function(x, ...) {
  drawn <- list()
  original <- graphics::text
  local_mocked_bindings(text = function(x, y, labels, cex = 1, adj = c(0.5, 0.5), ...) {
    if (length(adj) == 1) adj <- c(adj, 0.5)
    labels <- as.character(labels)
    for (i in seq_along(labels)) {
      px <- rep_len(x, length(labels))[i]
      py <- rep_len(y, length(labels))[i]
      width <- graphics::strwidth(labels[i], cex = cex)
      height <- graphics::strheight(labels[i], cex = cex)
      drawn[[length(drawn) + 1L]] <<- list(
        label = labels[i], left = px - width * adj[1], right = px + width * (1 - adj[1]),
        bottom = py - height * adj[2], top = py + height * (1 - adj[2]),
        usr = graphics::par("usr"), srt = list(...)$srt
      )
    }
    original(x, y, labels, cex = cex, adj = adj, ...)
  }, .package = "graphics")
  plot(x, ...)
  drawn
}

fixup_expect_bounds <- function(drawn) {
  for (z in drawn) {
    expect_true(z$left + 1e-7 >= z$usr[1], info = z$label)
    expect_true(z$right - 1e-7 <= z$usr[2], info = z$label)
    expect_true(z$bottom + 1e-7 >= z$usr[3], info = z$label)
    expect_true(z$top - 1e-7 <= z$usr[4], info = z$label)
    expect_true(is.null(z$srt) || z$srt == 0, info = z$label)
  }
  collisions <- character()
  if (length(drawn) > 1) for (i in seq_len(length(drawn) - 1)) {
    for (j in seq.int(i + 1, length(drawn))) {
      a <- drawn[[i]]
      b <- drawn[[j]]
      if (a$right > b$left && b$right > a$left && a$top > b$bottom && b$top > a$bottom) {
        collisions <- c(collisions, paste(a$label, b$label, sep = " / "))
      }
    }
  }
  expect_identical(collisions, character())
}

test_that("adaptive diagrams fit many logs, stacked trees, and caller panels", {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 900, height = 500)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  make <- function(lengths, ids = "short") {
    p <- product("Short sawlog", 1, lengths = lengths, trim = 0.1, min_sed = 1,
                 diameter_basis = "ib", sold_by = "cubic", price = 10)
    merchandise(rep(20, length(ids)), 80, "demo.paraboloid", p,
                id = ids, spcd = 202, currency = "USD", status = TRUE)
  }
  many <- make(4)
  expect_gt(nrow(many$logs), 12)
  drawn <- fixup_drawn_text(many, show_value = TRUE)
  fixup_expect_bounds(drawn)
  labels <- vapply(drawn, `[[`, character(1), "label")
  expect_true(all(c("Number", "Product", "Length", "Small end (ib)", "Quantity", "Value") %in%
                    labels))
  expect_false(any(grepl("^Log [0-9]", labels)))
  expect_equal(sum(labels == "Short sawlog"), nrow(many$logs) + 1)
  expect_true(any(grepl("USD", labels)))
  unpriced_labels <- vapply(fixup_drawn_text(many), `[[`, character(1), "label")
  expect_false("Value" %in% unpriced_labels)
  expect_false(any(grepl("USD", unpriced_labels)))
  stacked <- make(12, c("first", "second"))
  drawn <- fixup_drawn_text(stacked, trees = stacked$trees$id)
  fixup_expect_bounds(drawn)
  labels <- vapply(drawn, `[[`, character(1), "label")
  expect_true(any(grepl("^first \\|", labels)))
  expect_true(any(grepl("^second \\|", labels)))
  graphics::par(mfrow = c(1, 2))
  original <- graphics::par(c("mfrow", "mar", "cex"))
  fixup_expect_bounds(fixup_drawn_text(make(16)))
  expect_identical(graphics::par("mfg")[1:2], c(1L, 1L))
  fixup_expect_bounds(fixup_drawn_text(many))
  expect_identical(graphics::par("mfg")[1:2], c(1L, 2L))
  expect_identical(graphics::par(names(original)), original)
})

test_that("plots annotate post-hoc deductions and located breaks", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  p <- product("saw", 1, lengths = 16, trim = 0.5, min_sed = 1,
               diameter_basis = "ib", sold_by = "cubic")
  x <- merchandise(20, 80, "demo.paraboloid", p, id = "a", status = TRUE,
                   defects = defect("a", 70, NA_real_, effect = "break"))
  x <- apply_defect_pct(x, 10, kind = "handling")
  x <- apply_defect_pct(x, seq_len(nrow(x$logs)), kind = "variable")
  drawn <- fixup_drawn_text(x)
  labels <- vapply(drawn, `[[`, character(1), "label")
  expect_true(any(grepl("Unlocated percent deductions: 10%, varies by log", labels)))
  expect_true("break" %in% labels)
})
