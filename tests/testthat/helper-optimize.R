# Exhaustive enumeration is deliberately independent of the native bucking engine.
.optimize_patterns <- function(p, ht, dbh) {
  heights <- seq(0, ht, by = 0.5)
  cumulative <- stem_volume(
    dbh,
    ht,
    202,
    model = "demo.paraboloid",
    from = 0,
    to = heights
  )$value
  diameters <- dib(dbh, ht, heights, 202, model = "demo.paraboloid")$value
  walk <- function(start, counts) {
    patterns <- list(data.frame(product = character(), start = numeric(), length = numeric(),
                                value = numeric()))
    for (i in seq_len(nrow(p))) {
      if (!is.na(p$max_logs[i]) && counts[i] >= p$max_logs[i]) next
      for (len in seq(p$min_length[i], p$max_length[i], by = 0.5)) {
        end <- start + len + p$trim[i]
        if (end > ht) next
        if (diameters[match(end, heights)] < p$min_sed[i]) next
        value <- (cumulative[match(start + len, heights)] -
                    cumulative[match(start, heights)]) * p$price[i] / p$price_per[i]
        next_counts <- counts
        next_counts[i] <- next_counts[i] + 1L
        head <- data.frame(product = p$product[i], start = start, length = len, value = value)
        for (tail in walk(end, next_counts)) {
          patterns[[length(patterns) + 1L]] <- rbind(head, tail)
        }
      }
    }
    patterns
  }
  walk(1, integer(nrow(p)))
}

.optimize_specs <- function() {
  products(product("saw", min_length = 16, max_length = 16, min_sed = 0,
                   volume_unit = "cubic", price = 2, max_sweep = 10),
           product("pulp", min_length = 8, max_length = 8, min_sed = 0,
                   volume_unit = "cubic", price = 1))
}
