.interface_spcd <- function(model) {
  ids <- unique(as.character(model))
  codes <- vapply(ids, function(id) {
    if (is.na(id) || !has_taper_model(id)) return(202)
    scope <- get_taper_model(id)$spcd
    if (length(scope)) as.double(scope[1]) else 202
  }, numeric(1))
  unname(codes[match(as.character(model), ids)])
}

.interface_metric_result <- function(x, multiplier) {
  x$value <- x$value * multiplier
  x
}

.interface_taper_inches <- function(data) {
  data$dbh <- data$dbh / 2.54
  data$dib <- data$dib / 2.54
  data$ht <- data$ht / 0.3048
  data$h <- data$h / 0.3048
  data
}

.oracle_dib <- function(dbh, ht, h, spcd, model = NULL, ...) {
  .diameter_impl(
    dbh, ht, h, model, c(list(spcd = spcd), list(...)), "imperial", TRUE, "dib", "dib"
  )
}
.oracle_dob <- function(dbh, ht, h, spcd, model = NULL, ...) {
  .diameter_impl(
    dbh, ht, h, model, c(list(spcd = spcd), list(...)), "imperial", TRUE, "dob", "dob"
  )
}
.oracle_height_at_dib <- function(dbh, ht, dib, spcd, model = NULL, ...) {
  .height_impl(
    dbh, ht, dib, model, c(list(spcd = spcd), list(...)), "imperial", TRUE,
    "dib", "height_at_dib"
  )
}
.oracle_height_at_dob <- function(dbh, ht, dob, spcd, model = NULL, ...) {
  .height_impl(
    dbh, ht, dob, model, c(list(spcd = spcd), list(...)), "imperial", TRUE,
    "dob", "height_at_dob"
  )
}
.oracle_stem_volume <- function(
  dbh, ht, spcd, model = NULL, lower = 0, lower_type = "stump",
  upper = 0, upper_type = "tip", inside_bark = TRUE,
  stump_ht = NULL, ...
) {
  .stem_volume_impl(
    dbh, ht, model, lower, lower_type, upper, upper_type,
    if (inside_bark) "inside" else "outside", stump_ht,
    c(list(spcd = spcd), list(...)), "imperial", TRUE
  )
}
