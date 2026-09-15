height_test_data <- function(form = "chapman_richards", seed = 810) {
  set.seed(seed)
  groups <- paste0("plot_", seq_len(12))
  group <- rep(groups, each = 12)
  dbh <- rep(seq(3, 30, length.out = 12), length(groups))
  random_effect <- rep(rnorm(length(groups), 0, 0.12), each = 12)
  coefficients <- switch(form,
    chapman_richards = c(la = log(95), lb = log(0.055), lc = log(1.25)),
    curtis = c(la = log(15), lb = log(0.6)),
    wykoff = c(a = log(100), lb = log(5)),
    naslund = c(
      la = log(0.08),
      lb = log(0.5)
    ),
    schumacher = c(la = log(100), lb = log(8))
  )
  mean_height <- merchandiser:::.height_evaluate(dbh, coefficients, form, random_effect)
  data.frame(
    dbh = dbh, ht = mean_height + rnorm(length(dbh), 0, 1.5), spcd = 122L, group = group,
    mean_height = mean_height, stringsAsFactors = FALSE
  )
}

.height_test_cache <- new.env(parent = emptyenv())

height_test_fit <- function() {
  if (is.null(.height_test_cache$fit)) {
    data <- height_test_data()
    .height_test_cache$fit <- fit_height(
      dbh = data$dbh, ht = data$ht, spcd = data$spcd,
      group = data$group
    )
  }
  .height_test_cache$fit
}
