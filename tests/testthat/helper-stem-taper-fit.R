taper_test_truth <- list(
  kozak_1988 = c(
    a0 = 0.9, a1 = 1, a2 = 0.995, b1 = 0.5, b2 = 0.05,
    b3 = 0.2, b4 = 0.1, b5 = 0.02, p = 0.2
  ),
  kozak_2002 = c(
    a0 = 1, a1 = 0.95, a2 = 0.05, b1 = 0.5, b2 = 0.2,
    b3 = 0.6, b4 = 0.1, b5 = 0.02, b6 = 0.1
  ),
  max_burkhart = c(
    b1 = -3, b2 = 1.5, b3 = -0.4, b4 = 25, a1 = 0.75, a2 = 0.12
  )
)

simulate_taper_test_data <- function(form, noise = 0.001, seed = 730L,
                                     tree_count = 24L) {
  set.seed(seed)
  relative_height <- c(
    0.01, 0.03, 0.05, 0.08, 0.12, 0.2,
    0.35, 0.5, 0.65, 0.8, 0.9, 0.97
  )
  dbh <- runif(tree_count, 18, 55)
  ht <- runif(tree_count, 14, 34)
  data <- data.frame(
    tree_id = rep(seq_len(tree_count), each = length(relative_height)),
    dbh = rep(dbh, each = length(relative_height)),
    ht = rep(ht, each = length(relative_height))
  )
  data$h <- data$ht * rep(relative_height, tree_count)
  data$dib <- merchandiser:::.taper_evaluate(
    form, data$dbh, data$ht, data$h, taper_test_truth[[form]]
  ) + stats::rnorm(nrow(data), 0, noise)
  data
}

taper_test_cache <- new.env(parent = emptyenv())

cached_taper_fit <- function(form) {
  if (!exists(form, envir = taper_test_cache, inherits = FALSE)) {
    assign(
      form,
      fit_taper(simulate_taper_test_data(form), form = form),
      envir = taper_test_cache
    )
  }
  get(form, envir = taper_test_cache, inherits = FALSE)
}

register_taper_test_model <- function(id, form, coefficients, bark_ratio = 0.9) {
  model <- taper_model_from_coefficients(
    id, form, coefficients, bark_ratio = bark_ratio
  )
  register_taper_model(model)
  model
}
