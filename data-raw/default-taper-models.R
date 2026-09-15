## Select species-specific defaults from the shipped source lookups.
library(merchandiser)

lookups <- data.frame(
  region = c(6L, 8L, 9L),
  forest = c(12L, 8L, 9L),
  district = 0L
)
rows <- list()
for (spcd in species_reference$spcd) {
  for (i in seq_len(nrow(lookups))) {
    candidate <- nvel_default_equation(
      region = lookups$region[i],
      forest = lookups$forest[i],
      district = lookups$district[i],
      spcd = spcd
    )
    if (!has_taper_model(candidate))
      next
    model <- get_taper_model(candidate)
    if (!identical(model$spcd, as.integer(spcd)) || length(model$inputs$required))
      next
    rows[[length(rows) + 1L]] <- data.frame(
      spcd = as.integer(spcd),
      model = candidate,
      source = paste0(
        "nvel_default_equation(",
        lookups$region[i],
        ", ",
        lookups$forest[i],
        ", ",
        lookups$district[i],
        ", spcd)"
      )
    )
    break
  }
}
default_taper_models <- do.call(rbind, rows)
rownames(default_taper_models) <- NULL
save(default_taper_models, file = "data/default_taper_models.rda", version = 2)

## Keep the exported table available to package functions without attaching data.
internal <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = internal)
internal$default_taper_models <- default_taper_models
save(list = ls(internal), envir = internal, file = "R/sysdata.rda", version = 2)
