.mc_resolve_species <- function(x, name = "spcd", allow_na = TRUE) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    stop(name, " must contain numeric species codes.", call. = FALSE)
  }
  if (!allow_na && anyNA(x))
    stop(name, " cannot be missing.", call. = FALSE)
  as.double(x)
}

.mc_default_models <- function(spcd, model = NULL, taper_map = NULL) {
  spcd <- .mc_resolve_species(spcd)
  if (!is.null(taper_map)) {
    if (!is.data.frame(taper_map) || !all(c("spcd", "model") %in% names(
      taper_map
    )) || !is.numeric(taper_map$spcd) ||
      anyNA(taper_map$spcd) || anyDuplicated(taper_map$spcd) || !is.character(
      taper_map$model
    ) ||
      anyNA(taper_map$model)) {
      stop("taper_map needs unique numeric spcd and registered model columns.",
        call. = FALSE
      )
    }
    unregistered <- taper_map$model[!has_taper_model(taper_map$model)]
    if (length(unregistered)) {
      stop("taper_map names unregistered models: ", paste(unique(unregistered), collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (!is.null(model))
    return(model)
  if (!is.null(taper_map)) {
    return(taper_map$model[match(spcd, taper_map$spcd)])
  }
  result <- default_taper_models$model[match(spcd, default_taper_models$spcd)]
  result[is.na(result) & is.finite(spcd)] <- ""
  result
}

.mc_stem_inputs <- function(spcd, model, dots, outside = FALSE) {
  spcd <- .mc_resolve_species(spcd)
  model <- .mc_default_models(spcd, model)
  list(model = model, aux = c(list(spcd = spcd), dots))
}
