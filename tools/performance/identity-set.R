# Run against an installed package with R_LIBS selecting the build.
library(merchandiser)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
out <- args[1L]
invisible(threads(as.integer(args[2L])))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
save_result <- function(name, value) {
  active <- merchandiser:::.tv_registry$active
  value <- force(value)
  stopifnot(identical(merchandiser:::.tv_registry$active, active))
  saveRDS(value, file.path(out, paste0(name, ".rds")))
  cat(name, "saved at", threads(), "threads\n")
}

# Product definitions copied verbatim from README.Rmd:43-84.
large <- product(product = 'Large sawlog',
                 min_length = 32,  ## feet
                 max_length = 32,  ## feet
                 min_sed = 12,  ## inches, small end diameter
                 inside_bark = TRUE,  ## diameter limit is inside bark
                 trim = 0.5,  ## feet added to each log
                 volume_unit = 'scribner',  ## Scribner board feet
                 price = 900,  ## dollars
                 price_per = 1000)  ## per thousand board feet

medium <- product(product = 'Medium sawlog',
                  min_length = 16,  ## feet
                  max_length = 16,  ## feet
                  min_sed = 10,  ## inches, small end diameter
                  inside_bark = TRUE,  ## diameter limit is inside bark
                  trim = 0.5,  ## feet added to each log
                  volume_unit = 'scribner',  ## Scribner board feet
                  price = 700,  ## dollars
                  price_per = 1000)  ## per thousand board feet

small <- product(product = 'Small sawlog',
                 min_length = 16,  ## feet
                 max_length = 16,  ## feet
                 min_sed = 6,  ## inches, small end diameter
                 inside_bark = TRUE,  ## diameter limit is inside bark
                 trim = 0.5,  ## feet added to each log
                 volume_unit = 'scribner',  ## Scribner board feet
                 price = 500,  ## dollars
                 price_per = 1000)  ## per thousand board feet

pulp <- product(product = 'Pulp',
                min_length = 8,  ## feet
                max_length = 20,  ## feet
                min_sed = 3,  ## inches, small end diameter
                inside_bark = TRUE,  ## diameter limit is inside bark
                trim = 0.5,  ## feet added to each log
                volume_unit = 'green_ton',  ## green short tons
                price = 30,  ## dollars
                price_per = 1)  ## per ton

## Combine the products in cutting priority order
specifications <- products(large, medium, small, pulp)
readme_products <- specifications

# Product definitions copied verbatim from vignettes/pacific-northwest.Rmd.
export <- product(product = 'export',  ## product name, unitless
                  spcd = c(202, 263),  ## species codes, unitless
                  min_dbh = 12,  ## inches
                  min_length = 32,  ## feet
                  max_length = 40,  ## feet
                  trim = 1,  ## feet
                  min_sed = 12,  ## inches
                  inside_bark = TRUE,  ## logical, unitless
                  volume_unit = 'scribner',  ## board feet
                  split_scale = FALSE)  ## logical, unitless

## Define domestic saw specifications
domestic_saw <- product(product = 'domestic_saw',  ## product name, unitless
                        spcd = c(202, 263),  ## species codes, unitless
                        min_dbh = 10,  ## inches
                        min_length = 16,  ## feet
                        max_length = 40,  ## feet
                        trim = 1,  ## feet
                        min_sed = 6,  ## inches
                        inside_bark = TRUE,  ## logical, unitless
                        volume_unit = 'scribner',  ## board feet
                        split_scale = FALSE)  ## logical, unitless

## Define pulp specifications
pulp <- product(product = 'pulp',  ## product name, unitless
                spcd = c(202, 263),  ## species codes, unitless
                min_dbh = 5,  ## inches
                min_length = 8,  ## feet
                max_length = 40,  ## feet
                trim = 0.5,  ## feet
                min_sed = 3,  ## inches
                inside_bark = TRUE,  ## logical, unitless
                volume_unit = 'green_ton')  ## green short tons

## Combine products in cutting priority order
specifications <- products(export = export,
                           domestic_saw = domestic_saw,
                           pulp = pulp)
pnw_products <- specifications

# Product definitions copied verbatim from vignettes/southern.Rmd.
sawtimber <- product(product = 'sawtimber',  ## product name, unitless
                     spcd = 131,  ## species code, unitless
                     min_dbh = 12,  ## inches
                     min_length = 16,  ## feet
                     max_length = 40,  ## feet
                     trim = 0.5,  ## feet
                     min_sed = 8,  ## inches
                     inside_bark = FALSE,  ## logical, unitless
                     max_logs = 1,  ## logs per segment
                     volume_unit = 'green_ton')  ## green short tons

## Define chip n saw specifications
chip_n_saw <- product(product = 'chip_n_saw',  ## product name, unitless
                      spcd = 131,  ## species code, unitless
                      min_dbh = 8,  ## inches
                      max_dbh = 12,  ## inches
                      min_length = 16,  ## feet
                      max_length = 40,  ## feet
                      trim = 0.5,  ## feet
                      min_sed = 6,  ## inches
                      inside_bark = FALSE,  ## logical, unitless
                      max_logs = 1,  ## logs per segment
                      volume_unit = 'green_ton')  ## green short tons

## Define pulpwood specifications
pulpwood <- product(product = 'pulpwood',  ## product name, unitless
                    spcd = 131,  ## species code, unitless
                    min_dbh = 5,  ## inches
                    max_dbh = 8,  ## inches
                    min_length = 8,  ## feet
                    max_length = 40,  ## feet
                    trim = 0.5,  ## feet
                    min_sed = 3,  ## inches
                    inside_bark = FALSE,  ## logical, unitless
                    max_logs = 1,  ## logs per segment
                    volume_unit = 'green_ton')  ## green short tons

## Combine products in cutting priority order
specifications <- products(sawtimber = sawtimber,
                           chip_n_saw = chip_n_saw,
                           pulpwood = pulpwood)
south_products <- specifications

trees <- example_trees_south
records <- defects_from_stoppers(
  tree_id = trees$tree_id,
  ht = trees$ht,
  topwood_product = "pulpwood",
  saw_stop = trees$saw_stop,
  pulp_stop = trees$pulp_stop,
  jump_butt = trees$jump_butt
)
for (pulp_first in c(FALSE, TRUE)) {
  order_products <- function(p) {
    if (pulp_first) p[c(nrow(p), seq_len(nrow(p) - 1L)), ] else p
  }
  suffix <- if (pulp_first) "pulp_first" else "page_order"
  for (strategy in c("cascade", "optimize")) {
    trees <- example_trees
    save_result(paste("readme", strategy, suffix, sep = "_"), merchandise(
      trees$tree_id, trees$dbh, trees$ht, trees$spcd,
      order_products(readme_products), model = trees$model, strategy = strategy,
      quiet = TRUE
    ))
  }
  trees <- example_trees_pnw
  for (with_defects in c(FALSE, TRUE)) {
    save_result(paste("pnw", with_defects, suffix, sep = "_"), merchandise(
      trees$tree_id, trees$dbh, trees$ht, trees$spcd,
      order_products(pnw_products), age = trees$age,
      defects = if (with_defects) example_defects_pnw else NULL, quiet = TRUE
    ))
  }
  trees <- example_trees_south
  save_result(paste("south", suffix, sep = "_"), merchandise(
    trees$tree_id, trees$dbh, trees$ht, trees$spcd,
    order_products(south_products), age = trees$age, defects = records, quiet = TRUE
  ))
}
for (name in c("example_trees", "example_trees_pnw", "example_trees_south")) {
  trees <- get(name)
  common <- list(dbh = trees$dbh, ht = trees$ht, spcd = trees$spcd)
  if (!is.null(trees$model)) common$model <- trees$model
  save_result(paste0(name, "_profile"), do.call(stem_profile, c(
    common, list(tree_id = trees$tree_id, step = 0.5)
  )))
  for (h in c(4.5, 17.3, 33)) {
    save_result(paste(name, "dib", h, sep = "_"), do.call(dib, c(common, list(h = h))))
    if (name != "example_trees_south") {
      save_result(paste(name, "dob", h, sep = "_"), do.call(dob, c(common, list(h = h))))
    }
  }
  for (target in c(6, 4)) {
    save_result(paste(name, "height", target, sep = "_"), do.call(
      height_at_dib, c(common, list(dib = target))
    ))
  }
  save_result(paste0(name, "_volume"), do.call(stem_volume, common))
  save_result(paste0(name, "_volume_to_6"), do.call(stem_volume, c(common, list(to_dib = 6))))
}
