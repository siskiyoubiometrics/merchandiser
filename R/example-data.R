#' Example trees for stem calculations
#'
#' The trees are synthetic and illustrate package calculations.
#' @format A data frame with the following columns:
#' \describe{
#' \item{tree_id}{Tree identifier, unique within the example table.}
#' \item{spcd}{Numeric inventory species code.}
#' \item{dbh}{Diameter at breast height outside bark, inches.}
#' \item{ht}{Total tree height above ground, feet.}
#' \item{model}{Registered taper model identifier.}
#' }
"example_trees"

#' Example Pacific Northwest trees
#'
#' The trees are synthetic and illustrate package calculations.
#' @format A data frame with the following columns:
#' \describe{
#' \item{stand}{Stand identifier, text.}
#' \item{plot}{Plot identifier, text.}
#' \item{tree_id}{Tree identifier, unique within the example table.}
#' \item{spcd}{Numeric inventory species code.}
#' \item{dbh}{Diameter at breast height outside bark, inches.}
#' \item{ht}{Total tree height above ground, feet.}
#' \item{ht_status}{Height origin, measured or predicted.}
#' \item{age}{Tree age, years.}
#' \item{longitude}{Longitude, decimal degrees.}
#' \item{latitude}{Latitude, decimal degrees.}
#' }
"example_trees_pnw"

#' Example southern plantation trees
#'
#' The trees are synthetic and illustrate package calculations.
#' @format A data frame with the following columns:
#' \describe{
#' \item{stand}{Stand identifier, text.}
#' \item{plot}{Plot identifier, text.}
#' \item{tree_id}{Tree identifier, unique within the example table.}
#' \item{spcd}{Numeric inventory species code.}
#' \item{dbh}{Diameter at breast height outside bark, inches.}
#' \item{ht}{Total tree height above ground, feet.}
#' \item{age}{Tree age, years.}
#' \item{saw_stop}{Height where only pulpwood may be cut above it, feet. Missing means no stop.}
#' \item{pulp_stop}{Height where merchandising ends, feet. Missing means no stop.}
#' \item{jump_butt}{Height above a cull butt section, feet. Missing means no jump.}
#' \item{longitude}{Longitude, decimal degrees.}
#' \item{latitude}{Latitude, decimal degrees.}
#' }
"example_trees_south"

#' Example Pacific Northwest defect records
#'
#' The trees are synthetic and illustrate package calculations.
#' @format A data frame with the following columns:
#' \describe{
#' \item{tree_id}{Identifier linking this record to tree_id in example_trees_pnw.}
#' \item{start_height}{Start of the defect section above ground, feet.}
#' \item{end_height}{End of the defect section above ground, feet. Missing means the top of the
#'   tree.}
#' \item{effect}{Cutting effect, cull, restrict, end, or sweep.}
#' \item{product}{Required product name for restrict, pulp in this example. Missing for other
#'   effects.}
#' \item{percent}{Whole-number sweep percentage. Missing for other effects.}
#' }
"example_defects_pnw"

#' Example stem measurements
#'
#' The trees are synthetic and illustrate package calculations.
#' @format A data frame with the following columns:
#' \describe{
#' \item{tree_id}{Tree identifier, repeated for measurements on the same tree.}
#' \item{spcd}{Numeric inventory species code.}
#' \item{dbh}{Diameter at breast height outside bark, inches.}
#' \item{ht}{Total tree height above ground, feet.}
#' \item{h}{Measurement height above ground, feet.}
#' \item{dib}{Diameter inside bark, inches.}
#' }
"example_stem_measurements"

#' Shipped default taper models by species
#'
#' Species-specific models are selected from Region 6, then Region 8, then Region 9 lookups.
#' A default must resolve to exactly that species and require no extra measurements.
#' Species without an accepted model have no row.
#' @format A data frame with the following columns:
#' \describe{
#' \item{spcd}{Integer inventory species code.}
#' \item{model}{Character taper model identifier.}
#' \item{source}{Character lookup call that supplied the model.}
#' }
#' @export
#' @examples
#' ## Inspect the shipped defaults for the example species
#' default_taper_models[default_taper_models$spcd %in% example_trees$spcd, ]
"default_taper_models"
