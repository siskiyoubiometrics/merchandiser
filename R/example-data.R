#' Example trees from the Pacific Northwest
#'
#' Synthetic tree measurements for [complete_heights()], [biomass()], and [merchandise()].
#' `ht_observed` retains a subset of `ht_simulated` for height fitting.
#' @details The generator is `data-raw/example-trees.R`. Locations, ages, pruning flags,
#' and expansion factors are synthetic inputs.
#' @format A data frame. No values are missing
#'   except unmeasured `ht_observed` values.
#' \describe{
#'   \item{stand}{Character stand label, unitless.}
#'   \item{plot}{Character synthetic plot label, unitless.}
#'   \item{tree}{Integer tree number, unique within this example set.}
#'   \item{spcd}{Integer Forest Inventory and Analysis species code, unitless.}
#'   \item{species}{Character common species name, unitless.}
#'   \item{dbh}{Numeric diameter at breast height outside bark, inches.}
#'   \item{ht_observed}{Numeric retained synthetic total height above ground, feet. Missing values
#' mark withheld heights.}
#'   \item{ht_simulated}{Numeric generated total height above ground, feet.}
#'   \item{age}{Integer assumed age in years.}
#'   \item{pruned}{Logical synthetic pruning indicator.}
#'   \item{region}{Integer Forest Service region code, unitless.}
#'   \item{forest}{Integer Forest Service forest code, unitless.}
#'   \item{district}{Integer Forest Service district code, unitless.}
#'   \item{longitude}{Numeric assumed longitude, decimal degrees east.}
#'   \item{latitude}{Numeric assumed latitude, decimal degrees north.}
#'   \item{expansion_factor}{Numeric example expansion factors in trees per acre.}
#' }
#' @source `data-raw/example-trees.R` contains the height coefficients and source references.
#' @seealso [example_trees] for a smaller complete inventory,
#'   [example_trees_south] for stopping-height examples,
#'   [fit_height()] to estimate missing heights from measured trees.
#' @usage
#'
#' ## Call signatures
#' example_trees_pnw
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the shipped measurements
#' example_trees_pnw %>%
#'   select(tree, species, dbh, ht_observed) %>%
#'   slice_head(n = 3)
#' @section Status and missing values:
#' The data carry no calculation statuses. Missing heights in `ht_observed` are withheld from
#' fitting. Missing stopping heights produce no corresponding restriction in
#' [defects_from_stoppers()].
#' @section Using the supplied measurements:
#' The measurements use imperial units. `units = 'metric'` declares input units without converting
#' these data.
"example_trees_pnw"

#' Example trees from the South
#'
#' Synthetic tree measurements with stopping heights for [defects_from_stoppers()] and
#' [merchandise()]. `ht_observed` retains every simulated height.
#' @details The generator is `data-raw/example-trees.R`. Locations, ages, pruning flags,
#' and expansion factors are synthetic inputs.
#' @format A data frame. Only stopper fields
#'   contain missing values, indicating no recorded stopper of that kind.
#' \describe{
#'   \item{stand}{Character stand label, unitless.}
#'   \item{plot}{Character synthetic plot label, unitless.}
#'   \item{tree}{Integer tree number, unique within this example set.}
#'   \item{spcd}{Integer Forest Inventory and Analysis species code, unitless.}
#'   \item{species}{Character common species name, unitless.}
#'   \item{dbh}{Numeric diameter at breast height outside bark, inches.}
#'   \item{ht_observed}{Numeric retained synthetic total height above ground, feet. Missing values
#' mark withheld heights.}
#'   \item{ht_simulated}{Numeric generated total height above ground, feet.}
#'   \item{age}{Integer assumed age in years.}
#'   \item{pruned}{Logical synthetic pruning indicator.}
#'   \item{region}{Integer Forest Service region code, unitless.}
#'   \item{forest}{Integer Forest Service forest code, unitless.}
#'   \item{district}{Integer Forest Service district code, unitless.}
#'   \item{longitude}{Numeric assumed longitude, decimal degrees east.}
#'   \item{latitude}{Numeric assumed latitude, decimal degrees north.}
#'   \item{expansion_factor}{Numeric example expansion factors in trees per acre.}
#'   \item{saw_stop}{Numeric height above ground, feet. Wood above this
#'     stopper is restricted to pulp.
#'
#' Missing means no saw stopper.}
#'   \item{pulp_stop}{Numeric height above ground, feet. Wood above this
#'     stopper is culled. Missing means no pulp stopper.}
#'   \item{jump_butt}{Numeric height above ground, feet.
#'
#' Wood from stump to
#'     this height is culled. Missing means no jump butt.}
#' }
#' @source `data-raw/example-trees.R` contains the height coefficients and source references.
#' @seealso [example_trees_pnw] for unmeasured-height examples,
#'   [example_trees] for a small complete inventory,
#'   [defects_from_stoppers()] to translate the stopping-height columns.
#' @usage
#'
#' ## Call signatures
#' example_trees_south
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the shipped measurements
#' example_trees_south %>%
#'   select(tree, species, dbh, ht_observed) %>%
#'   slice_head(n = 3)
#' @section Status and missing values:
#' The data carry no calculation statuses. Missing heights in `ht_observed` are withheld from
#' fitting. Missing stopping heights produce no corresponding restriction in
#' [defects_from_stoppers()].
#' @section Using the supplied measurements:
#' The measurements use imperial units. `units = 'metric'` declares input units without converting
#' these data.
"example_trees_south"

#' Synthetic example trees
#'
#' Complete synthetic Douglas-fir and western hemlock measurements with assigned taper models. The
#' table supplies inputs for stem volume, biomass, and log calculations.
#' @details `data-raw/example-trees.R` records the measurements and model assignments.
#' @format A data frame without missing values:
#' \describe{
#'   \item{tree}{Integer tree number, unique within this example set.}
#'   \item{spcd}{Numeric Forest Inventory and Analysis species code, unitless.}
#'   \item{species}{Character common name, unitless.}
#'   \item{dbh}{Numeric diameter at breast height outside bark, inches.}
#'   \item{ht}{Numeric total height above ground, feet.}
#'   \item{model}{Character registered taper model identifier, unitless,
#'     `F00FW2W202` for Douglas-fir and `F03FW2W263` for western hemlock.}
#' }
#' @seealso [example_trees_pnw] for height completion examples,
#'   [example_trees_south] for stopping-height examples,
#'   [merchandise()] to calculate log volume.
#' @usage
#'
#' ## Call signatures
#' example_trees
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the shipped measurements
#' example_trees %>%
#'   select(tree, species, dbh, ht) %>%
#'   slice_head(n = 3)
#' @section Status and missing values:
#' The data have no missing measurements or calculation statuses.
#' @section Using the supplied measurements:
#' The measurements use imperial units. `units = 'metric'` declares input units without converting
#' these data.
"example_trees"
#' Modeled upper-stem measurements for taper fitting
#'
#' Inside-bark diameters calculated from the complete synthetic example inventory.
#' These are modeled measurements for demonstrating fitting, not field observations
#' or independent validation data. The generator is `data-raw/example-trees.R`.
#' @format A data frame with these columns:
#' \describe{
#'   \item{tree}{Integer tree number matching [example_trees], repeated by measurement.}
#'   \item{species}{Character common species name.}
#'   \item{dbh}{Numeric outside-bark breast height diameter, inches.}
#'   \item{ht}{Numeric total height above ground, feet.}
#'   \item{h}{Numeric measurement height above ground, feet.}
#'   \item{dib}{Numeric modeled diameter inside bark at the measurement height, inches.}
#' }
#' @seealso [fit_taper()] for fitting and [example_trees] for the complete inventory.
#' @usage
#'
#' ## Call signatures
#' example_stem_measurements
#' @examples
#' ## Load dplyr
#' library(dplyr)
#'
#' ## Inspect the shipped measurements
#' example_stem_measurements %>%
#'   select(tree, dbh, ht, h, dib) %>%
#'   slice_head(n = 3)
"example_stem_measurements"
