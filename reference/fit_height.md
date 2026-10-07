# Fit height relationships by species and group

Fit pooled and species height relationships, optionally with group
adjustments. Invalid rows are omitted with a warning. Species below the
minimum sample size or with an unsuccessful fit use the pooled
relationship.

## Usage

``` r
fit_height(
  dbh,
  ht,
  spcd,
  group = NULL,
  form = 'chapman_richards',
  min_n = 7
)
```

## Arguments

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Rows with invalid measurements are omitted with a warning.

- ht:

  Measured total height above ground, in feet. Accepts numeric values
  greater than zero and no greater than 500. Required, with no default.
  Valid measurements supply the response for pooled and species fits.

- spcd:

  Species identifiers as numeric positive whole-number codes within the
  R integer range and present in species_reference. Required, without a
  default. Codes group observations for species fits and need not have a
  registered taper equation. Unknown species rows are omitted with a
  warning.

- group:

  Group identifiers for shared adjustments, supplied as an atomic vector
  matching the observations. Defaults to `NULL`, with no separate group
  assignments. Known groups use their fitted adjustments for conditional
  predictions, while unseen groups use the population relationship.

- form:

  Height relationship as one character string. Accepts
  `'chapman_richards'`, `'curtis'`, `'wykoff'`, `'naslund'`, or
  `'schumacher'`. Defaults to `'chapman_richards'`. The selected form is
  used for pooled and species fits.

- min_n:

  Minimum valid observations for a separate species fit, as one positive
  whole number. Defaults to `7`. Species with fewer observations use the
  pooled model.

## Value

A `height_fit` list containing:

- `data`: retained `dbh` (inches), `ht` (feet), `spcd` (code), and
  `group` (identifier).

- `fixed_effects`: `spcd` (code), `model_key` (identifier), `pooled`
  (indicator), and equation coefficients `a`, `b`, `c` on the
  form-specific scale evaluated with diameter in inches and height in
  feet. Unused coefficients are missing.

- `variance_components`: `spcd`, `model_key`, `random_parameter`,
  `random_effect_sd` (coefficient scale), and `residual_sd` (feet).

- `random_effects`: `model_key`, `group`, and `effect` (internal
  coefficient scale).

- `n_by_species`: `spcd` and `n` (observation count).

- `form`, `min_n`, `groups_seen`, `pooled_species`,
  `convergence_pooled_species`, `measurement_system`, and
  `package_version`: fitting choices and provenance.

- `models`, `model_map`, and `internal_fixed_effects`: fitted model
  objects, species-to-model assignments, and coefficients used for
  prediction.

## Examples

``` r
## Select measured heights from the shipped list
library(dplyr)

## Retain the tree rows used in this calculation
trees <- example_trees_pnw %>%
  filter(ht_status == 'measured')

## Fit and inspect the relationship with plot adjustments
fit_height(dbh = trees$dbh,
           ht = trees$ht,
           spcd = trees$spcd,
           group = trees$plot)
#> <height_fit>
#>   form: chapman_richards
#>   measured trees: 100
#>   species: 2
#>   groups: 10
#>   below-minimum pooled species: 0
```
