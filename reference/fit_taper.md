# Fit taper coefficients to repeated stem measurements

Estimate population or grouped taper relationships from measured inside
bark diameters. Public inputs and residual diagnostics use inches and
feet. Fitting stores coefficients in centimeters and meters. Invalid
measurements are omitted with a warning.

## Usage

``` r
fit_taper(
  tree_id,
  dbh,
  ht,
  h,
  dib,
  spcd,
  form = 'max_burkhart',
  group = NULL,
  weights = NULL,
  start = NULL
)
```

## Arguments

- tree_id:

  Tree identifiers repeated for measurements from the same tree, as an
  atomic vector. Required, without a default. Breast-height diameter and
  total height must remain constant within each identifier.

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid measurement rows are omitted with a warning.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default. Heights
  must be constant within each tree identifier, and its measurement
  heights cannot exceed this total.

- h:

  Measurement height above ground, in feet. Accepts numeric values from
  zero through total height, inclusive. Required, with no default.

- dib:

  Observed inside bark diameter at `h`, in inches. Accepts numeric
  values from zero through 400. Required, without a default. Nonfinite
  or invalid measurement rows are omitted with a warning.

- spcd:

  Species identifiers as numeric positive whole-number codes present in
  species_reference. Required, without a default. Retained observations
  determine the fitted model's species scope. Unrecognized species rows
  are omitted with a warning.

- form:

  Equation form as one character string. Accepts `'max_burkhart'`,
  `'kozak_1988'`, or `'kozak_2002'`. Defaults to `'max_burkhart'`. The
  form determines coefficient names, order, and constraints.

- group:

  Group identifiers matching measurement rows, as an atomic vector.
  Defaults to `NULL` for an ordinary nonlinear fit. Multiple groups
  request a mixed effects fit, with a random effect on `b1` for the
  segmented form or the Kozak scale parameter. A single group falls back
  with a warning.

- weights:

  Relative observation weights as positive finite numeric values
  matching measurements. Defaults to `NULL`, assigning equal weight.
  Larger weights increase a measurement's contribution to fitting.

- start:

  Starting coefficients as a finite named numeric vector matching the
  selected form, or `NULL`. Defaults to `NULL`, using the built-in
  starting search. Starts must satisfy the form's coefficient
  constraints.

## Value

A `taper_fit` list containing:

- `form`, `coefficients` (metric equation scale), `spcd` (species
  scope), `n_trees`, `n_measurements` (counts), `groups_seen`, `method`,
  `measurement_system`, `input_measurement_system`, and
  `package_version`.

- `fit_statistics`: `overall` and `by_relative_height` summaries with
  observation count `n`, diameter root mean squared error `rmse`, and
  mean residual `bias` in inches, plus `relative_height_class`
  (relative-height interval) for the latter table.

- `residual_diagnostics`: `tree_id`, `dbh` (inches), `ht` and `h`
  (feet), `relative_height` (height fraction), `observed_dib`,
  `fitted_dib`, `population_fitted_dib`, `residual` (inches),
  `standardized_residual` (residual divided by its standard deviation),
  `spcd`, and optional `group`.

- `random_effects`: `group` and `effect` on the internal coefficient
  scale.

- `convergence`: optimizer outcome, attempt counts, and stop messages.

- `model`, `internal_coefficients`, and `random_parameter`: fitted
  object and its internal parameterization.

- `data_metric`: retained `tree_id`, `dbh` and `dib` (centimeters), `ht`
  and `h` (meters), `species` (numeric code), `weight` (relative fitting
  weight), and optional `group` (identifier) or `p_fixed`
  (reference-height fraction). The plot method compares observed and
  fitted relative diameters. Predictions return inches and use
  population coefficients for unseen groups.

## Examples

``` r
## Fit a profile to the shipped stem measurements
fit <- fit_taper(tree_id = example_stem_measurements$tree_id,
                 dbh = example_stem_measurements$dbh,
                 ht = example_stem_measurements$ht,
                 h = example_stem_measurements$h,
                 dib = example_stem_measurements$dib,
                 spcd = example_stem_measurements$spcd)

## Inspect diameter errors in inches
fit$fit_statistics$overall
#>    n      rmse        bias
#> 1 90 0.2745199 0.003557597
```
