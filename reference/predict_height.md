# Predict tree heights and optional intervals

Predict total height from a fitted relationship. Conditional predictions
use known group adjustments, while missing or unseen groups use
population values. Intervals simulate group variation and residual
error, holding fixed coefficients constant. They exclude coefficient
uncertainty and inventory sampling uncertainty.

## Usage

``` r
predict_height(
  fit,
  dbh,
  spcd,
  group = NULL,
  re_form = 'conditional',
  interval = FALSE,
  level = 0.95,
  nsim = 1000,
  seed = NULL
)
```

## Arguments

- fit:

  Fitted height relationship returned by
  [`fit_height()`](https://siskiyoubiometrics.com/merchandiser/reference/fit_height.md).
  Required, with no default. Predictions use its species mapping and
  fitted group adjustments.

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid rows return missing heights, with warnings for
  invalid finite inputs.

- spcd:

  Species identifiers as numeric positive whole-number codes within the
  R integer range. Required, without a default. A code absent from the
  fitted species mapping returns a missing prediction with a warning.

- group:

  Group identifiers for shared adjustments, supplied as an atomic vector
  matching the observations. Defaults to `NULL`, with no separate group
  assignments. Known groups use their fitted adjustments for conditional
  predictions, while unseen groups use the population relationship.

- re_form:

  Adjustment basis as one character string. Accepts `'conditional'` or
  `'population'`. Defaults to `'conditional'`, using available fitted
  group effects. Population predictions omit those fitted adjustments.

- interval:

  Whether to return simulated prediction intervals. Accepts one `TRUE`
  or `FALSE`. Defaults to `FALSE`, returning a numeric vector. `TRUE`
  returns fitted values and interval endpoints, with lower endpoints
  constrained to breast height.

- level:

  Coverage requested for prediction intervals, as a single finite
  fraction strictly between zero and one. Defaults to `0.95`. Used only
  when `interval = TRUE`.

- nsim:

  Simulation count for intervals as one positive whole number, no
  greater than 1000000. Defaults to `1000`. More simulations refine the
  simulated quantiles without adding coefficient uncertainty.

- seed:

  Random seed as one nonnegative whole number within the R integer
  range, or `NULL`. Defaults to `NULL`, using the current random state.
  A supplied seed makes interval simulation repeatable.

## Value

With `interval = FALSE`, a numeric vector of heights in feet. With
`TRUE`, a data frame with `fit`, `lower`, and `upper`, all in feet.
Input order is retained. Species absent from the fit return missing
values with a warning.

## Examples

``` r
## Fit and predict heights on the shipped example trees
predict_height(fit = fit_height(dbh = example_trees$dbh,
                                ht = example_trees$ht,
                                spcd = example_trees$spcd),
               dbh = example_trees$dbh,
               spcd = example_trees$spcd)
#> [1]  60.01570  69.98356  79.98629  90.00321 100.01820 119.99331
```
