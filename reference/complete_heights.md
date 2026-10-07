# Fill missing heights while retaining measured values

Complete nonfinite heights from a fitted diameter-height relationship.
Measured heights are preserved only on rows with valid diameter, height,
and species inputs. Invalid finite rows generate a warning and remain
missing.

## Usage

``` r
complete_heights(
  dbh,
  ht,
  spcd,
  group = NULL,
  fit = NULL
)
```

## Arguments

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid rows return missing heights, with warnings for
  invalid finite inputs.

- ht:

  Measured total height above ground, in feet. Accepts numeric values
  greater than zero and no greater than 500, or nonfinite values to be
  predicted. Required, with no default. Invalid finite rows remain
  missing. Nonfinite heights are completed from the supplied fit, or
  from a fit to the remaining valid observations.

- spcd:

  Species identifiers as numeric positive whole-number codes within the
  R integer range. Required, without a default. Codes select
  relationships in the supplied fit or group observations when a new fit
  is needed.

- group:

  Group identifiers for shared adjustments, supplied as an atomic vector
  matching the observations. Defaults to `NULL`, with no separate group
  assignments. Known groups use their fitted adjustments for conditional
  predictions, while unseen groups use the population relationship.

- fit:

  Height fit returned by
  [`fit_height()`](https://siskiyoubiometrics.com/merchandiser/reference/fit_height.md),
  or `NULL`. Defaults to `NULL`, fitting the available valid heights
  with the supplied species and groups. Supplying a fit avoids
  refitting.

## Value

A numeric vector of total heights in feet, in input order. Measured
values on rows with valid diameter, height, and species inputs are
unchanged. Missing predictions remain `NA` when the fit cannot cover a
species or an input is invalid.

## Examples

``` r
## Remove stored predictions while preserving measurements
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

## Retain the tree rows used in this calculation
trees <- example_trees_pnw %>%
  mutate(observed = if_else(ht_status == 'predicted', NA_real_, ht))

## Refit and inspect completed heights with the original plot groups
head(complete_heights(dbh = trees$dbh,
                      ht = trees$observed,
                      spcd = trees$spcd,
                      group = trees$plot))
#> Warning: fit_height(): 200 rows with missing or nonfinite dbh, ht, spcd, or group were omitted.
#> [1] 141.80000  70.22935  81.27289  47.30000 173.68144  67.61414
```
