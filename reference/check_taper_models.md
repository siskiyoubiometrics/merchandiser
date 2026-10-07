# Check model identifiers, scope, and required inputs

Diagnose unavailable equations, incompatible species, missing auxiliary
measurements, and invalid auxiliary values. This does not assess
statistical fit accuracy.

## Usage

``` r
check_taper_models(
  model,
  spcd = NULL,
  ...
)
```

## Arguments

- model:

  Equation identifiers as a character vector. Required, without a
  default. Scalar identifiers recycle to the supplied species or
  auxiliary input count.

- spcd:

  Numeric species codes to check against model scope, or `NULL`.
  Defaults to `NULL`, omitting scope checks. Other model and auxiliary
  checks still run.

- ...:

  Additional named inputs accepted by the selected model, with none
  supplied by default. Numeric inputs must be finite: positive
  `upper_ht1`, `upper_ht2`, and `site_index` use feet, positive
  `upper_d1` and `upper_d2` use inches, and positive `basal_area` uses
  square feet per acre. `form_class` accepts positive numbers.
  `bark_ratio` is inside diameter divided by outside diameter, greater
  than zero and no greater than one. `decay_class` accepts whole numbers
  from 1 through 5 and `cull` accepts percentages from 0 through 100.
  `upper_bark` accepts `'ib'` or `'ob'`. Upper heights and diameters
  must be supplied in pairs. Only inputs declared by the selected model
  are accepted.

## Value

A data frame containing problem rows only, with `row` (input position),
`id` (model identifier), `status` (integer code), `problem`
(description), and `input` (related input name). An empty table means no
checked problem was found.

## Examples

``` r
## Check the equations and species stored with the example trees
check_taper_models(model = example_trees$model,
                   spcd = example_trees$spcd)
#> [1] row     id      status  problem input  
#> <0 rows> (or 0-length row.names)
```
