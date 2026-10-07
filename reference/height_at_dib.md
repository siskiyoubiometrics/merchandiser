# Locate a specified inside bark diameter

Locate the height in feet corresponding to an inside bark diameter in
inches. Use the result to set a stem-section boundary under the selected
taper equation. The default compatibility setting selects the highest
crossing, while source compatibility can reproduce a different source
result. Scalar inputs recycle across trees, and invalid rows retain
their input position and a status.

## Usage

``` r
height_at_dib(
  dbh,
  ht,
  dib,
  spcd,
  model = NULL,
  ...
)
```

## Arguments

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid measurement rows return missing results with a
  status.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default.
  Measurement heights and section bounds must fall within the tree.

- dib:

  Target diameter on the named bark basis, in inches. Accepts positive
  numeric values. Required, with no default. The default compatibility
  setting selects the highest crossing when the profile reaches the
  diameter more than once. Source compatibility can reproduce a
  different source inverse result.

- spcd:

  Species identifier as a numeric vector of positive whole-number codes.
  Required, with no default. The species must be recognized for species
  properties and within the selected equation's scope.

- model:

  Taper equation identifier as a character vector. Defaults to `NULL`,
  selecting the stored species default. A supplied identifier overrides
  that choice. Scalar identifiers recycle across trees.

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

A data frame in input order with `value` (height above ground, feet) and
`status` (integer code described by
[`status_codes()`](https://siskiyoubiometrics.com/merchandiser/reference/status_codes.md)).
Missing results remain in the table. Status 102 can accompany a retained
value when multiple profile crossings exist.

## Examples

``` r
## Measure the first shipped tree with its stored equation
height_at_dib(dbh = example_trees$dbh[1],
              ht = example_trees$ht[1],
              spcd = example_trees$spcd[1],
              dib = 6,
              model = example_trees$model[1])
#>      value status
#> 1 34.11951      0
```
