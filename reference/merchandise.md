# Select and scale logs from measured trees

Select logs within available stem sections using explicit product
dimensions and eligibility limits. The default cascade follows product
order. Optimization maximizes priced log value within the candidate
lengths and segment boundaries. Physical end diameters include trim, but
scale uses the nominal body and records trim as residual.

## Usage

``` r
merchandise(
  tree_id,
  dbh,
  ht,
  spcd,
  products,
  model = NULL,
  taper_map = NULL,
  age = NULL,
  pruned_ht = NULL,
  defects = NULL,
  stump_ht = 1,
  strategy = 'cascade',
  quiet = FALSE,
  ...
)
```

## Arguments

- tree_id:

  Tree identifiers as a unique, nonmissing atomic vector. Required, with
  no default. Identifiers link logs, residuals, defects, and status to
  the supplied trees.

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid measurement rows return missing results with a
  status.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default.
  Measurement heights and section bounds must fall within the tree.

- spcd:

  Species identifier as a numeric vector of positive whole-number codes.
  Required, with no default. The species must be recognized for species
  properties and within the selected equation's scope.

- products:

  Validated product rows from
  [`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md)
  or
  [`products()`](https://siskiyoubiometrics.com/merchandiser/reference/products.md).
  Required, with no default. Names must be unique, and row order
  supplies priority for the default cascade strategy.

- model:

  Taper equation identifier as a character vector. Defaults to `NULL`,
  selecting the stored species default. A supplied identifier overrides
  that choice. Scalar identifiers recycle across trees.

- taper_map:

  Species-to-model assignments as a data frame with unique numeric
  `spcd` and character `model` columns. Defaults to `NULL`, using
  shipped defaults. Missing mapped species receive a status rather than
  falling back. Explicit `model` takes precedence, but a supplied map is
  still validated.

- age:

  Tree age in years as a nonnegative numeric vector. Defaults to `NULL`,
  leaving age unspecified. Required when any product has an age limit.

- pruned_ht:

  Pruned height above ground, in feet, as finite nonnegative numeric
  values no greater than total height. Defaults to `NULL`. Required when
  a product has `requires_pruned = TRUE`.

- defects:

  Located defect records with the columns returned by
  [`defect()`](https://siskiyoubiometrics.com/merchandiser/reference/defect.md).
  Defaults to `NULL`, adding no defects. Records must use the same tree
  identifier type and valid product names for restrictions.

- stump_ht:

  Stump height above ground, in feet. Accepts finite, nonnegative
  numeric values below total height. Defaults to `1`. Cutting starts
  above this height, subject to located defects. The stump remains in
  the residual record.

- strategy:

  Log selection strategy as one character string. Accepts `'cascade'`
  (the default) or `'optimize'`. Cascade follows product order.
  Optimization requires finite positive prices for every product and
  selects the greatest total value within each segment.

- quiet:

  Whether to suppress aggregated status warnings. Accepts one `TRUE` or
  `FALSE`, without a missing value. Defaults to `FALSE`. Recorded status
  rows remain available when warnings are suppressed.

- ...:

  Additional named inputs accepted by the selected model, with none
  supplied by default. Numeric inputs must be finite: positive
  `upper_ht1`, `upper_ht2`, and `site_index` use feet, positive
  `upper_d1` and `upper_d2` use inches, and positive `basal_area` uses
  square feet per acre. `form_class` accepts positive numbers.
  `bark_ratio` is inside diameter divided by outside diameter, greater
  than zero and no greater than one. `decay_class` accepts whole numbers
  from 0 through 5 and `cull` accepts percentages from 0 through 100.
  `upper_bark` accepts `'ib'` or `'ob'`. Upper heights and diameters
  must be supplied in pairs. Only inputs declared by the selected model
  are accepted.

## Value

A `merch_result` list containing:

- `logs`: `tree_id` (identifier), `log` (sequence within tree),
  `product` (label), `start_height` and `end_height` (physical ends,
  feet), `length` (nominal feet), `scaling_length` (rounded feet), `sed`
  and `led` (physical end diameters, inches), `scaling_diameter` (board
  foot scaling diameter inside bark, inches, otherwise missing),
  `inside_bark` (eligibility basis), `scale` (quantity in
  `volume_unit`), and `volume_unit` (rule label). When any product is
  priced, `value` gives currency amounts and is missing for unpriced
  products. Scale units are board feet, cubic feet, cords, or green
  short tons according to the rule. Do not total unlike units.

- `residuals`: `tree_id`, `start_height`, `end_height` (feet), and
  `cause`. Causes include stump, trim, top, end, cull, restricted,
  short_remainder, diameter_limit, and no_entry_product.

- `status`: reported conditions only, with `tree_id`, `status` (integer
  code), `name`, and `description`. A clean run has no rows.

- `assumptions`: the columns and units documented in
  [`assumptions()`](https://siskiyoubiometrics.com/merchandiser/reference/assumptions.md).

- `call`: validated inputs and selected models needed to reproduce the
  calculation.

## Details

Optimization ties favor fewer logs, then longer lower logs and lower
physical ends. Remaining ties compare serialized start heights, ends,
cut kinds, nominal lengths, and product names in byte order. Values
within eight machine epsilons times the larger absolute value are
treated as ties. Log counts restart at cull and restriction boundaries.
Independently capped products multiply the possible states, and off-grid
preparation rejects more than 50,000 cut positions.

## Examples

``` r
## Define an unpriced cubic-foot product
saw <- product(product = 'saw',  ## product label
               min_length = 16,  ## feet
               max_length = 32,  ## feet
               min_sed = 6,  ## inches inside bark
               volume_unit = 'cubic')  ## cubic feet

## Select logs from the shipped trees
result <- merchandise(tree_id = example_trees$tree_id,
                      dbh = example_trees$dbh,
                      ht = example_trees$ht,
                      spcd = example_trees$spcd,
                      products = saw,
                      model = example_trees$model)

## Inspect selected logs
head(result$logs)
#>   tree_id log product start_height end_height length scaling_length       sed
#> 1       1   1     saw            1       33.0   32.0             32  6.226943
#> 2       2   1     saw            1       33.0   32.0             32  8.649995
#> 3       3   1     saw            1       33.0   32.0             32 10.373030
#> 4       3   2     saw           33       55.5   22.5             22  6.022404
#> 5       4   1     saw            1       33.0   32.0             32 12.768666
#> 6       4   2     saw           33       64.5   31.5             31  6.001249
#>        led scaling_diameter inside_bark     scale volume_unit
#> 1 13.01220               NA        TRUE 14.774535       cubic
#> 2 14.89709               NA        TRUE 23.296636       cubic
#> 3 17.09747               NA        TRUE 28.825139       cubic
#> 4 10.37303               NA        TRUE  8.719825       cubic
#> 5 19.46847               NA        TRUE 41.335071       cubic
#> 6 12.76867               NA        TRUE 16.632820       cubic
```
