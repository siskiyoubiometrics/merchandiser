# Define log dimensions, eligibility, and pricing for a product

Define the tree and log limits, scaling rule, and optional price for a
product. Returns a validated product row for
[`merchandise()`](https://siskiyoubiometrics.com/merchandiser/reference/merchandise.md).
Combine rows with
[`products()`](https://siskiyoubiometrics.com/merchandiser/reference/products.md)
to set cutting priority under the default strategy.

## Usage

``` r
product(
  product,
  spcd = NULL,
  min_dbh = 0,
  max_dbh = NA_real_,
  min_age = NA_real_,
  max_age = NA_real_,
  requires_pruned = FALSE,
  min_length,
  max_length,
  length_round = 1,
  trim = 0,
  min_sed,
  max_sed = NA_real_,
  min_led = 0,
  max_led = NA_real_,
  inside_bark = TRUE,
  max_sweep = NA_real_,
  max_logs = NA_integer_,
  volume_unit,
  split_scale = FALSE,
  round = 'default',
  cord_solid_fraction = NA_real_,
  price = NA_real_,
  price_per = 1
)
```

## Arguments

- product:

  Product label as a single nonempty character string. Required, with no
  default. Names must be unique when rows are combined and must match
  names used in defect restrictions.

- spcd:

  Species allowed for this product as a numeric vector of positive
  whole-number codes. Defaults to `NULL`, allowing all species. Tree
  calculations still require a usable taper equation and recognized
  species properties.

- min_dbh:

  Minimum outside bark diameter at breast height, in inches, for a tree
  to qualify. Accepts a single finite, nonnegative number. The lower
  limit is inclusive. Defaults to `0`, which imposes no additional
  minimum on a valid tree. Cannot exceed a supplied `max_dbh`.

- max_dbh:

  Maximum outside bark diameter at breast height, in inches, for tree
  eligibility. Accepts a single finite positive number or `NA`. The
  upper limit is exclusive. Defaults to `NA_real_`, imposing no maximum.
  Cannot be smaller than `min_dbh`.

- min_age:

  Minimum tree age, in years. Accepts a single finite nonnegative number
  or `NA`. The lower limit is inclusive. Defaults to `NA_real_`,
  imposing no minimum age. Requires `age` in
  [`merchandise()`](https://siskiyoubiometrics.com/merchandiser/reference/merchandise.md)
  and cannot exceed `max_age`.

- max_age:

  Maximum tree age, in years. Accepts a single finite nonnegative number
  or `NA`. The upper limit is exclusive. Defaults to `NA_real_`,
  imposing no maximum age. Requires `age` in
  [`merchandise()`](https://siskiyoubiometrics.com/merchandiser/reference/merchandise.md)
  and cannot be below `min_age`.

- requires_pruned:

  Whether logs must fit within the pruned portion of the tree. Accepts
  `TRUE` or `FALSE`, without missing values. Defaults to `FALSE`. When
  `TRUE`,
  [`merchandise()`](https://siskiyoubiometrics.com/merchandiser/reference/merchandise.md)
  requires `pruned_ht` and checks the physical log end against that
  height.

- min_length:

  Shortest nominal log length, in feet. Accepts one finite positive
  number. Required, with no default. Cannot exceed `max_length`, and the
  interval must include a candidate on the half-foot grid.

- max_length:

  Longest nominal log length, in feet. Accepts one finite positive
  number. Required, with no default. Cannot be below `min_length`. Trim
  occupies additional stem length beyond this nominal length.

- length_round:

  Increment for rounding scaling length down, in feet. Accepts one
  finite positive number. Defaults to `1`. Board foot rules accept only
  `1` or `2`. Cubic, cord, and green weight scales use nominal length
  even when reported scaling length differs.

- trim:

  Extra length occupied above the nominal log body, in feet. Accepts one
  finite nonnegative number. Defaults to `0`. Physical end diameters
  include trim, but scale excludes it and the wood remains in the
  residual record.

- min_sed:

  Minimum diameter at the physical small end of a candidate log, in
  inches. Accepts one finite nonnegative number. Required, with no
  default. The lower limit is inclusive, uses `inside_bark`, and cannot
  exceed `max_sed`.

- max_sed:

  Maximum diameter at the physical small end of a candidate log, in
  inches. Accepts one finite nonnegative number or `NA`. The upper limit
  is inclusive. Defaults to `NA_real_`, imposing no maximum. Uses
  `inside_bark` and cannot be below `min_sed`.

- min_led:

  Minimum diameter at the physical large end of a candidate log, in
  inches. Accepts a single finite, nonnegative number. The lower limit
  is inclusive. Defaults to `0`, which imposes no additional large end
  minimum. Uses the bark basis selected by `inside_bark` and cannot
  exceed a supplied `max_led`.

- max_led:

  Maximum diameter at the physical large end of a candidate log, in
  inches. Accepts one finite nonnegative number or `NA`. The upper limit
  is inclusive. Defaults to `NA_real_`, imposing no maximum. Uses
  `inside_bark` and cannot be below `min_led`.

- inside_bark:

  Bark basis for the log end diameter limits. Accepts `TRUE` or `FALSE`,
  with no missing value. Defaults to `TRUE` for inside bark. `FALSE`
  uses outside bark. Cubic and cord scales use the same basis. Board
  foot rules still use inside bark diameters, and green weight includes
  wood and attached bark. This setting does not change the tree diameter
  basis for `min_dbh`.

- max_sweep:

  Largest sweep percentage accepted by a log overlapping a sweep
  interval. Accepts one finite number from 0 through 100 or `NA`.
  Defaults to `NA_real_`, imposing no sweep limit. Comparison uses the
  percentage in
  [`defect()`](https://siskiyoubiometrics.com/merchandiser/reference/defect.md),
  without deducting it from scale.

- max_logs:

  Maximum number of logs from this product within each available stem
  segment. Accepts one positive whole number or `NA`. Defaults to
  `NA_integer_`, imposing no count limit. Counts restart at cull and
  restriction boundaries.

- volume_unit:

  Scaling rule as one character string. Accepts `'scribner'`,
  `'international'`, and `'doyle'` for board feet, `'cubic'` for cubic
  feet, `'cord'` for cords, or `'green_ton'` for green short tons.
  Required, with no default. The selected rule determines the scale and
  price basis.

- split_scale:

  Whether Scribner logs use shorter scaling sections. Accepts `TRUE` or
  `FALSE`, without missing values. Defaults to `FALSE`. It affects
  Scribner scale only and does not add physical cuts.

- round:

  Scaling diameter rounding as one character string. Accepts
  `'default'`, `'down'`, `'nearest'`, or `'none'`. Defaults to
  `'default'`, using the selected board foot rule's convention. Does not
  round tree measurements or physical eligibility limits. `none`
  bypasses preprocessing, but the source rule can still round
  internally.

- cord_solid_fraction:

  Solid volume divided by stacked cord volume. Accepts one finite number
  strictly between zero and one, or `NA`. Defaults to `NA_real_`. A
  finite fraction is required when `volume_unit = 'cord'` and converts
  the selected solid volume to cords.

- price:

  Amount per `price_per` scale units, in a caller-selected currency.
  Accepts one finite nonnegative number or `NA`. Defaults to `NA_real_`,
  leaving the product unpriced. Optimization requires a positive price
  on every product.

- price_per:

  Number of scale units covered by `price`. Accepts one finite positive
  number. Defaults to `1`. Log value is `scale * price / price_per`,
  using the unit selected by `volume_unit`.

## Value

A one-row `merch_products` data frame with all specification fields:

- `product`: label. `spcd`: list column of allowed numeric codes.

- `min_dbh`, `max_dbh`: outside bark diameter limits, inches.

- `min_age`, `max_age`: age limits, years. `requires_pruned`: pruning
  requirement.

- `min_length`, `max_length`, `length_round`, `trim`: lengths, feet.

- `min_sed`, `max_sed`, `min_led`, `max_led`: end diameter limits,
  inches.

- `inside_bark`: diameter basis. `max_sweep`: percent. `max_logs`: count
  per segment.

- `volume_unit`: scale rule. `split_scale`: split-scaling choice.
  `round`: rounding choice.

- `cord_solid_fraction`: solid-to-stacked volume fraction.

- `price`: currency amount. `price_per`: scale units per price.

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
