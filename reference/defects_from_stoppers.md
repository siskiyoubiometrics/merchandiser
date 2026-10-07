# Convert stopper heights to located defects

Translate saw stops to restrictions, pulp stops to usable-stem ends, and
jump butts to culls. Supplied stops must remain above the stump, within
the tree, and in increasing physical order.

## Usage

``` r
defects_from_stoppers(
  tree_id,
  ht,
  topwood_product,
  saw_stop = NULL,
  pulp_stop = NULL,
  jump_butt = NULL,
  pulp_tree = FALSE,
  stump_ht = 1
)
```

## Arguments

- tree_id:

  Identifiers linking each defect to an input tree. Accepts a nonmissing
  atomic vector. Required, with no default. Its type must match the tree
  list when records are validated.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default.
  Measurement heights and section bounds must fall within the tree.

- topwood_product:

  Product allowed above a saw stop or on a whole pulp tree, as a
  nonempty character scalar. Required, without a default. The label must
  match the product used in subsequent merchandising.

- saw_stop:

  Height above which only `topwood_product` is allowed, in feet. Accepts
  positive numeric values or missing values. Defaults to `NULL`, adding
  no saw restriction. Must follow any jump butt and precede any pulp
  stop.

- pulp_stop:

  Height ending the usable stem, in feet. Accepts positive numeric
  values or missing values. Defaults to `NULL`, adding no end record.
  Must lie above other supplied stops and no higher than total height.

- jump_butt:

  Upper height of an unusable butt section, in feet. Accepts positive
  numeric values or missing values. Defaults to `NULL`, adding no butt
  cull. A cull spans from `stump_ht` to this height.

- pulp_tree:

  Whether the entire tree is restricted to `topwood_product`. Accepts
  `TRUE` or `FALSE` per tree, without missing values. Defaults to
  `FALSE`. Cannot accompany a saw stop or jump butt for the same tree.

- stump_ht:

  Stump height above ground, in feet. Accepts finite, nonnegative
  numeric values below total height. Defaults to `1`. A jump-butt cull
  starts here, and supplied stopping heights must lie above it.

## Value

A `merch_defects` data frame with `tree_id` (identifier), `start_height`
and `end_height` (feet above ground), `effect` (cutting effect),
`product` (restriction label, otherwise missing), and `percent` (sweep
percentage, otherwise missing). Scalars recycle to the record count.

## Examples

``` r
## Convert the southern list's stopping heights
converted <- defects_from_stoppers(tree_id = example_trees_south$tree_id,
                                   ht = example_trees_south$ht,
                                   topwood_product = 'pulp',
                                   saw_stop = example_trees_south$saw_stop,
                                   pulp_stop = example_trees_south$pulp_stop,
                                   jump_butt = example_trees_south$jump_butt)

## Inspect the resulting defect intervals
head(converted)
#> <merch_defects> 6 records
#>  tree_id start_height end_height   effect product percent
#>        2         25.4         NA restrict    pulp      NA
#>        5         40.9         NA      end    <NA>      NA
#>       24         33.3         NA restrict    pulp      NA
#>       31         34.2         NA      end    <NA>      NA
#>       40         24.6         NA      end    <NA>      NA
#>       49         39.0         NA restrict    pulp      NA
```
