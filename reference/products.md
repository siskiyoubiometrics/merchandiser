# Combine products in cutting priority order

Combine validated product rows into a specification. Under the default
cascade strategy, earlier rows receive the first opportunity to use each
available stem section.

## Usage

``` r
products(
  ...
)
```

## Arguments

- ...:

  One or more product rows or tables from
  [`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md).
  Required, without a default. Rows are combined in argument order, with
  duplicate product names rejected.

## Value

A multiple-row `merch_products` data frame with all specification
fields:

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

## Combine the specification before cutting
specifications <- products(saw)

## Select logs from the shipped trees
result <- merchandise(tree_id = example_trees$tree_id,
                      dbh = example_trees$dbh,
                      ht = example_trees$ht,
                      spcd = example_trees$spcd,
                      products = specifications,
                      model = example_trees$model)

## Inspect selected products
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
