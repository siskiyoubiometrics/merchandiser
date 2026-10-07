# Check defect heights and product restrictions

Validate records against tree heights and product names. Exact
duplicates are removed with a message, and overlapping valid culls are
merged.

## Usage

``` r
validate_defects(
  defects,
  tree_id,
  ht,
  products
)
```

## Arguments

- defects:

  Defect records with the columns returned by
  [`defect()`](https://siskiyoubiometrics.com/merchandiser/reference/defect.md).
  Required, without a default. Invalid records are retained with a
  status rather than silently used for cutting.

- tree_id:

  Unique tree identifiers corresponding to `ht`. Accepts a nonmissing
  atomic vector of the same identifier type as the defects. Required,
  without a default.

- ht:

  Total tree heights above ground, in feet. Accepts one numeric value
  for each tree identifier. Required, without a default. Used to resolve
  missing interval ends and check physical bounds.

- products:

  Validated product rows from
  [`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md)
  or
  [`products()`](https://siskiyoubiometrics.com/merchandiser/reference/products.md).
  Required, with no default. Names must be unique, and row order
  supplies priority for the default cascade strategy.

## Value

A `merch_defects` data frame with `tree_id` (identifier), `start_height`
and `end_height` (feet above ground), `effect` (cutting effect),
`product` (restriction label, otherwise missing), and `percent` (sweep
percentage, otherwise missing). Scalars recycle to the record count. An
integer `status` column reports each validation condition. See
[`status_codes()`](https://siskiyoubiometrics.com/merchandiser/reference/status_codes.md)
for descriptions.

## Examples

``` r
## Define an unpriced cubic-foot product
saw <- product(product = 'saw',  ## product label
               min_length = 16,  ## feet
               max_length = 32,  ## feet
               min_sed = 6,  ## inches inside bark
               volume_unit = 'cubic')  ## cubic feet

## Define the product named by the shipped restrictions
pulp <- product(product = 'pulp',  ## product label
                min_length = 8,  ## feet
                max_length = 20,  ## feet
                min_sed = 3,  ## inches inside bark
                volume_unit = 'green_ton')  ## green short tons

## Validate shipped defects against their matching trees
checked <- validate_defects(defects = example_defects_pnw,
                            tree_id = example_trees_pnw$tree_id,
                            ht = example_trees_pnw$ht,
                            products = products(saw, pulp))

## Inspect validated records
head(checked)
#> <merch_defects> 6 records
#>  tree_id start_height end_height   effect product percent status
#>        7            1          8     cull    <NA>      NA      0
#>       14           12         15     cull    <NA>      NA      0
#>       21           88         NA      end    <NA>      NA      0
#>       28           35         NA restrict    pulp      NA      0
#>       35           28         70    sweep    <NA>      25      0
#>       42            1          8     cull    <NA>      NA      0
```
