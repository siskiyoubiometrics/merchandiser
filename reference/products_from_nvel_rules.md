# Convert explicit source rules to products

Translate a complete source rule record into explicit product rows. Use
the result when the source rule fields have already been resolved for an
equation.

## Usage

``` r
products_from_nvel_rules(
  x
)
```

## Arguments

- x:

  Validated list returned by
  [`nvel_rules()`](https://siskiyoubiometrics.com/merchandiser/reference/nvel_rules.md)
  with explicit finite length, diameter, stump, and trim fields.
  Required, without a default. Missing source defaults cannot be
  translated without an equation-specific lookup.

## Value

A list with `products` (two
[`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md)
rows named `nvel_primary` and `nvel_secondary`, scaled in Scribner board
feet), `stump_ht` (feet), and `model_aux` (list containing `bark_ratio`,
an inside-to-outside diameter ratio). Product columns and units are
those of
[`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md).
Continuous product bounds do not reproduce every source top-segment
rule.

## Examples

``` r
## Translate complete source rules and inspect the stump height in feet
products_from_nvel_rules(x = nvel_rules(even_or_odd = 1,
                                        option = 11,
                                        maximum_length = 32,
                                        minimum_length = 16,
                                        primary_top = 6,
                                        secondary_top = 4,
                                        stump = 1,
                                        trim = 0.5,
                                        minimum_board_foot_dbh = 1))$stump_ht
#> [1] 1
```
