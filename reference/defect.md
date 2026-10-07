# Record a located stem defect

Describe a cull, usable-stem end, product restriction, or sweep
interval. Construction checks field types. Use
[`validate_defects()`](https://siskiyoubiometrics.com/merchandiser/reference/validate_defects.md)
to check heights and product relationships.

## Usage

``` r
defect(
  tree_id,
  start_height,
  end_height,
  effect,
  product = NULL,
  percent = NULL
)
```

## Arguments

- tree_id:

  Identifiers linking each defect to an input tree. Accepts a nonmissing
  atomic vector. Required, with no default. Its type must match the tree
  list when records are validated.

- start_height:

  Lower height of the affected interval, in feet above ground. Accepts
  numeric values. Required, with no default. Validation requires a
  nonnegative height within the tree.

- end_height:

  Upper height of the affected interval, in feet above ground. Accepts
  numeric values or `NA`. Required, with no default. Missing values
  extend intervals to the tip. An end effect accepts missing or equal
  starting and ending heights. Other intervals require a higher end.

- effect:

  Effect on cutting as a character vector. Accepts `'cull'` to remove
  wood, `'end'` to stop cutting, `'restrict'` to allow only a named
  product, or `'sweep'` to compare severity with product limits.
  Required, with no default.

- product:

  Required product label for a restriction, as a character vector.
  Defaults to `NULL`, represented by missing values. Must match a
  supplied specification for restrict effects and remain missing for
  other effects.

- percent:

  Sweep severity, in percent, as numeric values from 0 through 100.
  Defaults to `NULL`, represented by missing values. Required for sweep
  effects and must remain missing for other effects. It affects
  eligibility, not a scale deduction.

## Value

A `merch_defects` data frame with `tree_id` (identifier), `start_height`
and `end_height` (feet above ground), `effect` (cutting effect),
`product` (restriction label, otherwise missing), and `percent` (sweep
percentage, otherwise missing). Scalars recycle to the record count.

## Examples

``` r
## Record a cull on the first example tree
defect(tree_id = example_trees$tree_id[1],
       start_height = 1,
       end_height = 8,
       effect = 'cull')
#> <merch_defects> 1 records
#>  tree_id start_height end_height effect product percent
#>        1            1          8   cull    <NA>      NA
```
