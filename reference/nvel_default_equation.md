# Look up a source library equation identifier

Select a source equation from location and species codes. Use the
identifier to check whether that source assignment is implemented.

## Usage

``` r
nvel_default_equation(
  region,
  forest,
  district,
  spcd,
  variant = NULL
)
```

## Arguments

- region:

  Forest Service region code as whole-number numeric values from 1
  through 10. Required, without a default. Selects the source lookup
  table.

- forest:

  Forest code as whole-number numeric values from 0 through 99.
  Required, without a default. Used with region and district in the
  source lookup.

- district:

  District code as whole-number numeric values from 0 through 99.
  Required, without a default. Used with region and forest in the source
  lookup.

- spcd:

  Species code as numeric whole numbers from 1 through 9999. Required,
  without a default. The resulting source identifier may not be
  implemented by this package.

- variant:

  Geographic variant as a character vector, or `NULL`. Defaults to
  `NULL`, using the source default. Values are trimmed and normalized to
  uppercase. Accepted variants by region are `1`: EM, IE, CI, `5`: CA,
  SO, WS, NC, `6`: BM, EC, SO, WC, PN, NC, IE, CA, OC, OP, `7`: WC, NC,
  SO, PN, CA, OC, OP, `8`: SN, and `9`: LS, CS, NE, SN. Other regions
  accept only an unspecified variant.

## Value

A character vector of source equation identifiers in input order, with
missing values where no identifier is available. A returned identifier
is not a guarantee that the package implements it. Check with
[`has_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/has_taper_model.md).

## Examples

``` r
## Look up a source equation for a shipped species
model <- nvel_default_equation(region = 6,
                               forest = 0,
                               district = 0,
                               spcd = example_trees$spcd[1])

## Check whether the returned equation is implemented
has_taper_model(model = model)
#> [1] TRUE
```
