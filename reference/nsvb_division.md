# Look up ecological division by state and county

Translate numeric state and county codes to the ecological division used
for biomass coefficients. Use the returned code to request
location-specific biomass estimates.

## Usage

``` r
nsvb_division(
  state,
  county
)
```

## Arguments

- state:

  State code as numeric whole numbers from 1 through 99. Required,
  without a default. Nonfinite codes receive missing-input status.
  Finite codes outside the accepted range or with fractional values
  cause an error.

- county:

  County code within the supplied state, as numeric whole numbers from 1
  through 999. Required, without a default. Finite fractional or
  out-of-range codes cause an error. State and county jointly determine
  the lookup.

## Value

A data frame with `value` (integer ecological division code) and
`status` (integer result code). Missing inputs receive status 1, and
unrecognized locations receive status 8.

## Examples

``` r
## Compare a county lookup with the shipped coordinates
nsvb_division(state = 41, county = 5)
#>   value status
#> 1  1242      0

## Inspect the lookup at the shipped coordinates
nsvb_division_xy(x = example_trees_pnw$longitude[1],
                 y = example_trees_pnw$latitude[1])
#>   value status
#> 1  1240      0
```
