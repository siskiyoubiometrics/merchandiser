# Look up calculation status codes

List result codes for invalid inputs, unavailable equations, capability
limits, and retained calculation conditions. Join these descriptions to
a result when investigating a reported code.

## Usage

``` r
status_codes()
```

## Value

A data frame with `status` (integer code), `name` (short name),
`category` (condition class), `description` (meaning), and `source`
(origin of the definition). Code zero denotes a successful calculation.

## Examples

``` r
## Look up codes returned by an example measurement
library(dplyr)

## Calculate diameters using the stored species defaults
measured <- dib(dbh = example_trees$dbh,
                ht = example_trees$ht,
                h = 20,
                spcd = example_trees$spcd)

## Inspect descriptions of the returned codes
status_codes() %>%
  filter(status %in% measured$status)
#>   status name category                             description     source
#> 1      0   ok       ok The requested measurement is available. stem model
```
