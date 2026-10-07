# Record registered taper identifiers and versions

Capture registry provenance for a calculation. The manifest excludes
model coefficients and source identifiers resolved without an explicit
registry entry.

## Usage

``` r
taper_manifest()
```

## Value

A data frame with `id` (model identifier), `owner_package` (contributing
package), `package_version` (version string), and `generation`
(registration order). Save fitted objects or coefficient vectors
separately.

## Examples

``` r
## Find the registered entries used by the shipped example
library(dplyr)

## Retain registered identifiers used by the example
taper_manifest() %>%
  filter(id %in% example_trees$model)
#>                    id owner_package package_version generation
#> F00FW2W202 F00FW2W202  merchandiser           0.5.0         25
#> F03FW2W263 F03FW2W263  merchandiser           0.5.0         40
```
