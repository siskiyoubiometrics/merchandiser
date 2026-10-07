# Shipped default taper models by species

Species defaults retain a source equation with matching species scope. A
default must resolve to exactly that species and require no extra
measurements. Species without an accepted model have no row.

## Usage

``` r
default_taper_models
```

## Format

A data frame with the following columns:

- spcd:

  Integer inventory species code.

- model:

  Character taper model identifier.

- source:

  Character lookup call that supplied the model.

## Examples

``` r
## Inspect the shipped defaults for the example species
library(dplyr)

## Retain equation assignments for the example species
default_taper_models %>%
  filter(spcd %in% example_trees$spcd) %>%
  select(spcd, model)
#>   spcd      model
#> 1  202 F00FW2W202
#> 2  263 F03FW2W263
```
