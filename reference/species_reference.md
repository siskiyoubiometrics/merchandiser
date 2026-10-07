# Species reference for joining tree lists

Species codes, names, and bark ratios are available for user-managed
joins. Function arguments accept numeric codes only.

## Usage

``` r
species_reference
```

## Format

A data frame with these columns:

- `spcd`: numeric species code.

- `common`, `scientific`, `genus`: species and genus names.

- `symbol`: plant symbol.

- `bark_ratio`: inside to outside diameter ratio.

- `softwood_hardwood`: wood group.

- `wood_density`: oven-dry wood weight, pounds per cubic foot.

- `sources`: provenance.

## Examples

``` r
## Attach species names to the shipped tree list
library(dplyr)

## Join species labels by numeric code
example_trees %>%
  left_join(y = species_reference,
            by = 'spcd') %>%
  select(tree_id, spcd, common)
#>   tree_id spcd          common
#> 1       1  202     Douglas-fir
#> 2       2  263 western hemlock
#> 3       3  202     Douglas-fir
#> 4       4  263 western hemlock
#> 5       5  202     Douglas-fir
#> 6       6  263 western hemlock
```
