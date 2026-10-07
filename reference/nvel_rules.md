# Record source library measurement rules

Validate a source library rule record. Use the retained fields to
describe a resolved source measurement convention.

## Usage

``` r
nvel_rules(
  even_or_odd = NA_integer_,
  option = NA_integer_,
  maximum_length = NA_real_,
  minimum_length = NA_real_,
  minimum_top_length = NA_real_,
  merchantable_length = NA_real_,
  primary_top = NA_real_,
  secondary_top = NA_real_,
  stump = NA_real_,
  trim = NA_real_,
  bark_ratio = NA_real_,
  minimum_board_foot_dbh = NA_real_,
  scribner = 'regional',
  prod = '01',
  ht_type = '',
  live = 'L',
  ctype = 'C',
  cull = 0,
  forest = 0,
  district = 0
)
```

## Arguments

- even_or_odd:

  Source log-length rounding code as a numeric scalar. Accepts `1` for
  whole-foot lengths, `2` for even-foot lengths, or `NA`. Defaults to
  `NA_integer_`, leaving the source default unresolved.

- option:

  Source segmentation code as a numeric scalar. Accepts `11` through
  `14` for the source length conventions or `21` through `24` for
  top-segment conventions. Defaults to `NA_integer_`, leaving the source
  default unresolved.

- maximum_length:

  Longest nominal source segment, in feet. Accepts one finite positive
  number or `NA`. Defaults to `NA_real_`, leaving the source default
  unresolved.

- minimum_length:

  Shortest nominal source segment, in feet. Accepts one finite positive
  number or `NA`. Defaults to `NA_real_`, leaving the source default
  unresolved.

- minimum_top_length:

  Shortest source top segment, in feet. Accepts one finite positive
  number or `NA`. Defaults to `NA_real_`, leaving the source default
  unresolved.

- merchantable_length:

  Primary-product stem length required for eligibility, in feet. Accepts
  one finite positive number or `NA`. Defaults to `NA_real_`, leaving
  the source default unresolved.

- primary_top:

  Primary inside bark top diameter, in inches. Accepts one finite
  positive number or `NA`. Defaults to `NA_real_`, leaving the source
  default unresolved.

- secondary_top:

  Secondary inside bark top diameter, in inches. Accepts one finite
  positive number or `NA`. Defaults to `NA_real_`, leaving the source
  default unresolved.

- stump:

  Source stump height above ground, in feet. Accepts one finite
  nonnegative number or `NA`. Defaults to `NA_real_`, leaving the source
  default unresolved.

- trim:

  Extra length above a nominal source segment, in feet. Accepts one
  finite positive number or `NA`. Defaults to `NA_real_`, leaving the
  source default unresolved.

- bark_ratio:

  Inside bark diameter divided by outside bark diameter. Accepts a
  single finite number greater than zero and no greater than one, or
  `NA`. Defaults to `NA_real_`, leaving the ratio unspecified. Enables a
  constant-ratio outside bark calculation where needed.

- minimum_board_foot_dbh:

  Minimum outside bark breast-height diameter for board foot scale, in
  inches. Accepts one finite positive number or `NA`. Defaults to
  `NA_real_`, leaving the source default unresolved.

- scribner:

  Source Scribner method as one character string. Accepts 'regional',
  'table', or 'factor'. Defaults to 'regional', using the source default
  method. Other choices request table scale or source factors.

- prod:

  Source product code as one character string containing exactly two
  digits. Defaults to '01' for sawtimber. '08' selects the source nonsaw
  product route.

- ht_type:

  Source height basis as one character string. Accepts ”, 'F', or 'L'.
  Defaults to ”, leaving the basis unspecified. 'F' requests feet and
  'L' requests logs.

- live:

  Source live-status flag as one character string. Accepts 'L' or 'D'.
  Defaults to 'L' for live trees. 'D' identifies dead trees.

- ctype:

  Source calculation route as one character string. Accepts 'C', 'I',
  'F', or 'B'. Defaults to 'C' for cruise. Other codes select Forest
  Inventory and Analysis, Forest Vegetation Simulator, or the alternate
  national biomass route.

- cull:

  Whole-tree cull percentage for the source equation. Accepts one finite
  number from 0 through 100 or `NA`. Defaults to `0`. It does not deduct
  scale from merchandise logs.

- forest:

  Forest Service forest code as one whole number from 0 through 99 or
  `NA`. Defaults to `0`. Retained for source rule selection.

- district:

  Forest Service district code as one whole number from 0 through 99 or
  `NA`. Defaults to `0`. Retained for source rule selection.

## Value

A validated `treevolume_nvel_rules` list retaining each scalar argument:
`even_or_odd` and `option` (source codes), `maximum_length`,
`minimum_length`, `minimum_top_length`, `merchantable_length`, `stump`,
and `trim` (feet), `primary_top`, `secondary_top`, and
`minimum_board_foot_dbh` (inches), `bark_ratio` (diameter ratio),
`scribner`, `prod`, `ht_type`, `live`, and `ctype` (source flags),
`cull` (percent), and `forest` and `district` (location codes). Missing
values remain unresolved source defaults.

## Examples

``` r
## Inspect an explicit source length and top-diameter rule
nvel_rules(even_or_odd = 1,
           maximum_length = 32,
           minimum_length = 16,
           primary_top = 6)
#> $even_or_odd
#> [1] 1
#> 
#> $option
#> [1] NA
#> 
#> $maximum_length
#> [1] 32
#> 
#> $minimum_length
#> [1] 16
#> 
#> $minimum_top_length
#> [1] NA
#> 
#> $merchantable_length
#> [1] NA
#> 
#> $primary_top
#> [1] 6
#> 
#> $secondary_top
#> [1] NA
#> 
#> $stump
#> [1] NA
#> 
#> $trim
#> [1] NA
#> 
#> $bark_ratio
#> [1] NA
#> 
#> $minimum_board_foot_dbh
#> [1] NA
#> 
#> $scribner
#> [1] "regional"
#> 
#> $prod
#> [1] "01"
#> 
#> $ht_type
#> [1] ""
#> 
#> $live
#> [1] "L"
#> 
#> $ctype
#> [1] "C"
#> 
#> $cull
#> [1] 0
#> 
#> $forest
#> [1] 0
#> 
#> $district
#> [1] 0
#> 
#> attr(,"class")
#> [1] "treevolume_nvel_rules" "list"                 
```
