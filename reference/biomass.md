# Estimate dry biomass and carbon in metric tonnes

Estimate tree biomass independently of merchandising products. Inputs
use inches and feet. All mass outputs use metric tonnes, and the
aboveground total excludes foliage and roots.

## Usage

``` r
biomass(
  dbh,
  ht,
  spcd,
  division = 0,
  ...
)
```

## Arguments

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid measurement rows return missing results with a
  status.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default.
  Measurement heights and section bounds must fall within the tree.

- spcd:

  Species identifiers as numeric positive whole-number codes. Required,
  without a default. The biomass lookup must recognize the code after
  its source species remapping. Unknown codes return missing masses with
  a status.

- division:

  Ecological division code as a numeric vector of whole numbers from 0
  through 1999. Defaults to `0`, using national coefficients. Lookup
  helpers return local codes, and unsupported codes receive a status.

- ...:

  Named biomass inputs, with defaults used when omitted. Component
  boundaries accept positive `primary_top = 6` and `secondary_top = 4`
  in inches and nonnegative `stump_ht = 1` in feet. `max_log_length`,
  `min_log_length`, and `trim` accept positive feet or `NA_real_` (their
  defaults), leaving source length choices unresolved. `cull = 0`
  accepts percentages from 0 through 100, and `decay_class = 0` accepts
  whole numbers from 0 through 5. These inputs affect the source biomass
  calculation and do not read merchandising products.

## Value

A data frame in input order. All mass columns use metric tonnes:

- `dry_aboveground_no_foliage`: stem wood, stem bark, and branches.

- `dry_stem_wood`, `dry_stem_bark`: whole-stem wood and bark.

- `dry_stump_wood`, `dry_stump_bark`: stump wood and bark.

- `dry_saw_wood`, `dry_saw_bark`: sawlog portion wood and bark.

- `dry_topwood_wood`, `dry_topwood_bark`: topwood portion wood and bark.

- `dry_tip_wood`, `dry_tip_bark`: tip wood and bark.

- `dry_branches`, `dry_foliage`: separate crown components.

- `dry_top_and_limb`: tip wood, tip bark, and branches combined.

- `carbon`: carbon in aboveground dry mass excluding foliage.

- `tco2e`: carbon dioxide equivalent, `carbon * 44 / 12`.

- `status`: integer result code. Whole-stem and partition columns
  overlap. Do not sum all component columns. Foliage is separate from
  the aboveground total, and roots are not estimated.

## Examples

``` r
## Estimate masses for the first shipped tree
mass <- biomass(dbh = example_trees$dbh[1],
                ht = example_trees$ht[1],
                spcd = example_trees$spcd[1])

## Inspect dry mass in metric tonnes
mass$dry_aboveground_no_foliage
#> [1] 0.3258422
```
