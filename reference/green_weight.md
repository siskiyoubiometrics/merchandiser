# Convert solid stem volume to merchandising weight

Convert solid wood volume to wood, bark, or combined stem weight using
species properties and optional overrides. This function returns short
tons for merchandising, rather than biomass carbon.

## Usage

``` r
green_weight(
  volume,
  spcd,
  inside_bark = TRUE,
  component = 'stem',
  moisture = 'green',
  specific_gravity = NULL,
  moisture_pct = NULL,
  bark_specific_gravity = NULL,
  bark_moisture_pct = NULL,
  bark_volume_pct = NULL
)
```

## Arguments

- volume:

  Solid stem volume, in cubic feet, as a nonnegative numeric vector.
  Required, without a default. `inside_bark` identifies whether the
  supplied volume contains bark.

- spcd:

  Species identifiers as numeric positive whole-number codes recognized
  by the species-property lookup. Required, without a default. A
  recognized species is still required when wood or bark properties are
  overridden.

- inside_bark:

  Basis of the supplied solid volume, as `TRUE` or `FALSE` per row.
  Defaults to `TRUE` for wood volume excluding bark. With `FALSE`, bark
  volume is separated using the bark-to-wood volume ratio. Missing
  values produce missing results with status.

- component:

  Mass component as a character vector. Accepts `'stem'`, `'wood'`, or
  `'bark'`. Defaults to `'stem'`, including wood and attached bark even
  when the volume input is inside bark.

- moisture:

  Mass basis as a character vector. Accepts `'green'` or `'dry'`.
  Defaults to `'green'`, adding moisture to dry wood and bark mass.
  `'dry'` excludes moisture but retains the selected components.

- specific_gravity:

  Wood specific gravity as finite numeric values greater than zero and
  no greater than two. Defaults to `NULL`, using species properties.
  Overrides apply only to the wood component.

- moisture_pct:

  Wood moisture as percent of dry mass, from 0 through 300. Defaults to
  `NULL`, using species properties. Used only for green mass.

- bark_specific_gravity:

  Bark specific gravity as finite numeric values greater than zero and
  no greater than two. Defaults to `NULL`, using species properties.
  Overrides apply to the bark component.

- bark_moisture_pct:

  Bark moisture as percent of dry mass, from 0 through 300. Defaults to
  `NULL`, using species properties. Used only for green mass.

- bark_volume_pct:

  Bark volume as percent of inside bark wood volume, from 0 through 100.
  Defaults to `NULL`, using species properties. Determines attached bark
  volume or separates an outside bark volume.

## Value

A data frame in input order with `value` (short tons on the selected
component and moisture basis) and `status` (integer result code). Scalar
inputs recycle across rows.

## Examples

``` r
## Calculate solid volume before converting to weight
volume <- stem_volume(dbh = example_trees$dbh[1],
                      ht = example_trees$ht[1],
                      spcd = example_trees$spcd[1],
                      model = example_trees$model[1])

## Inspect green stem weight including attached bark
green_weight(volume = volume$value,
             spcd = example_trees$spcd[1],
             inside_bark = TRUE)
#>       value status
#> 1 0.3944409      0
```
