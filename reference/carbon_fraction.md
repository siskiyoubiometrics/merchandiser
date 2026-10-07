# Look up the carbon fraction of dry biomass

Return the species fraction used to convert dry aboveground mass
excluding foliage to carbon. Use it to check the carbon calculation for
numeric species codes.

## Usage

``` r
carbon_fraction(
  spcd
)
```

## Arguments

- spcd:

  Species identifiers as numeric positive whole-number codes. Required,
  without a default. The carbon lookup applies its source species
  remapping. Unknown or invalid codes return missing fractions with a
  warning.

## Value

A numeric vector in species input order, with a `source` attribute
recording provenance. Missing codes return `NA`. Invalid or unknown
codes return `NA` with a warning. The default compatibility setting
returns the rounded fraction used in biomass calculations, while source
compatibility returns the raw table fraction.

## Examples

``` r
## Inspect the fraction used for the first example species
carbon_fraction(spcd = example_trees$spcd[1])
#> [1] 0.516
#> attr(,"source")
#> [1] "NVEL tables10.inc and nsvb.f NVB_CarbonFrac, commit 38548071d5aa652bb90c7f111f86b427f798a1c9, three-decimal NVBC carbon rounding"
```
