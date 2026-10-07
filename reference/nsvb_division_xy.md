# Look up ecological division from coordinates

Locate coordinates in the shipped ecological division polygons for
biomass coefficient selection. Use the returned code when estimating
biomass with local coefficients.

## Usage

``` r
nsvb_division_xy(
  x,
  y,
  crs = 4326
)
```

## Arguments

- x:

  Horizontal coordinate as a numeric vector. Required, without a
  default. The default coordinate system uses longitude in decimal
  degrees. Other systems use their own coordinate units.

- y:

  Vertical coordinate as a numeric vector. Required, without a default.
  The default coordinate system uses latitude in decimal degrees.
  Coordinates outside the polygon coverage receive status 8.

- crs:

  Coordinate reference system as one numeric code or character string
  accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).
  Defaults to `4326` for longitude and latitude in degrees. A different
  system requires sf to transform coordinates. A constructed sf
  coordinate-system object is not accepted.

## Value

A data frame with `value` (integer ecological division code) and
`status` (integer result code). Missing inputs receive status 1, and
unrecognized locations receive status 8.

## Examples

``` r
## Inspect the ecological division at the shipped location
nsvb_division_xy(x = example_trees_pnw$longitude[1],
                 y = example_trees_pnw$latitude[1])
#>   value status
#> 1  1240      0
```
