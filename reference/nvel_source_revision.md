# Inspect the pinned source library revision

Return the source revision associated with shipped equations and
fixtures. Use it to identify the implementation behind a source
comparison.

## Usage

``` r
nvel_source_revision()
```

## Value

A character scalar containing the pinned source commit, with
`upstream_url` and `fixtures_release_tag` attributes.

## Examples

``` r
## Inspect the revision behind the shipped example equations
nvel_source_revision()
#> [1] "38548071d5aa652bb90c7f111f86b427f798a1c9"
#> attr(,"upstream_url")
#> [1] "https://github.com/FMSC-Measurements/VolumeLibrary"
#> attr(,"fixtures_release_tag")
#> [1] "v0.1.0"
```
