# Check availability of taper equation identifiers

Check whether identifiers resolve to models without testing the
submitted tree measurements or species scope. Use it to check stored
identifiers before selecting an equation.

## Usage

``` r
has_taper_model(
  model
)
```

## Arguments

- model:

  Equation identifiers as a character vector. Required, without a
  default. Missing, empty, and unrecognized identifiers return `FALSE`.

## Value

A vector of `TRUE` or `FALSE` values in identifier order. This indicates
availability, not suitability for a particular tree.

## Examples

``` r
## Check the stored equations for the shipped trees
has_taper_model(model = example_trees$model)
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE
```
