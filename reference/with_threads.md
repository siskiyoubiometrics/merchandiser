# Evaluate a calculation with a temporary thread limit

Set a worker limit for one expression and restore the previous setting
afterward, including when evaluation fails. Use this wrapper to limit
workers within a larger script.

## Usage

``` r
with_threads(
  n,
  code
)
```

## Arguments

- n:

  Requested thread count as one finite whole number of at least one.
  Required, without a default. Larger requests are limited to the
  available cores, and builds without parallel support use one.

- code:

  Expression to evaluate under the temporary limit. Required, without a
  default. Evaluation occurs in the calling environment.

## Value

The value returned by `code`, with its visibility preserved. The prior
thread setting is restored.

## Examples

``` r
## Measure the example trees under a temporary single-worker limit
with_threads(n = 1,
             code = dib(dbh = example_trees$dbh,
                        ht = example_trees$ht,
                        h = 20,
                        spcd = example_trees$spcd))
#>      value status
#> 1  8.56020      0
#> 2 11.00230      0
#> 3 12.19541      0
#> 4 14.74339      0
#> 5 15.63832      0
#> 6 20.14008      0
```
