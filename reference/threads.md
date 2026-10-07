# Inspect or set the calculation thread limit

Control the worker limit used by parallel tree calculations.
Initialization reads MERCHANDISER_THREADS, then TREEVOLUME_THREADS, and
otherwise starts with one worker.

## Usage

``` r
threads(
  n = NULL
)
```

## Arguments

- n:

  Requested thread count as one finite whole number of at least one.
  Defaults to `NULL` to report the current count. Larger requests are
  limited to the available cores, and builds without parallel support
  use one.

## Value

With `n = NULL`, the current integer thread limit. With a count
supplied, the previous limit after setting the new one. Counts are
limited to available cores, and nonparallel builds use one worker.

## Examples

``` r
## Inspect the active calculation limit
threads()
#> [1] 1

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
