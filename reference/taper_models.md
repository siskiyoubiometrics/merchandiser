# List registered taper equations by form or species

Inspect registered model capabilities and provenance. Dynamically
resolved source identifiers need not appear in this registry list.

## Usage

``` r
taper_models(
  form = NULL,
  spcd = NULL
)
```

## Arguments

- form:

  Equation form names as a character vector, or `NULL`. Defaults to
  `NULL`, accepting every registered form. When supplied, only matching
  forms are returned.

- spcd:

  Species codes as numeric positive whole numbers, or `NULL`. Defaults
  to `NULL`, imposing no species filter. Models with unrestricted scope
  remain eligible.

## Value

A data frame with `id`, `form`, `kernel`, and `spcd_scope`
(identification), `has_dob`, `has_inverse`, `has_integral` (capability
indicators), `oracle_verified` (verification indicator),
`measurement_system` (internal units), `stump_ht` (feet for imperial
models or meters for metric models), `owner_package` (contributing
package), `source` (provenance), and `generation` (registration order).

## Examples

``` r
## Inspect registered models covering an example species
head(taper_models(spcd = example_trees$spcd[1]))
#>            id           form   kernel spcd_scope has_dob has_inverse
#> 3  100FW2W202 flewelling_2pt compiled        202    TRUE       FALSE
#> 36 100JB2W202       r1_taper compiled        202   FALSE        TRUE
#> 37 200CZ2W202       r2_taper compiled        202   FALSE        TRUE
#> 38 200CZ3W202       r2_taper compiled        202   FALSE        TRUE
#> 4  200FW2W202 flewelling_2pt compiled        202    TRUE       FALSE
#> 5  300FW2W202 flewelling_2pt compiled        202    TRUE       FALSE
#>    has_integral oracle_verified measurement_system stump_ht owner_package
#> 3          TRUE            TRUE           imperial        1  merchandiser
#> 36         TRUE            TRUE           imperial        1  merchandiser
#> 37         TRUE            TRUE           imperial        1  merchandiser
#> 38         TRUE            TRUE           imperial        1  merchandiser
#> 4          TRUE            TRUE           imperial        1  merchandiser
#> 5          TRUE            TRUE           imperial        1  merchandiser
#>                                                                                                       source
#> 3  US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#> 36 US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#> 37 US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#> 38 US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#> 4  US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#> 5  US Forest Service National Volume Estimator Library, NVEL commit 38548071d5aa652bb90c7f111f86b427f798a1c9
#>    generation
#> 3           3
#> 36       3120
#> 37       3130
#> 38       3137
#> 4           7
#> 5          13
```
