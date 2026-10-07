# Convert a taper fit to a usable model

Create a model from fitted population coefficients, retaining the fitted
species scope. Registration is a separate step.

## Usage

``` r
as_taper_model(
  x,
  model,
  stump_ht = 1,
  bark_ratio = NA_real_,
  source = NULL
)
```

## Arguments

- x:

  Fitted relationship returned by
  [`fit_taper()`](https://siskiyoubiometrics.com/merchandiser/reference/fit_taper.md).
  Required, without a default. The converted model uses population
  coefficients rather than group-specific adjustments.

- model:

  Identifier for the new equation as a nonempty character scalar.
  Required, with no default. Private registrations require a namespaced
  identifier containing a dot, such as `'local.profile'`.

- stump_ht:

  Default stump height above ground, in feet. Accepts one finite
  nonnegative number. Defaults to `1`. Stored in the model's internal
  height units without changing public input units.

- bark_ratio:

  Inside bark diameter divided by outside bark diameter. Accepts a
  single finite number greater than zero and no greater than one, or
  `NA`. Defaults to `NA_real_`, leaving the ratio unspecified. Enables a
  constant-ratio outside bark calculation where needed.

- source:

  Provenance for the equation as a single character string. Defaults to
  `NULL`, using the form citation where available. Stored with the model
  for later inspection.

## Value

A `taper_model` list containing `id` (identifier), `form` (equation
form), `kernel` (implementation and capability metadata), `dib`, `dob`,
`height_at_dib`, and `volume` (callbacks or absent optional callbacks),
`inputs` (required, optional, and paired auxiliary names),
`measurement_system` (internal units), `spcd` (species scope),
`stump_ht` (feet for imperial models or meters for metric models),
`bark_ratio` (inside-to-outside diameter ratio), `source` (provenance),
`notes`, `data` (equation data or coefficients), and `class_version`.
The optional `oracle_verified` attribute records source-fixture
verification. Public calls continue to accept inches and feet regardless
of stored coefficient units.

## Examples

``` r
## Fit a profile to the shipped stem measurements
fit <- fit_taper(tree_id = example_stem_measurements$tree_id,
                 dbh = example_stem_measurements$dbh,
                 ht = example_stem_measurements$ht,
                 h = example_stem_measurements$h,
                 dib = example_stem_measurements$dib,
                 spcd = example_stem_measurements$spcd)

## Inspect the model created from population coefficients
print(as_taper_model(x = fit, model = 'local.profile'))
#> <taper_model local.profile>
#>   form: max_burkhart
#>   kernel: compiled
#>   units: metric
#>   oracle verified: not applicable
#>   capabilities: inverse, integral
```
