# Construct a taper model from metric coefficients

Create an unregistered model from an existing coefficient vector. Supply
coefficients on the equation's metric scale and use inches and feet in
public measurements.

## Usage

``` r
taper_model_from_coefficients(
  id,
  form,
  coefficients,
  spcd = integer(),
  stump_ht = 1,
  bark_ratio = NA_real_,
  source = NULL
)
```

## Arguments

- id:

  Identifier for the new equation as a nonempty character scalar.
  Required, with no default. Private registrations require a namespaced
  identifier containing a dot, such as `'local.profile'`.

- form:

  Equation form as one character string. Accepts `'max_burkhart'`,
  `'kozak_1988'`, or `'kozak_2002'`. Required, without a default. The
  form determines coefficient names, order, and constraints.

- coefficients:

  Coefficients for the selected equation form as a finite named numeric
  vector in the form's required order. Required, with no default.
  Coefficients use centimeters for diameters and meters for heights,
  while public calls use inches and feet.

- spcd:

  Species scope as numeric positive whole-number codes. Defaults to
  [`integer()`](https://rdrr.io/r/base/integer.html), imposing no
  species restriction. A scoped model reports a condition when called
  for another species.

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

## Details

Max-Burkhart coefficients are `b1`, `b2`, `b3`, `b4`, `a1`, `a2`, with
`0 < a2 < a1 < 1`. Kozak 1988 uses `a0`, `a1`, `a2`, `b1` through `b5`,
and `p`, with positive `a0` and `a2` and `0 < p < 1`. Kozak 2002 uses
`a0`, `a1`, `a2`, and `b1` through `b6`, with positive `a0`.

## Examples

``` r
## Use the shipped stem measurements
measurements <- example_stem_measurements

## Fit coefficients to the measured profiles
fit <- fit_taper(tree_id = measurements$tree_id,
                 dbh = measurements$dbh,
                 ht = measurements$ht,
                 h = measurements$h,
                 dib = measurements$dib,
                 spcd = measurements$spcd)

## Inspect a model reconstructed from the fitted metric coefficients
taper_model_from_coefficients(id = 'local.coefficients',
                              form = fit$form,
                              coefficients = fit$coefficients,
                              spcd = fit$spcd)
```
