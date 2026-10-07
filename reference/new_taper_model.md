# Create a taper model from diameter and volume functions

Define an equation using existing R callbacks. Callbacks receive inches
and feet and must return finite numeric vectors without changing global
state. Registration validates the model and its callback behavior.

## Usage

``` r
new_taper_model(
  id,
  form,
  dib,
  dob = NULL,
  height_at_dib = NULL,
  volume = NULL,
  inputs = list(required = character(), optional = character(), pairs = list()),
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

  Descriptive equation form as one nonempty character string. Required,
  without a default. Used to identify and filter the model, without
  selecting a built-in coefficient equation.

- dib:

  Inside bark diameter function with arguments `dbh`, `ht`, `h`, and
  `aux`. Required, without a default. Diameters use inches, heights use
  feet, and `aux` is a named list. Return one numeric diameter per input
  tree.

- dob:

  Outside bark diameter function with arguments `dbh`, `ht`, `h`, and
  `aux`, or `NULL`. Defaults to `NULL`. Uses the same units as `dib`,
  with a supplied bark ratio available as a fallback.

- height_at_dib:

  Inverse function with arguments `dbh`, `ht`, `dib`, and `aux`, or
  `NULL`. Defaults to `NULL`, enabling numerical inversion. Return
  height in feet for a target diameter in inches.

- volume:

  Inside bark volume function with arguments `dbh`, `ht`, `lower`,
  `upper`, and `aux`, or `NULL`. Defaults to `NULL`, enabling numerical
  integration. Bounds use feet and the returned volume uses cubic feet.

- inputs:

  Named list declaring `required` and `optional` auxiliary input names
  and `pairs` of inputs that must occur together. Defaults to empty
  character vectors and an empty pair list. Only declared supported
  auxiliary names are accepted.

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
  `NULL`, storing an empty provenance string. Supply source text to
  retain an attribution with the model for later inspection.

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
## Reuse the demonstration model's diameter function
local_model <- new_taper_model(id = 'local.demonstration',
                               form = 'paraboloid',
                               dib = get_taper_model(model = 'demo.paraboloid.r')$dib)

## Evaluate the callback on a shipped tree, in inches and feet
local_model$dib(dbh = example_trees$dbh[1],
                ht = example_trees$ht[1],
                h = 20,
                aux = list())
#> [1] 9.168689
```
