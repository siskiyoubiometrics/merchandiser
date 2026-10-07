# Inspect a taper model and its required inputs

Resolve an identifier to its equation, capabilities, inputs, and
provenance. Inspect this object before supplying measurements required
by a particular model.

## Usage

``` r
get_taper_model(
  model
)
```

## Arguments

- model:

  Equation identifier as one nonempty character string. Required,
  without a default. The identifier must resolve to an available
  equation.

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
## Inspect the model stored with the first shipped tree
get_taper_model(model = example_trees$model[1])
#> <taper_model F00FW2W202>
#>   form: flewelling_2pt
#>   kernel: compiled
#>   units: imperial
#>   oracle verified: yes
#>   capabilities: dob, integral
```
