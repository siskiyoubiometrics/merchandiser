# Register a private taper model for the session

Validate and register a model for use in stem calculations. Private
identifiers must contain a dot. Existing registrations cannot be
replaced without first removing the private registration.

## Usage

``` r
register_taper_model(
  x
)
```

## Arguments

- x:

  Model returned by
  [`new_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/new_taper_model.md),
  [`as_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/as_taper_model.md),
  or
  [`taper_model_from_coefficients()`](https://siskiyoubiometrics.com/merchandiser/reference/taper_model_from_coefficients.md).
  Required, without a default. Its identifier and callback behavior are
  validated before registration.

## Value

The validated `taper_model` described in
[`new_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/new_taper_model.md),
invisibly. The session registry is updated after validation.

## Examples

``` r
## Copy the first shipped tree's equation under a private identifier
local_model <- get_taper_model(model = example_trees$model[1])

## Assign a private identifier to the copied model
local_model$id <- 'local.example'

## Register and inspect the private copy
print(register_taper_model(x = local_model))
#> <taper_model local.example>
#>   form: flewelling_2pt
#>   kernel: compiled
#>   units: imperial
#>   oracle verified: yes
#>   capabilities: dob, integral

## Remove the temporary registration
unregister_taper_model(model = local_model$id)
```
