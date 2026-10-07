# Remove a private taper model from the session

Remove an equation registered in the current session. Shipped models
cannot be removed.

## Usage

``` r
unregister_taper_model(
  model
)
```

## Arguments

- model:

  Equation identifier as one nonempty character string. Required,
  without a default. The identifier must resolve to a private registered
  equation.

## Value

`TRUE`, invisibly, after removing the private registration.

## Examples

``` r
## Copy the first shipped tree's equation under a private identifier
local_model <- get_taper_model(model = example_trees$model[1])

## Assign a private identifier to the copied model
local_model$id <- 'local.example'

## Register the private copy
register_taper_model(x = local_model)

## Remove and confirm the temporary registration
print(unregister_taper_model(model = local_model$id))
#> [1] TRUE
```
