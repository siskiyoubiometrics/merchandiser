# Translate an inventory equation identifier

Translate a Forest Inventory and Analysis equation code to a source
library identifier. Use the returned source identifier when checking an
inventory equation assignment.

## Usage

``` r
nvel_from_fia_code(
  fia_code,
  spcd,
  geosub = '',
  primary_top = 0
)
```

## Arguments

- fia_code:

  Forest Inventory and Analysis equation code as character strings of
  exactly eight characters. Required, without a default. Whitespace is
  not removed, and codes are normalized to uppercase.

- spcd:

  Species code as numeric whole numbers from 1 through 9999. Required,
  without a default. The resulting source identifier may not be
  implemented by this package.

- geosub:

  Geographic substitution as a character vector. Defaults to an empty
  string, requesting no substitution. Compatibility mode controls
  fixed-width source string behavior.

- primary_top:

  Primary inside bark top diameter, in inches, as numeric values from 0
  through 99. Defaults to `0`, using the translated rule. A value of
  `99` preserves the equation template setting.

## Value

A data frame with `model` (source identifier), `volume_type` (source
volume code), `primary_top` (inside bark diameter, inches), and
`errflag` (source translation code). Codes 0, 1, and 6 distinguish
success, an invalid template, and an unsupported translation.

## Examples

``` r
## Translate a source inventory equation for the example species.
nvel_from_fia_code(fia_code = 'BD000006',
                   spcd = example_trees$spcd[1])
#>        model volume_type primary_top errflag
#> 1 R02ALN0202         SV6           6       0
```
