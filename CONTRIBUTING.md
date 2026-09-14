# Contributing to merchandiser

merchandiser is an open-source R package released under the MIT license.
Contributions are welcome as issues and pull requests on GitHub.

## Reporting a problem

Open an issue at https://github.com/siskiyoubiometrics/merchandiser/issues.
Include the package version (`packageVersion('merchandiser')`), the call you
made, the shipped example data or a small data frame that reproduces it, and
the output or error you saw.

## Proposing a change

1. Open an issue first for anything that changes a calculation, a default, or
   an exported interface, so the change can be discussed before the work.
2. Fork the repository and make the change on a branch.
3. Add or update tests under `tests/testthat`. A change to scaling, taper, or
   bucking arithmetic needs a test case with a documented source for the
   expected value.
4. Run `devtools::document()`, `devtools::test()`, `lintr::lint_package()`
   (zero lints), and `R CMD check --as-cran` (zero errors, zero warnings).
5. Open a pull request describing what changed and why.

## Code style

Package code follows the tidyverse style guide with two-space indentation and
snake_case names, enforced by `lintr` with the configuration in `.lintr`.
Examples and articles use `magrittr` pipes with one step per line, a `##`
comment before each step, single quotes for strings, and named arguments.

## Ported material

Parts of the package are independent ports of routines from the US Forest
Service National Volume Estimator Library. Credits for ported material are
recorded in `inst/COPYRIGHTS`. A contribution that ports additional material
must add its credit there.

## License

By contributing you agree that your contribution is licensed under the MIT
license in `LICENSE.md`.
