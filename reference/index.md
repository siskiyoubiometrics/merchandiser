# Package index

## Select and price logs

- [`merchandise()`](https://siskiyoubiometrics.com/merchandiser/reference/merchandise.md)
  : Select and scale logs from measured trees
- [`assumptions()`](https://siskiyoubiometrics.com/merchandiser/reference/assumptions.md)
  : Inspect assumptions recorded with a merchandising result
- [`example_trees`](https://siskiyoubiometrics.com/merchandiser/reference/example_trees.md)
  : Example trees for stem calculations

## Define products

- [`product()`](https://siskiyoubiometrics.com/merchandiser/reference/product.md)
  : Define log dimensions, eligibility, and pricing for a product
- [`products()`](https://siskiyoubiometrics.com/merchandiser/reference/products.md)
  : Combine products in cutting priority order

## Record defects

- [`defect()`](https://siskiyoubiometrics.com/merchandiser/reference/defect.md)
  : Record a located stem defect
- [`validate_defects()`](https://siskiyoubiometrics.com/merchandiser/reference/validate_defects.md)
  : Check defect heights and product restrictions
- [`defects_from_stoppers()`](https://siskiyoubiometrics.com/merchandiser/reference/defects_from_stoppers.md)
  : Convert stopper heights to located defects
- [`defect_by_thirds()`](https://siskiyoubiometrics.com/merchandiser/reference/defect_by_thirds.md)
  : Combine defect percentages by stem-volume thirds
- [`example_defects_pnw`](https://siskiyoubiometrics.com/merchandiser/reference/example_defects_pnw.md)
  : Example Pacific Northwest defect records

## Fit and complete heights

- [`fit_height()`](https://siskiyoubiometrics.com/merchandiser/reference/fit_height.md)
  : Fit height relationships by species and group
- [`complete_heights()`](https://siskiyoubiometrics.com/merchandiser/reference/complete_heights.md)
  : Fill missing heights while retaining measured values
- [`predict_height()`](https://siskiyoubiometrics.com/merchandiser/reference/predict_height.md)
  : Predict tree heights and optional intervals

## Measure stems

- [`stem_profile()`](https://siskiyoubiometrics.com/merchandiser/reference/stem_profile.md)
  : Sample diameters and cumulative volumes along stems
- [`dib()`](https://siskiyoubiometrics.com/merchandiser/reference/dib.md)
  : Measure inside bark diameter at a height
- [`dob()`](https://siskiyoubiometrics.com/merchandiser/reference/dob.md)
  : Measure outside bark diameter at a height
- [`height_at_dib()`](https://siskiyoubiometrics.com/merchandiser/reference/height_at_dib.md)
  : Locate a specified inside bark diameter
- [`height_at_dob()`](https://siskiyoubiometrics.com/merchandiser/reference/height_at_dob.md)
  : Locate a specified outside bark diameter
- [`stem_volume()`](https://siskiyoubiometrics.com/merchandiser/reference/stem_volume.md)
  : Measure solid volume between stem bounds
- [`green_weight()`](https://siskiyoubiometrics.com/merchandiser/reference/green_weight.md)
  : Convert solid stem volume to merchandising weight
- [`get_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/get_taper_model.md)
  : Inspect a taper model and its required inputs
- [`taper_models()`](https://siskiyoubiometrics.com/merchandiser/reference/taper_models.md)
  : List registered taper equations by form or species
- [`has_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/has_taper_model.md)
  : Check availability of taper equation identifiers
- [`default_taper_models`](https://siskiyoubiometrics.com/merchandiser/reference/default_taper_models.md)
  : Shipped default taper models by species
- [`species_reference`](https://siskiyoubiometrics.com/merchandiser/reference/species_reference.md)
  : Species reference for joining tree lists

## Estimate biomass and carbon

- [`biomass()`](https://siskiyoubiometrics.com/merchandiser/reference/biomass.md)
  : Estimate dry biomass and carbon in metric tonnes
- [`carbon_fraction()`](https://siskiyoubiometrics.com/merchandiser/reference/carbon_fraction.md)
  : Look up the carbon fraction of dry biomass
- [`nsvb_division()`](https://siskiyoubiometrics.com/merchandiser/reference/nsvb_division.md)
  : Look up ecological division by state and county
- [`nsvb_division_xy()`](https://siskiyoubiometrics.com/merchandiser/reference/nsvb_division_xy.md)
  : Look up ecological division from coordinates
- [`nsvb_division_polygons`](https://siskiyoubiometrics.com/merchandiser/reference/nsvb_division_polygons.md)
  : Ecological division boundaries

## Work with tree lists

- [`status_codes()`](https://siskiyoubiometrics.com/merchandiser/reference/status_codes.md)
  : Look up calculation status codes
- [`threads()`](https://siskiyoubiometrics.com/merchandiser/reference/threads.md)
  : Inspect or set the calculation thread limit
- [`with_threads()`](https://siskiyoubiometrics.com/merchandiser/reference/with_threads.md)
  : Evaluate a calculation with a temporary thread limit
- [`example_trees_pnw`](https://siskiyoubiometrics.com/merchandiser/reference/example_trees_pnw.md)
  : Example Pacific Northwest trees
- [`example_trees_south`](https://siskiyoubiometrics.com/merchandiser/reference/example_trees_south.md)
  : Example southern plantation trees

## Fit and manage taper equations

- [`fit_taper()`](https://siskiyoubiometrics.com/merchandiser/reference/fit_taper.md)
  : Fit taper coefficients to repeated stem measurements
- [`as_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/as_taper_model.md)
  : Convert a taper fit to a usable model
- [`register_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/register_taper_model.md)
  : Register a private taper model for the session
- [`unregister_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/unregister_taper_model.md)
  : Remove a private taper model from the session
- [`check_taper_models()`](https://siskiyoubiometrics.com/merchandiser/reference/check_taper_models.md)
  : Check model identifiers, scope, and required inputs
- [`taper_manifest()`](https://siskiyoubiometrics.com/merchandiser/reference/taper_manifest.md)
  : Record registered taper identifiers and versions
- [`taper_model_from_coefficients()`](https://siskiyoubiometrics.com/merchandiser/reference/taper_model_from_coefficients.md)
  : Construct a taper model from metric coefficients
- [`new_taper_model()`](https://siskiyoubiometrics.com/merchandiser/reference/new_taper_model.md)
  : Create a taper model from diameter and volume functions
- [`example_stem_measurements`](https://siskiyoubiometrics.com/merchandiser/reference/example_stem_measurements.md)
  : Example stem measurements

## Source library

- [`nvel_default_equation()`](https://siskiyoubiometrics.com/merchandiser/reference/nvel_default_equation.md)
  : Look up a source library equation identifier
- [`nvel_from_fia_code()`](https://siskiyoubiometrics.com/merchandiser/reference/nvel_from_fia_code.md)
  : Translate an inventory equation identifier
- [`nvel_rules()`](https://siskiyoubiometrics.com/merchandiser/reference/nvel_rules.md)
  : Record source library measurement rules
- [`nvel_source_revision()`](https://siskiyoubiometrics.com/merchandiser/reference/nvel_source_revision.md)
  : Inspect the pinned source library revision
- [`products_from_nvel_rules()`](https://siskiyoubiometrics.com/merchandiser/reference/products_from_nvel_rules.md)
  : Convert explicit source rules to products
