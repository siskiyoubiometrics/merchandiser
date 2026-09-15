# merchandiser 0.5.0

* Batched provider preparation and optimizer back-pointers reduce the measured three-product cascade time for 1,000 distinct stems from 24.08 to 2.519 seconds and the three-product optimizer time with each product capped at eight logs from 3.981 to 1.2 seconds per stem on an AMD Ryzen 9 3900X with eight threads, with identical public results.
* Worker counts must be whole numbers of at least one. Counts above the detected core count are clamped with a warning, including `MERCHANDISER_THREADS`. An unavailable core count uses one worker.
* Diameters above 400 inches and heights above 500 feet return row statuses 2 and 3. `stem_profile()` rejects `step` below 0.05 feet and empty `dbh`, `ht`, `model`, or `step`. `predict_height()` rejects `nsim` above 1e6. `dob()` applies the same diameter bounds as `dib()`.
* Both bucking algorithms retain fitting half foot lengths at decimal segment boundaries within 1e-9 feet. Cascade uses the smallest allowed product `min_sed` to find the top, with the first product breaking bark-basis ties. This corrects tops for products listed out of decreasing diameter order. Optimization rejects zero prices and names the product.
* A negative, nonfinite, or at-or-above-tree stump returns new status 411, `invalid_stump_height`, on that tree. Invalid auxiliary domains return new status 55, `auxiliary_out_of_domain`, in stem functions, model diagnostics, and merchandising. Undeclared auxiliary names are checked before provider calls.
* New defect status 405, `defect_unknown_tree`, identifies records without a matching tree. Merchandising drops those records before consistency checks with one warning naming their tree identifiers. New status 409, `defect_product_misplaced`, identifies a product named on a cull, end, or sweep. New status 407, `defect_unknown_product`, rejects unknown restrictions on their tree. Defect and tree identifiers must have the same numeric or character mode. Exact duplicate records are dropped with a message and overlapping valid culls merge. Defect construction retains range and consistency checks in `validate_defects()`.
* Merchandising retains status 52 for explicit models outside their species scope. New status 408, `invalid_pruned_height`, rejects negative, nonfinite, or above-tree pruning heights. Unknown biomass divisions return row status 8 instead of national coefficients.
* `defect_by_thirds()` requires numeric percentages, rejecting factors and logicals. Green weight specific gravities cannot exceed 2. Wood and bark moisture percentages cannot exceed 300. Bark volume percentage cannot exceed 100.
* `complete_heights()` replaces invalid measured heights with missing values and warns truthfully. Valid measured heights remain unchanged. `fit_height()` warns when missing or infinite input rows are omitted. Explicit models no longer bypass `taper_map` validation.
* Unknown `nvel_default_equation()` variants error with their region. `nvel_from_fia_code()` rejects `primary_top` outside zero through 99. `nvel_rules()` requires scalar fields and a minimum length at most the maximum. Products reject zero `max_logs`, zero `max_dbh`, and empty product tables. `products()` requires at least one product. Compiled taper model registration validates callback fields too.
* `fit_taper()` and `fit_height()` omit invalid measurement rows with one warning naming the count and reasons. Diameters cannot exceed 400 inches, total heights cannot exceed 500 feet, measurement heights must lie within the tree, and species must occur in `species_reference`. Missing and invalid rows share the warning.
* `nvel_from_fia_code()` requires exactly eight characters. NVEL location codes must be numeric whole numbers. `defect_by_thirds()` returns status 3 for out-of-range tree heights. `defects_from_stoppers()` rejects negative, nonfinite, or above-500-foot heights and names the argument. Optional missing stopping heights still mean no stop.
* Recycling errors name the argument and conflicting sizes. Product field errors name the field, scaling choice errors list accepted choices, and coefficient errors name `coefficients`. Coefficient model constructors reject nonnumeric `stump_ht` before unit conversion. Unknown merchandising status codes are named `unknown_status`.

* `default_taper_models` records shipped species-specific defaults. Stem functions and `merchandise()` return status 404 when no default exists. Explicit models and merchandising taper maps keep precedence.
* `stem_volume()` and `stem_profile()` accept `from`, `to`, `from_dib`, `from_dob`, `to_dib`, and `to_dob`. Height bounds and profile `step` use feet. Both use `stump_ht = 1`, and profiles default to half foot spacing.
* `taper_model_from_coefficients()` and `as_taper_model()` now default to a one foot stump instead of ground level. Default stem volumes for supplied and fitted models therefore exclude that stump section. `as_taper_model()` is a plain function with visible arguments and retains the fit's species scope.
* `fit_taper()` accepts measurement vectors instead of a data frame and defaults to `max_burkhart`. Its `spcd` vector records each measured tree's species, and the fitted scope is the sorted unique species set.
* Taper models use `form` instead of `family`, with class version 0.5. Constructor `source` defaults to `NULL`, preserving the existing stored provenance. `check_taper_models()` accepts extra measurements through `...`.
* Defect and residual section columns are `start_height` and `end_height`. Example tree identifiers are `tree_id`. `defect_by_thirds()` replaces `defect_pct_from_thirds()` and takes `lower`, `middle`, and `upper` percentages. Stopping-height conversion requires `topwood_product` and uses `pulp_tree = FALSE`.
* `status_codes()` and `check_taper_models()` return `status`. Status 406 is removed, status 412 is `optimizer_failed`, and unlisted stem codes are named `unknown_status`. `nsvb_division()` returns `value` and `status`, including status 8 for counties without a division.
* Biomass reports `dry_aboveground_no_foliage`. Model-selection assumptions retain `species_default` or `caller_model`, with equation provenance available from `taper_models()`.
* `green_weight()` removes its reserved `...` and shows plain component and moisture defaults. `nvel_default_equation()` removes its unused `product` argument.

* The product and model species scopes are `spcd`.
* Value optimization now assigns any eligible product at each cut, independently of product order. Ties prefer fewer logs, then longer logs lower on the stem. Product log limits restart at cull and restriction boundaries. Trees run through the shared parallel bucking engine.
* The optimal bucking guide compares strategies, mapped defects, prices, and measured computation costs.

* Tree inputs now use inches and feet. Public calculation functions accept numeric Forest Inventory and Analysis species codes through `spcd`. Species names and symbols are no longer resolved in arguments. The species reference table remains available for joins.
* `merchandise()` requires `tree_id` first. `pruned_ht` replaces `pruned` and records clear wood from the ground to the pruning height. `strategy` selects the priority cascade or value optimization. Optimization requires a price for every product.
* `merchandise()` removes `id`, `species`, `pruned`, `utilization_top`, `utilization_top_basis`, `utilization_height`, `curvature_scale`, `report_also`, `scaling`, `currency`, `units`, `status`, `preset`, `region`, `forest`, and `district`. Product specifications define the usable top.
* Cascade cuts products in their supplied order. `priority` is removed. Continuous candidate lengths use an internal half foot grid. `length_round` rounds the nominal length down to the mill's scaling increment. `max_logs` replaces `max_logs_per_segment`. `requires_pruned` replaces the product field `pruned`.
* Product fields `lengths`, `length_step`, `min_boundary_length`, `allow_after_higher_product`, `allow_lower_products`, `accepts_pulp_restriction`, `max_crook`, `max_rot_pct`, `grade`, `diameter_basis`, `sold_by`, `specification_source`, all `meta_` columns, and legacy field aliases are removed. `max_sweep` is a numeric percentage.
* Defects contain only `tree_id`, `start_height`, `end_height`, `effect`, `product`, and `percent`. Effects are `cull`, `restrict`, `end`, and `sweep`. Cull and restriction boundaries split the stem and restart product priority and log counts. Missing `end_height` means the tree top. The former `pulp`, `exclude`, `rot`, `crook`, `fork`, and `break` effects are removed. `defect_id`, `category`, `pathology`, and `source_record_id` are removed. No defect percentage is applied inside merchandising.
* `defect()`, `defects_from_stoppers()`, `validate_defects()`, and `stem_profile()` rename `id` to `tree_id`. Stopping heights now produce the four supported effects. The standalone thirds utility uses species defaults and leaves application of its percentage to the user's script.
* A log occupies nominal length plus trim. Every scaled quantity and cubic volume covers only the nominal body. Physical log ends determine diameter limits. Trim is a separate residual row and contributes no volume. Separate trim and physical cubic columns and paired gross and net scale columns are removed.
* Results contain `logs`, `residuals`, `status`, `assumptions`, and replay inputs in `call`. Each table begins with `tree_id`. Status contains nonzero tree results only. Value appears on logs when any product is priced. The former `trees`, `scales`, `values`, `defect_accounting`, `run_metadata`, and `diagnostics` tables are removed.
* Stem measurement functions accept `spcd` and choose a taper model by species unless `model` is supplied. `stem_volume()` renames `bark` to logical `inside_bark`. One-row-per-input functions have no identifier argument.
* Public vector-to-table switches named `status` are removed. Those functions always return data frames with a status column. Public `units` arguments are removed from stem, height, taper fitting, model construction, biomass, and green weight functions.
* `new_taper_model()` and `taper_model_from_coefficients()` remove `units` and `notes` and retain `bark_ratio` and `source`. Model lookups use `model`, registration takes `x`, and identifiers assigned by constructors use `id`. Model species scopes keep `spcd`.
* `biomass()` returns dry mass and carbon in metric tonnes, with carbon dioxide equivalent in `tco2e`. National coefficients are selected by `division = 0`. Invalid input rows remain aligned. Green biomass columns and the `system`, `id`, `units`, and `status` arguments are removed from the public biomass interface. `green_weight()` returns merchandising weight in short tons and is not a carbon calculation. `biomass_component()` is removed.
* `as_taper_model()` retains the fitted species scope and removes its `species` override. `fit_taper()` no longer treats a species argument as a column name.
* Example trees retain numeric species codes. Pacific Northwest heights are complete and carry measured or predicted origin. The southern example contains four plantation stands with distinct stand ages. New mapped Pacific Northwest defect records illustrate culls, restricted forks, broken tops, and sweep. Product examples are unexported source scripts, not shipped specifications.
* Removed functions: `tree_cubic_volume()`, `validate_products()`, `validate_taper_model()`, `model_capabilities()`, `check_taper_manifest()`, `compare_bucking_prices()`, `apply_defect_pct()`, `check_model_ids()`, `check_registry_manifest()`, `co2e()`, `example_product_names()`, `example_products()`, `has_model()`, `list_presets()`, `mc_status_codes()`, `merch_defect()`, `merch_piece_count()`, `merch_product()`, `merch_products()`, `merch_summary()`, `merch_summary_by_product()`, `merch_volume()`, `optimize_bucking()`, `preset()`, `product_preset()`, `product_presets()`, `product_summary()`, `product_summary_by_tree()`, `register_model()`, `registry_manifest()`, `species_lookup()`, `stand_table()`, `stock_table()`, `taper_model()`, `taper_model_spec()`, `tv_models()`, `tv_species`, `tv_status_codes()`, `tv_threads()`, `unregister_model()`.

* Source board foot rules scale the rounded length in whole or even feet according to `length_round`.
* `sed` and `led` measure the physical ends on the product's bark basis, which `inside_bark` records and product diameter limits use. `scaling_diameter` is the inside bark diameter used and rounded by the source board foot rule, and is `NA` for cubic, green weight, and cord products.
* Cubic, green weight, and cord products scale the nominal body and report `scaling_length` for the mill's records only.
* `tree_log_count()` is removed. Count logs from the logs table of `merchandise()`. `stem_volume()` needs no products and measures from `stump_ht` to the tip or the inside bark diameter supplied in `to_dib`.

# merchandiser 0.4.0

- Products now use `volume_unit`, `inside_bark`, `split_scale`, and `round`.
- Scaling diameter preprocessing supports downward and nearest-inch rounding.
- Log tables report `gross_scale`, `net_scale`, and `volume_unit`.
- Stand tables report `merchandising_status`.
- Example inventories use integer `tree` numbers and retain separate plot labels.
- Documentation uses named calls, magrittr pipes, and formatted output tables.
- Guides cover product specifications, defect, heights, taper, scaling, biomass,
  carbon, inventory summaries, prices, and recorded assumptions.
- The home and getting-started examples include Scribner sawlogs and green-ton pulp.
- Legacy measurements migrate with a deprecation message only when the product
  fields can express them. Other settings stop and identify `report_also` as the
  advanced reporting argument for measurement forms still available there.
- The Flewelling Black Hills `SF_CORR` routine now applies its correlation cap only
  when the higher height exceeds breast height. Fixture comparisons did not cover
  this branch. A standalone header comparison checks agreement with compiled
  Fortran. A regression test pins the corrected below-breast-height value.
- Caller-supplied missing quantities propagate through `apply_defect_pct()`
  without a warning or status.
