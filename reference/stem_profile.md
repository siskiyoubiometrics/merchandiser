# Sample diameters and cumulative volumes along stems

Evaluate a taper equation at regular height intervals, including the
final bound. Invalid trees retain a row with missing measurements and a
status. The plot method distinguishes inside and outside bark profiles.

## Usage

``` r
stem_profile(
  tree_id,
  dbh,
  ht,
  spcd,
  model = NULL,
  step = 0.5,
  from = NULL,
  to = NULL,
  from_dib = NULL,
  from_dob = NULL,
  to_dib = NULL,
  to_dob = NULL,
  stump_ht = 1,
  ...
)
```

## Arguments

- tree_id:

  Unique, nonmissing identifiers for the input trees, as an atomic
  vector. Required, without a default. Repeated output rows retain these
  identifiers.

- dbh:

  Outside bark diameter at breast height, in inches. Accepts numeric
  values greater than zero and no greater than 400. Required, with no
  default. Invalid measurement rows return missing results with a
  status.

- ht:

  Total height above ground, in feet. Accepts numeric values greater
  than zero and no greater than 500. Required, with no default.
  Measurement heights and section bounds must fall within the tree.

- spcd:

  Species identifier as a numeric vector of positive whole-number codes.
  Required, with no default. The species must be recognized for species
  properties and within the selected equation's scope.

- model:

  Taper equation identifier as a character vector. Defaults to `NULL`,
  selecting the stored species default. A supplied identifier overrides
  that choice. Scalar identifiers recycle across trees.

- step:

  Spacing between profile measurements, in feet. Accepts finite numeric
  values of at least 0.05, either scalar or one per tree. Defaults to
  `0.5`. The exact upper bound is included even when it does not fall on
  the spacing.

- from:

  Lower section height above ground, in feet. Accepts numeric values
  from zero through total height. Defaults to `NULL`, using `stump_ht`.
  Supply at most one of `from`, `from_dib`, and `from_dob`.

- to:

  Upper section height above ground, in feet. Accepts numeric values
  from zero through total height. Defaults to `NULL`, using the tip.
  Supply at most one of `to`, `to_dib`, and `to_dob`. The resulting
  upper bound must exceed the lower bound.

- from_dib:

  Lower section boundary specified by inside bark diameter, in inches.
  Accepts positive numeric values. Defaults to `NULL`, leaving this
  diameter bound unset. It locates the boundary independently of the
  volume bark basis and cannot accompany another lower bound.

- from_dob:

  Lower section boundary specified by outside bark diameter, in inches.
  Accepts positive numeric values. Defaults to `NULL`, leaving this
  diameter bound unset. It locates the boundary independently of the
  volume bark basis and cannot accompany another lower bound.

- to_dib:

  Upper section boundary specified by inside bark diameter, in inches.
  Accepts positive numeric values. Defaults to `NULL`, leaving this
  diameter bound unset. It locates the boundary independently of the
  volume bark basis and cannot accompany another upper bound.

- to_dob:

  Upper section boundary specified by outside bark diameter, in inches.
  Accepts positive numeric values. Defaults to `NULL`, leaving this
  diameter bound unset. It locates the boundary independently of the
  volume bark basis and cannot accompany another upper bound.

- stump_ht:

  Stump height above ground, in feet. Accepts finite, nonnegative
  numeric values below total height. Defaults to `1`. Used as the lower
  section bound unless another bound is supplied.

- ...:

  Additional named inputs accepted by the selected model, with none
  supplied by default. Numeric inputs must be finite: positive
  `upper_ht1`, `upper_ht2`, and `site_index` use feet, positive
  `upper_d1` and `upper_d2` use inches, and positive `basal_area` uses
  square feet per acre. `form_class` accepts positive numbers.
  `bark_ratio` is inside diameter divided by outside diameter, greater
  than zero and no greater than one. `decay_class` accepts whole numbers
  from 1 through 5 and `cull` accepts percentages from 0 through 100.
  `upper_bark` accepts `'ib'` or `'ob'`. Upper heights and diameters
  must be supplied in pairs. Only inputs declared by the selected model
  are accepted.

## Value

A `stem_profile` data frame with `tree_id` (identifier), `h` (feet above
ground), `dib` and `dob` (inches), `cum_volume_ib` and `cum_volume_ob`
(cubic feet above the lower bound), and `status` (integer code). Trees
remain in input order, with ascending heights within each tree.

## Examples

``` r
## Sample the first example tree
profile <- stem_profile(tree_id = example_trees$tree_id[1],
                        dbh = example_trees$dbh[1],
                        ht = example_trees$ht[1],
                        spcd = example_trees$spcd[1],
                        model = example_trees$model[1],
                        step = 5)

## Draw the sampled profile
plot(profile)
```
