# Example Pacific Northwest defect records

Located records supply each supported defect effect for the Pacific
Northwest example trees. Join them by tree identifier when cutting those
trees, with interval heights in feet.

## Usage

``` r
example_defects_pnw
```

## Format

A data frame with the following columns:

- tree_id:

  Identifier linking this record to tree_id in example_trees_pnw.

- start_height:

  Start of the defect section above ground, feet.

- end_height:

  End of the defect section above ground, feet. Missing means the top of
  the tree.

- effect:

  Cutting effect, cull, restrict, end, or sweep.

- product:

  Required product name for restrict, pulp in this example. Missing for
  other effects.

- percent:

  Whole-number sweep percentage. Missing for other effects.
