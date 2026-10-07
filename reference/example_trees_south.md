# Example southern plantation trees

Synthetic plantation stands provide tree ages and stopping heights for
merchandising. Use the age and stopper columns with diameters in inches
and heights in feet.

## Usage

``` r
example_trees_south
```

## Format

A data frame with the following columns:

- stand:

  Stand identifier, text.

- plot:

  Plot identifier, text.

- tree_id:

  Tree identifier, unique within the example table.

- spcd:

  Numeric inventory species code.

- dbh:

  Diameter at breast height outside bark, inches.

- ht:

  Total tree height above ground, feet.

- age:

  Tree age, years.

- saw_stop:

  Height where only pulpwood may be cut above it, feet. Missing means no
  stop.

- pulp_stop:

  Height where merchandising ends, feet. Missing means no stop.

- jump_butt:

  Height above a cull butt section, feet. Missing means no jump.

- longitude:

  Longitude, decimal degrees.

- latitude:

  Latitude, decimal degrees.
