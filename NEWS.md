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
