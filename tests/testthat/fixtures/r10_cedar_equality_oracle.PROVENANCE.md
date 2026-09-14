# Region 10 cedar equality oracle

Generated on 2026-09-06 with both pinned drivers at NVEL commit
`38548071d5aa652bb90c7f111f86b427f798a1c9`.

```text
tools/oracle/driver/nvel_driver_double /tmp/r10_equal_input.csv /tmp/r10_equal_output.csv
tools/oracle/driver/nvel_driver_single /tmp/r10_equal_input.csv /tmp/r10_equal_output_single.csv
```

Each `b` call requested CALCDIA at 40 feet and HT2TOPD at the stated
`STEMDIB`. The source input used Region 10, forest `01`, district `01`, product
`01`, a 6-inch primary top, a 4-inch secondary top, and a 1-foot stump. The
stored CSV contains only the inputs and outputs needed by the package test.
