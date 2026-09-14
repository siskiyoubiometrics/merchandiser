# Dynamic non-Alaska three-point oracle probes

These values were generated on 2026-09-06 with the read-only driver binaries
from the main `treevolume` repository. Both binaries reported NVEL version
`20260729`. The linked reference clone was pinned to NVEL commit
`38548071d5aa652bb90c7f111f86b427f798a1c9`.

The commands were:

```text
tools/oracle/driver/nvel_driver_double tests/testthat/fixtures/flewelling_dynamic_3pt_oracle_input.csv /tmp/flewelling_dynamic_3pt_oracle.double.raw.csv
tools/oracle/driver/nvel_driver_single tests/testthat/fixtures/flewelling_dynamic_3pt_oracle_input.csv /tmp/flewelling_dynamic_3pt_oracle.single.raw.csv
```

The fixture retains the echoed inputs plus `BUILD`, `ERRFLAG`, and `DIB` from
the raw output. `UPPER_BARK` records the Fortran model basis. West-side upper
measurements are outside bark and INGY measurements are inside bark.

SHA-256 values:

```text
c1206e65234dd648767c26f5ebecf0a7720754c9aee6279b77bccf2675aa23ce  flewelling_dynamic_3pt_oracle_input.csv
ea511ea9d222a220932494dc7a58fae089bfd7e031d3e59491a256ecfd53a00e  flewelling_dynamic_3pt_oracle.double.raw.csv
f235abeee19ab080fa38dde62c5c2826300d7fded6cc5c57401f98309474951a  flewelling_dynamic_3pt_oracle.single.raw.csv
```
