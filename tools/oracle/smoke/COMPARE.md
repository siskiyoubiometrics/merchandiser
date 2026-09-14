# Single vs double oracle comparison

Inputs: `out_single.csv` and `out_double.csv` (23 rows each, same `input.csv`).

Single values were written with `%.9g`, double with `%.17g`. Relative difference is |single - double| / |double|.

## Per output family (max over all rows and elements)

| family | values compared | values differing | max abs diff | where | max rel diff | where |
|---|---|---|---|---|---|---|
| ERRFLAG | 23 | 0 | 0 |  | 0.000e+00 |  |
| TLOGS | 11 | 0 | 0 |  | 0.000e+00 |  |
| NOLOGP | 11 | 0 | 0 |  | 0.000e+00 |  |
| NOLOGS | 11 | 0 | 0 |  | 0.000e+00 |  |
| VOL | 165 | 57 | 2.75659e-05 | row 18 VOL1 (70.7532883 vs 70.7533158658945) | 2.653e-06 | row 18 VOL15 (0.701058745 vs 0.701056884972551) |
| DIB | 4 | 4 | 6.44292e-07 | row 21 DIB (9.56771851 vs 9.567719154291717) | 6.734e-08 | row 21 DIB (9.56771851 vs 9.567719154291717) |
| DOB | 4 | 3 | 8.54663e-07 | row 21 DOB (10.2001209 vs 10.200121754663037) | 8.379e-08 | row 21 DOB (10.2001209 vs 10.200121754663037) |
| STEMHT | 4 | 4 | 9.7428e-06 | row 21 STEMHT (93.2089233 vs 93.2089330428036) | 1.062e-07 | row 20 STEMHT (84.5059128 vs 84.5059217738033) |
| DRYBIO | 30 | 30 | 0.000500797 | row 18 DRYBIO1 (2612.10767 vs 2612.1071692027986) | 5.132e-06 | row 18 DRYBIO9 (9.43454647 vs 9.434594887035537) |
| GRNBIO | 30 | 30 | 0.0022066 | row 18 GRNBIO1 (4751.15527 vs 4751.157476595796) | 5.727e-06 | row 18 GRNBIO9 (17.160471 vs 17.160569277052975) |
| LOGLEN | 220 | 0 | 0 |  | 0.000e+00 |  |
| BOLHT | 231 | 7 | 5.3e-06 | row 15 BOLHT8 (53.0999947 vs 53.099999999999994) | 9.981e-08 | row 15 BOLHT8 (53.0999947 vs 53.099999999999994) |
| LOGDIA | 693 | 95 | 1.08931e-05 | row 16 LOGDIA_6_2 (9.42486954 vs 9.4248586468943) | 1.156e-06 | row 16 LOGDIA_6_2 (9.42486954 vs 9.4248586468943) |
| LOGVOL | 1540 | 113 | 0.000410268 | row 18 LOGVOL_7_1 (1350.10535 vs 1350.105760268057) | 3.684e-07 | row 15 LOGVOL_4_7 (0.904548526 vs 0.9045481927322231) |

## VOL(1..15), DIB, DOB, STEMHT, NOLOGP, NOLOGS per column

| column | values | differing | max abs diff | max rel diff | where (rel) |
|---|---|---|---|---|---|
| VOL1 | 11 | 10 | 2.75659e-05 | 3.896e-07 | row 18 VOL1 (70.7532883 vs 70.7533158658945) |
| VOL2 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL3 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL4 | 11 | 9 | 2.0237e-05 | 3.043e-07 | row 18 VOL4 (66.5007248 vs 66.50074503701293) |
| VOL5 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL6 | 11 | 10 | 5e-08 | 4.000e-08 | row 9 VOL6 (0.600000024 vs 0.6) |
| VOL7 | 11 | 7 | 4.38998e-07 | 2.341e-07 | row 18 VOL7 (1.70772266 vs 1.70772305986287) |
| VOL8 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL9 | 11 | 1 | 1e-09 | 1.000e-08 | row 15 VOL9 (0.100000001 vs 0.1) |
| VOL10 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL11 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL12 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL13 | 11 | 0 | 0 | 0.000e+00 |  |
| VOL14 | 11 | 10 | 4.63183e-06 | 2.503e-06 | row 18 VOL14 (1.85043263 vs 1.8504372618316949) |
| VOL15 | 11 | 10 | 1.86003e-06 | 2.653e-06 | row 18 VOL15 (0.701058745 vs 0.701056884972551) |
| DIB | 4 | 4 | 6.44292e-07 | 6.734e-08 | row 21 DIB (9.56771851 vs 9.567719154291717) |
| DOB | 4 | 3 | 8.54663e-07 | 8.379e-08 | row 21 DOB (10.2001209 vs 10.200121754663037) |
| STEMHT | 4 | 4 | 9.7428e-06 | 1.062e-07 | row 20 STEMHT (84.5059128 vs 84.5059217738033) |
| NOLOGP | 11 | 0 | 0 | 0.000e+00 |  |
| NOLOGS | 11 | 0 | 0 | 0.000e+00 |  |

## Rows with nonzero ERRFLAG

| row | call | VOLEQ | ERRFLAG single | ERRFLAG double | meaning (from source) | note |
|---|---|---|---|---|---|---|
| 17 | a | F00FW2W202 | 4 | 4 | no usable height: HTTOT below 5 (4.5 for some models) and no product height (profile.f:121,235,256; volinit.f:340,812; nsvb.f:209,219,820) | VOLUMELIBRARY R6 DF with no height (expected nonzero ERRFLAG) |

## String outputs and presence mismatches

None: VOLEQ_OUT and every present/absent pattern agree between the two builds.

## Per-row ERRFLAG and VOL(1..5) side by side

| row | call | VOLEQ | ERR s/d | VOL1 s | VOL1 d | VOL2 s | VOL2 d | VOL4 s | VOL4 d |
|---|---|---|---|---|---|---|---|---|---|
| 9 | a | F00FW2W202 | 0/0 | 77.5999985 | 77.600000000000009 | 393 | 393 | 70.5 | 70.499999999999986 |
| 10 | a | F03FW2W263 | 0/0 | 43.7000008 | 43.700000000000003 | 200 | 200 | 38.5999985 | 38.599999999999994 |
| 11 | a | F00FW2W202 | 0/0 | 77.5999985 | 77.600000000000009 | 372 | 372 | 74.7999954 | 74.799999999999997 |
| 12 | a | I00FW3W202 | 0/0 | 89.4000015 | 89.400000000000006 | 490 | 490 | 80.3999939 | 80.400000000000006 |
| 13 | a | I00FW2W202 | 0/0 | 80.3000031 | 80.300000000000011 | 410 | 410 | 72.0999985 | 72.099999999999994 |
| 14 | a | 841CLKE131 | 0/0 | 24.1000004 | 24.100000000000001 | 97 | 97 | 22 | 22 |
| 15 | a | 900CLKE371 | 0/0 | 36.2999992 | 36.299999999999997 | 147 | 147 | 29.8999996 | 29.899999999999999 |
| 16 | a | A00F32W098 | 0/0 | 132.300003 | 132.30000000000001 | 570 | 570 | 120.300011 | 120.3 |
| 17 | a | F00FW2W202 | 4/4 | 0 | 0 | 0 | 0 | 0 | 0 |
| 18 | c | NVBM240202 | 0/0 | 70.7532883 | 70.753315865894507 | 365 | 365 | 66.5007248 | 66.500745037012933 |
| 19 | c | NVB0230131 | 0/0 | 26.9821682 | 26.982162997996603 | 100 | 100 | 21.5945015 | 21.594499206179712 |
