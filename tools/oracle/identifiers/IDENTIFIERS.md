# NVEL VOLEQ identifier enumeration

Source: read-only Fortran at `<local reference clone>/tools/oracle/nvel`
(referenced below by filename only). Companion inventory:
`<local reference clone>/tools/oracle/NVEL_INVENTORY.md`, sections 2, 3, 3.12, 5.

`identifiers.csv` has 659 rows: 581 rows are distinct 10-character VOLEQ *literals* actually found
quoted in the source, and 78 rows are *pattern* rows (marked `PATTERN ROW` in the `notes` column,
`voleq` written with `?` wildcards, e.g. `???FW2????`) representing a dispatch rule that is tested
by substring comparison rather than enumerated as a fixed literal. There is no single authoritative
enumeration of "every VOLEQ" in this codebase (NVEL_INVENTORY.md Sec 2.2) - this is the most complete
reconstruction obtainable from the quoted literals plus the substring dispatch rules, not a closed set.

## 1. Counts

### By family

| family | rows |
|---|---:|
| direct_volume | 350 |
| flewelling_2pt | 157 |
| blm_taper | 35 |
| r4_driver | 21 |
| r10_taper | 20 |
| r5_taper | 19 |
| r2_taper | 18 |
| flewelling_3pt | 12 |
| r1_taper | 7 |
| r12_taper | 6 |
| nsvb | 6 |
| clark_r9 | 4 |
| behre_taper | 3 |
| clark_r8 | 1 |
| **total** | **659** |

No row was left classified as `unknown` - every literal and every substring test found resolved to
one of the families above, though several rest on the pattern-row mechanism described below rather
than a literal example (`clark_r8` has exactly one row, a pattern row, because no literal example of
an unqualified region-8 Clark VOLEQ was found among the quoted strings - see Section 4 "Could not
determine").

### By profile_based

| profile_based | rows |
|---|---:|
| FALSE (direct volume/combined-variable, no stem-profile d2h/h2d) | 350 |
| TRUE (stem-profile / taper model provides diameter-at-height) | 309 |

`nsvb` is counted `TRUE`: `NVB_DibAtHT`/`NVB_DobAtHT` (nsvb.f:709,726) do provide diameter at an
arbitrary height, but by inverting a single Kozak-type volume-ratio equation
(`nsvb.f:942-950`), not by evaluating a segmented taper curve - see NVEL_INVENTORY.md Sec 5.4 and the
caveat repeated in every `nsvb` row's `notes` column.

### Literal vs. pattern rows

- 581 rows are literal 10-character strings actually quoted in the source (436 unique from
  `voleqdef.f`, 205 unique and non-spurious from `fiaeq2nveleq.for`, with duplicates across the two
  files collapsed to one row - 436+205=641 minus 60 shared strings = 581).
- 78 rows are pattern rows for dispatch rules resolved by `VOLEQ(a:b).EQ.'xxx'` substring tests
  rather than literal enumeration (see Section 3).

## 2. VOLEQ 10-character layout, as verified

`CHARACTER*10` throughout. Reconstructed from positional slicing across `voleqdef.f`,
`fia_volinit.for`, `dvest.f`, `blmtap.f`, `calcdia.f`, `ht2topd.f` - no single format-spec comment
exists anywhere in the source (NVEL_INVENTORY.md Sec 2.1, re-confirmed independently here):

| Position(s) | Field | Notes |
|---|---|---|
| 1 | region/equation-family code | ASCII digit `'1'`-`'9'` for USFS Region defaults, or a single letter denoting a non-R1-R10 family: `A`/`a`=Alaska(R10) in the FIA alias path, `I`/`i`=R1 alias, `H`/`h`=R5 alias, `F`=R6 alias (bug: tests `'F'` twice, never lowercase `'f'` - `fia_volinit.for:141-142`), `B`/`b`=R7 alias in the FIA path but ALSO ="BLM equation" in `blmtap.f`/`blmvol.f` (context-dependent), `P`/`R`/`N`/`S`=FIA national model families (Pacific NW/Rocky Mtn/North/South), `M`=Army-base, `C`=BIA Canadian Honer. See Section 4 for a further ambiguity found in this port (the letter `I` also appears as a literal position-1 character inside Region 6 default-table strings, unrelated to the FIA R1-alias meaning). |
| 2-3 | geographic code | National Forest number (`FORST`) for R1-R10 defaults, or a synthesized `GEOAREA`/`GEOSUB` code for FIA/special paths. |
| 3 (alone) | overloaded sub-flag | In R8_CEQN, position 3 alone is a `TOPCODE` diameter-class flag (`voleqdef.f:1955,2060`); `calcdia.f`/`ht2topd.f` test `VOLEQ(1:1)='8'.AND.VOLEQ(3:3)='1'` as a distinct case that reroutes region-8 Clark VOLEQs to the region-9 Clark code path. |
| 4-7 | model mnemonic | Chars 4-6 are the primary dispatch key (`MDL=VOLEQ(4:6)` in `fia_volinit.for:194`, `calcdia.f`, `ht2topd.f`, `volinit.f`, `grossvol.f`); char 7 is frequently an independent variant flag within the same field, e.g. `KRU0`/`KRU1`/`KRU2`, `WEN0`/`WEN1`, `MAC0`/`MAC1`, `CLK0`/`CLK1`/`CLKE`/`CLKO`. |
| 8-10 | species code | 3-digit FIA species code, zero-padded. |

The `nsvb` family breaks this layout entirely: positions 1-3 are the literal prefix `NVB`, position 4
is `'0'` or `'M'` (ecoprovince > 999 flag), positions 5-7 are the 3-digit ecoprovince code, and
positions 8-10 are the species code (`nsvb.f:1371-1444`, `NVB_DefaultEq`). This is called out in the
`notes` column of every `nsvb` row.

## 3. Family routing rules, with file:line

| family | routing rule | file:line |
|---|---|---|
| flewelling_2pt / flewelling_3pt | `VOLEQ(4:4).EQ.'F'.OR.'f'` (umbrella test) | `profile.f:1356-1359` |
| flewelling_2pt (FW2 sub-form) | `MDL.EQ.'FW2'` | `calcdia.f:108`, `ht2topd.f:69` |
| flewelling_3pt (FW3/F32/F33 sub-forms) | `MDL.EQ.'FW3'/'F32'/'F33'` | `calcdia.f:108-110` |
| r2_taper (Max & Burkhart, 2-pt) | `VOLEQ(4:6).EQ.'CZ2'` | `profile.f:1362-1364` |
| r2_taper (Czaplewski, 3-pt) | `VOLEQ(4:6).EQ.'CZ3'` | `profile.f:1362-1364`, dedicated init at `calcdia.f:118-135` |
| r5_taper | `VOLEQ(4:6).EQ.'WO2'` | `profile.f:1369-1370` |
| r10_taper | `VOLEQ(4:6).EQ.'DEM'/'CUR'` (plus `'BRU'` grouped at the `volinit.f` level) | `profile.f:1372-1376`; `volinit.f:453-455` |
| r1_taper | `VOLEQ(4:6).EQ.'JB2'` | `profile.f:1378-1391` |
| behre_taper | `VOLEQ(4:6).EQ.'BEH'`, then the non-`'B'` branch inside `BEHTAP` | `profile.f:1395-1396`; `blmtap.f:284-310` |
| blm_taper | same `BEH` test, then `VOLEQ(1:1).EQ.'B'/'b'` branch inside `BEHTAP` | `blmtap.f:285` (`BLMTAPEQ`+`BLMTAP`) |
| r4_driver | `VOLEQ(4:6).EQ.'MAT'` | `profile.f:1398-1399`; equation itself in `r4vol.f:544` (`R4MATTAPER`) |
| r12_taper | `MDL.EQ.'SN2'` | `volinit.f:479,486` (calls `R12VOL`, which calls `R12TAP`) |
| clark_r9 | `VOLEQ(1:1).EQ.'9'` OR (`VOLEQ(1:1).EQ.'8'.AND.VOLEQ(3:3).EQ.'1'`), with `MDL.EQ.'CLK'` | `calcdia.f:158-160`; `ht2topd.f:121-246` |
| clark_r8 | `MDL.EQ.'CLK'`, `VOLEQ(1:1).EQ.'8'`, `VOLEQ(3:3).NE.'1'` | `calcdia.f:158-176` (`R8CLKDIB` call at `calcdia.f:176`, itself calling `R8PREPCOEF` at `r8clkdib.f:2`) |
| direct_volume (Clark, FIA South) | `REGN.EQ.'S'.AND.MDL.EQ.'CLK'` - unrelated to clark_r8/r9 despite the shared mnemonic | `fia_volinit.for:331` (`CLARK_VOL`) |
| direct_volume (DVE family) | `MDL.EQ.'DVE'`, then `VOLEQ(1:1)` selects the region-specific sub-driver | `dvest.f:28-140` |
| direct_volume (PNW tarif) | `MDL.EQ.'TRF'` | `volinit.f:298`, `volinit2.f:166`, `grossvol.f:74` |
| direct_volume (Army base) | `VOLEQ(1:1).EQ.'M'`, then `VOLEQ(1:3).EQ.'M01'`/`'M02'` | `dvest.f:117,122,124` |
| direct_volume (BIA Honer) | `VOLEQ(1:1).EQ.'C'/'c'` | `dvest.f:131` |
| direct_volume (FIA P/R/N/S national models) | `REGN.EQ.VOLEQ(1:1)` in `{P,R,N,S}`, then `MDL.EQ.VOLEQ(4:6)` against ~47 named mnemonics | `fia_volinit.for:190-343` (full per-mnemonic table reproduced as one pattern row per mnemonic in `identifiers.csv`) |
| nsvb | `VOLEQ(1:3).EQ.'NVB'` | `calcdia.f:194`; `ht2topd.f:261`; assignment at `nsvb.f:1371-1444` |

## 4. Could not determine

- **No single authoritative VOLEQ enumeration exists** (confirmed independently, matches
  NVEL_INVENTORY.md Sec 2.2). This CSV is a reconstruction, not a closed set - species-validity
  combinatorics in R8_BEQN/R8_CEQN (geo-area x top-code x species, `voleqdef.f:1691-2091`) generate
  many more valid VOLEQ strings at runtime than are ever quoted as literals, and were NOT
  individually enumerated here (doing so would require executing the combinatorial logic, not just
  reading quoted strings).
- **`clark_r8` pattern row has no literal example.** No quoted 10-character literal in either
  `voleqdef.f` or `fiaeq2nveleq.for` matched the "region 8, position 3 != '1'" Clark case; the row in
  the CSV exists purely because the dispatch rule itself is directly documented in `calcdia.f`. It is
  possible such VOLEQs are only ever constructed at runtime (via `R8PREPCOEF`'s geo-area/species
  parsing) and never appear as source literals.
- **The letter `I` is ambiguous across contexts.** In `fia_volinit.for`'s FIA-input alias table,
  `VOLEQ(1:1)='I'` means "Region 1" (`fia_volinit.for:137-138`). But among the quoted literals in
  `voleqdef.f`, position-1 `'I'` is the single most common non-digit character (97 occurrences) and,
  per NVEL_INVENTORY.md Sec 2.2, corresponds to Region 6's `EQNUMI` ("Interior" sub-variant) array,
  not to the FIA Region-1 alias. Which meaning applies to any given `'I'`-prefixed literal in the CSV
  was NOT independently re-derived for every row (the `position1_family_code` column reports the raw
  character; it does not attempt to resolve which of the two meanings applies). The same kind of
  overload applies to `'B'` (BLM vs. FIA Region-7 alias) and `'F'` (Region-6 alias, with the
  known copy-paste bug at `fia_volinit.for:141-142`) - both already flagged in NVEL_INVENTORY.md Sec
  2.1/12.6.
- **F32 vs. F33 vs. FW3 exact distinction.** All three are 3-point Flewelling variants dispatched
  identically at the `profile.f`/`calcdia.f` level; the precise measurement-configuration difference
  between them (beyond the `sf_3pt.f` vs. `sf_3z.f` file split noted in NVEL_INVENTORY.md Sec 3.1)
  was not re-derived from the numerical code in this pass - flagged as inherited from the inventory,
  not independently resolved here.
- **JSP=30** is inside the SHP_OT dispatch range (`sf_shp.f:45`, JSP 23-30) but has no corresponding
  `F()` data row in `f_other.f` (whose array covers only JRSP 1-7, i.e. JSP 23-29). Whether JSP=30 is
  simply unused, or maps to a row this pass missed, is NOT DETERMINED.
- **JSP 6-10** have no species assignment in any of `f_west.f`/`f_ingy.f`/`f_alaska.f`/`f_other.f` -
  simply a gap in the index range, not resolved further.
- **The 12-character `NVELBEQ` biomass-crosswalk table** in `fiaeq2nveleq.for` (around lines 900-970,
  e.g. `'SIN***B1P03D'`) is a different, 12-character format entirely, and was excluded from
  `identifiers.csv`. One 10-character placeholder token, `'XXXXXXXXXX'`, sits inside that same table
  (`fiaeq2nveleq.for:915`) and was excluded as a false positive (it is a sentinel/error value in an
  unrelated FIA-equation-code array, not a real VOLEQ). A second false positive,
  `'0123456789'` (`fiaeq2nveleq.for:936`), is a `VERIFY()` character-class argument inside a
  commented-out line, not a VOLEQ literal - also excluded. Because of these two exclusions,
  `identifiers.csv` derives 205 usable literals from `fiaeq2nveleq.for` rather than the raw-grep
  count of 207 cited in NVEL_INVENTORY.md Sec 2.2 (that count did not filter out the two false
  positives).
- **Exact citation for R12/Hawaii's SN2 mnemonic name origin** ("SN2" does not obviously abbreviate
  "Sharpneck") was not investigated further; only the dispatch rule and target routine were verified.
