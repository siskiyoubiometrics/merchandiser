# FVS / NVEL submodule and patch investigation

Repository investigated: `https://github.com/USDAForestService/ForestVegetationSimulator`
(branch `main`). Investigated via `gh api` only - no clone was made, shallow or otherwise.

## Submodule path and URL

`.gitmodules` (fetched via
`gh api repos/USDAForestService/ForestVegetationSimulator/contents/.gitmodules`, blob sha
`2c72729d9d0a28f8beb013d488723db64891e50e`) contains exactly one submodule entry:

```
[submodule "volume/NVEL"]
	path = volume/NVEL
	url = https://github.com/FMSC-Measurements/VolumeLibrary
```

So the submodule path is `volume/NVEL`, and NVEL is pulled from a separate GitHub repo,
`https://github.com/FMSC-Measurements/VolumeLibrary` (not a `USDAForestService`-owned repo).

## Pinned NVEL commit

`gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/NVEL` returns a
`"type":"submodule"` entry with `"submodule_git_url":"https://github.com/FMSC-Measurements/VolumeLibrary"`
and pinned commit SHA:

```
4fa39a007973e8fb6a64b3f0c77b03000145fad3
```

`gh api repos/FMSC-Measurements/VolumeLibrary/commits/4fa39a007973e8fb6a64b3f0c77b03000145fad3`
resolves that commit: author date `2026-04-15T16:02:15Z`, commit message:

```
vollib release 20260415

Fixed the divided by zero problem in nsvb.f. Also modified the volinit.f for small tree
(no merch volume) biomass with non-NSVB equation recalculation.
```

No tag matches this SHA: `gh api repos/FMSC-Measurements/VolumeLibrary/tags` returned no tag
whose `commit.sha` equals the pinned SHA, so there is no named release/tag for this pin -
it is pinned to a bare commit on (presumably) the default branch.

## Patch file: found, but not a git patch/diff

There is a file named `volume/NVEL_Patches.txt` sitting in the FVS repo alongside the `volume/NVEL`
submodule directory (sibling, not inside the submodule). Its full content
(`gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/NVEL_Patches.txt`,
base64-decoded):

```
fixed an issue in R9Clark, when  (1.0-17.3/totht)**p, resulted in an underflow condition.  The fix was as follows:

      IF((1.0-17.3/totht).LT.0.005748.AND.p.GT.14)THEN
        Y = 0
      ELSE
        Y=(1.0-17.3/totht)**p
      ENDIF

I was testing some additional databases obtained from FSVeg DA for region 8, SN variant, and hit a few more instances where the computation was used as caused underflow.

R9clark.f: Subroutine R9dib, within IF statement 'IF(Ib.EQ.1.0)
R9clark.f: Subroutine R9ht, early in the subroutine that sets the combined variables
```

This is **not a `git apply`-able patch or unified diff** - it is a plain-text changelog note,
in prose, describing a manual bug fix applied to the Region 9 Clark taper routine (`R9Clark.f`,
called `r9clark.f` in the NVEL source at the audited local path). It names three sites: the general
`(1.0-17.3/totht)**p` underflow, an `IF(Ib.EQ.1.0)` block inside subroutine `R9dib`, and an early
combined-variable assignment inside subroutine `R9ht`. No file diff, hunk markers, or line numbers
are given - only a description of the guard condition to add before the exponentiation. No
mechanism in this repo (build script, workflow, or otherwise) was found that applies this text file
programmatically; it reads as a developer's note recording what was hand-fixed in the override
source described below, not an artifact consumed by any build step.

### How FVS actually diverges from the submodule (full-file overrides, not a diff)

The `volume/` directory (sibling to `volume/NVEL`) also contains full standalone Fortran source
files that duplicate NVEL routine names:

```
volume/NVEL_Patches.txt
volume/fvsbrucedemars.f
volume/fvshannbare.f
volume/fvsoldfst.f
volume/fvsoldgro.f
volume/fvsoldsec.f
volume/fvssierralog.f
volume/r9clark_fvsMod.f
volume/voleqdef.f
```

(`gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume` --jq '.[].name')

`volume/r9clark_fvsMod.f` and `volume/voleqdef.f` are FVS's own modified copies of routines that
also exist inside the `volume/NVEL` submodule (`r9clark.f` and `voleqdef.f` respectively, per the
local NVEL inventory). Their header comment blocks are revision logs, not diffs:

- `volume/r9clark_fvsMod.f` header lists dated revisions from multiple FVS developers (TDH, RNH,
  MGV, YW) from 2009 through at least 2016, including "revised RNH 11/19/2009 Added numerical error
  trap in equation for stmDib, subroutine r9dib" - consistent with, but predating, the underflow fix
  described in `NVEL_Patches.txt` (that note is undated but appears to describe a related/further
  fix to the same subroutine).
- `volume/voleqdef.f` header lists dated revisions specific to FVS's default-equation assignment,
  e.g. "03/25/2014 changed default equation for Region 3 (R3_EQN) Ponderosa pine ... to
  300FW2W122", "07/19/2021 Changed R8_CEQN to use the R8 new Clark equation 8*1CLKE***," and the
  comment "When VOLEQDEF is used by FVS, the call to VOLEQDEF always carries the variant 2 character
  symbol so the GETVARIANT routine is not used by FVS" - i.e., FVS-specific default-equation
  business logic layered on top of (and diverging from) the generic NVEL `VOLEQDEF`.

**Conclusion**: FVS does not apply a git-style patch/diff to the NVEL submodule at build or checkout
time. Instead it (a) pins the submodule to a specific upstream commit, and (b) maintains hand-edited
full-file forks of at least two NVEL routines (`voleqdef.f`, and the Region 9 Clark taper file
under the name `r9clark_fvsMod.f`) directly in its own `volume/` directory, which presumably shadow
or replace the submodule's versions at build time. `NVEL_Patches.txt` is a prose changelog of one
such hand-edit (an underflow guard in the Region 9 Clark taper code), not the patch mechanism
itself.

**NOT DETERMINED**: the exact build step that causes `volume/voleqdef.f` and
`volume/r9clark_fvsMod.f` to be compiled instead of (or in addition to) the submodule's
`volume/NVEL/voleqdef.f` and `volume/NVEL/r9clark.f`. No `Makefile`, `CMakeLists.txt`,
`SConstruct`, or `build.py` exists at the FVS repository root (all four returned HTTP 404 via
`gh api repos/USDAForestService/ForestVegetationSimulator/contents/<name>`), and
`gh api repos/USDAForestService/ForestVegetationSimulator/contents/.github/workflows` also returned
404 (no workflows directory found at that path, or the repo's CI config lives somewhere this
investigation did not check). The actual build configuration must live elsewhere in the tree (e.g.
under a subdirectory not enumerated here); locating it would require further calls not made here to
stay within the stated economical API budget.

**NOT DETERMINED**: whether the underflow fix described in `NVEL_Patches.txt` was ever contributed
back upstream to `FMSC-Measurements/VolumeLibrary` (i.e., whether the pinned commit
`4fa39a007973e8fb6a64b3f0c77b03000145fad3` already includes it, or whether it exists only in FVS's
`r9clark_fvsMod.f` fork). Confirming this would require diffing `r9clark_fvsMod.f` against
`FMSC-Measurements/VolumeLibrary`'s `r9clark.f` at the pinned commit, which was not done here.

## Commands and URLs used, in order

1. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/.gitmodules`
2. (decode) `base64 -d` on the returned `.content` field of call 1
3. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/NVEL`
4. `gh api repos/FMSC-Measurements/VolumeLibrary/tags --jq '.[] | select(.commit.sha=="4fa39a007973e8fb6a64b3f0c77b03000145fad3") | .name'`
5. `gh api repos/FMSC-Measurements/VolumeLibrary/commits/4fa39a007973e8fb6a64b3f0c77b03000145fad3 --jq '{sha:.sha, date:.commit.author.date, message:.commit.message}'`
6. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/ --jq '.[].name'`
7. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/.github/workflows --jq '.[].name'` (404 - path not found)
8. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/ci --jq '.[].name'`
9. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume --jq '.[].name'`
10. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/NVEL_Patches.txt --jq '.content'` then `base64 -d`
11. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/r9clark_fvsMod.f --jq '.content'` then `base64 -d | head -40`
12. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/volume/voleqdef.f --jq '.content'` then `base64 -d | head -40`
13. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/ --jq '.[] | select(.type=="file") | .name'`
14. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/Makefile` (404)
15. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/CMakeLists.txt` (404)
16. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/SConstruct` (404)
17. `gh api repos/USDAForestService/ForestVegetationSimulator/contents/build.py` (404)

No repository clone (full or shallow) was performed; all findings above came from the GitHub REST
API via `gh api`.
