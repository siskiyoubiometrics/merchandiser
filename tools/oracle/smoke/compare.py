#!/usr/bin/env python3
"""Compare the single and double oracle outputs and write COMPARE.md."""
import csv, math, sys
from collections import defaultdict

ERR_MEANING = {  # meanings taken from the assignments in the Fortran source (see REPORT.md)
    0: "no error",
    1: "equation not recognised or not valid for this call (volinit.f:362,537; nsvb.f:67,81,299)",
    2: "form class required but zero (profile.f:140, volinit.f:484)",
    3: "DBHOB below 1.0 (profile.f:119, volinit.f:169, nsvb.f:816)",
    4: "no usable height: HTTOT below 5 (4.5 for some models) and no product height (profile.f:121,235,256; volinit.f:340,812; nsvb.f:209,219,820)",
    6: "species or coefficient set not found (r8clkdib.f:88-266, nsvb.f:162,182)",
    7: "log height in tens of logs invalid (profile.f:167)",
    9: "required upper-stem or broken-top measurement missing (profile.f:240, nsvb.f:212,1189)",
    10: "UPSHT1 outside 4.5 to 0.95*HTTOT (profile.f:244)",
    12: "more than 20 logs (profile.f:380,388,757,768; r9logs.f; nsvb.f:380)",
    13: "merchantable top diameter at or above DBH inside bark (profile.f:1492)",
}

def load(p):
    with open(p, newline="") as f:
        r = list(csv.DictReader(f))
    return r

def fnum(s):
    try: return float(s)
    except (TypeError, ValueError): return None

def family(col):
    for fam in ("VOL", "DRYBIO", "GRNBIO", "LOGLEN", "BOLHT", "LOGDIA", "LOGVOL"):
        if col.startswith(fam) and col != "VOLEQ_OUT": return fam
    return col

def main(single, double, out):
    S, D = load(single), load(double)
    assert len(S) == len(D), "row count differs"
    result_cols = [c for c in S[0].keys() if c in ("ERRFLAG","VOLEQ_OUT","TLOGS","NOLOGP","NOLOGS","DIB","DOB","STEMHT") or family(c) != c]
    numeric = [c for c in result_cols if c not in ("VOLEQ_OUT",)]
    fam_stats = {}
    col_stats = {}
    string_mismatch = []
    for c in numeric:
        fam = family(c)
        for i, (s, d) in enumerate(zip(S, D)):
            sv, dv = fnum(s[c]), fnum(d[c])
            if sv is None and dv is None: continue
            if sv is None or dv is None:
                string_mismatch.append((i+1, c, s[c], d[c])); continue
            ad = abs(sv - dv)
            rd = ad / abs(dv) if dv != 0 else (0.0 if ad == 0 else math.inf)
            for key, store in ((fam, fam_stats), (c, col_stats)):
                st = store.setdefault(key, {"n": 0, "nz": 0, "maxabs": 0.0, "maxabs_at": "", "maxrel": 0.0, "maxrel_at": ""})
                st["n"] += 1
                if ad > 0: st["nz"] += 1
                if ad > st["maxabs"]: st["maxabs"], st["maxabs_at"] = ad, f"row {i+1} {c} ({sv!r} vs {dv!r})"
                if rd > st["maxrel"]: st["maxrel"], st["maxrel_at"] = rd, f"row {i+1} {c} ({sv!r} vs {dv!r})"
    for i, (s, d) in enumerate(zip(S, D)):
        if s["VOLEQ_OUT"] != d["VOLEQ_OUT"]:
            string_mismatch.append((i+1, "VOLEQ_OUT", s["VOLEQ_OUT"], d["VOLEQ_OUT"]))

    L = []
    L.append("# Single vs double oracle comparison\n")
    L.append(f"Inputs: `{single}` and `{double}` ({len(S)} rows each, same `input.csv`).\n")
    L.append("Single values were written with `%.9g`, double with `%.17g`. Relative difference is |single - double| / |double|.\n")
    L.append("## Per output family (max over all rows and elements)\n")
    L.append("| family | values compared | values differing | max abs diff | where | max rel diff | where |")
    L.append("|---|---|---|---|---|---|---|")
    order = ["ERRFLAG","TLOGS","NOLOGP","NOLOGS","VOL","DIB","DOB","STEMHT","DRYBIO","GRNBIO","LOGLEN","BOLHT","LOGDIA","LOGVOL"]
    for k in order:
        if k not in fam_stats: continue
        st = fam_stats[k]
        L.append(f"| {k} | {st['n']} | {st['nz']} | {st['maxabs']:.6g} | {st['maxabs_at']} | {st['maxrel']:.3e} | {st['maxrel_at']} |")
    L.append("\n## VOL(1..15), DIB, DOB, STEMHT, NOLOGP, NOLOGS per column\n")
    L.append("| column | values | differing | max abs diff | max rel diff | where (rel) |")
    L.append("|---|---|---|---|---|---|")
    for c in [f"VOL{i}" for i in range(1,16)] + ["DIB","DOB","STEMHT","NOLOGP","NOLOGS"]:
        if c in col_stats:
            st = col_stats[c]
            L.append(f"| {c} | {st['n']} | {st['nz']} | {st['maxabs']:.6g} | {st['maxrel']:.3e} | {st['maxrel_at']} |")
    L.append("\n## Rows with nonzero ERRFLAG\n")
    L.append("| row | call | VOLEQ | ERRFLAG single | ERRFLAG double | meaning (from source) | note |")
    L.append("|---|---|---|---|---|---|---|")
    any_err = False
    for i, (s, d) in enumerate(zip(S, D)):
        es, ed = s["ERRFLAG"], d["ERRFLAG"]
        h2t = s["VOLEQ_OUT"] if s["VOLEQ_OUT"].startswith("H2T_ERR") else ""
        if (es not in ("", "0")) or (ed not in ("", "0")) or h2t:
            any_err = True
            code = int(es or ed or 0)
            extra = f" HT2TOPD: {h2t}" if h2t else ""
            L.append(f"| {i+1} | {s['call']} | {s['VOLEQ']} | {es} | {ed} | {ERR_MEANING.get(code, 'not tabulated')}{extra} | {s.get('note','')} |")
    if not any_err: L.append("| - | - | - | - | - | none | - |")
    L.append("\n## String outputs and presence mismatches\n")
    if string_mismatch:
        for row, c, a, b in string_mismatch: L.append(f"- row {row} `{c}`: single `{a}` vs double `{b}`")
    else:
        L.append("None: VOLEQ_OUT and every present/absent pattern agree between the two builds.")
    L.append("\n## Per-row ERRFLAG and VOL(1..5) side by side\n")
    L.append("| row | call | VOLEQ | ERR s/d | VOL1 s | VOL1 d | VOL2 s | VOL2 d | VOL4 s | VOL4 d |")
    L.append("|---|---|---|---|---|---|---|---|---|---|")
    for i, (s, d) in enumerate(zip(S, D)):
        if s["call"] in ("a", "c"):
            L.append(f"| {i+1} | {s['call']} | {s['VOLEQ']} | {s['ERRFLAG']}/{d['ERRFLAG']} | {s['VOL1']} | {d['VOL1']} | {s['VOL2']} | {d['VOL2']} | {s['VOL4']} | {d['VOL4']} |")
    with open(out, "w") as f: f.write("\n".join(L) + "\n")

if __name__ == "__main__":
    main(*sys.argv[1:4])
