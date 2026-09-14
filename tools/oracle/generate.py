#!/usr/bin/env python3
"""Generate deterministic NVEL oracle fixtures with the single/double drivers."""

import csv
import os
import gzip
import hashlib
import io
from pathlib import Path
import random
import re
import subprocess
import sys
import time


fixture_root = os.environ.get("MERCHANDISER_FIXTURES",
                              os.environ.get("TREEVOLUME_FIXTURES", ""))
if not fixture_root:
    raise RuntimeError("Set MERCHANDISER_FIXTURES to the external fixture directory")
HERE = Path(fixture_root).resolve()
ORACLE = Path(__file__).resolve().parent
IDENTIFIERS = ORACLE / "identifiers" / "identifiers.csv"
DRIVER_DIR = ORACLE / "driver"
BUILD_DIR = ORACLE / "build"
WORK_DIR = HERE / ".work"
FULL_DIR = HERE / "full"
HOLDOUT_DIR = HERE / "holdout"

SEED = 20260905
DBHS = (2, 4, 6, 8, 10, 12, 16, 20, 24, 30, 40)
HEIGHTS = (15, 30, 45, 60, 80, 100, 130, 160, 200)
CALC_HEIGHTS = (1, 4.5, 8, 16, 17.3, 24, 32, 40, 50, 66, 80, 100)
TOP_DIAMETERS = (2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16)
CLARK_SI_BA = ((60, 80), (60, 150), (90, 80), (90, 150))
NVB_SPECIES = (202, 263, 122, 131, 110, 833, 316, 621, 802, 746)
NVB_DEAD_SPECIES = frozenset((202, 263))
BATCH_UNSAFE = {
    ("flewelling_2pt", "I15FW2W017"): (
        "CALCDIA stalls after repeated calls in a one-process family batch; "
        "fresh-process calls succeed, so hidden library state makes this identifier batch-unsafe"
    ),
}
BATCH_UNSAFE.update({
    ("r10_taper", voleq): (
        "CALCDIA/HT2TOPD times out in both precision builds on the required grid"
    )
    for voleq in (
        "A16CURW042", "A16CURW242", "A16DEMW042", "A16DEMW242",
        "A32CURW042", "A32CURW242", "A32DEMW042", "A32DEMW242",
        "A61CURW042", "A61CURW242", "A61DEMW042", "A61DEMW242",
    )
})
BATCH_UNSAFE_PREFIXES = {
    ("nsvb", "NVB0210"): (
        "single-precision VOLUMELIBRARY and NVBC segfault on the required grid; "
        "double precision and CALCDIA/HT2TOPD complete"
    ),
    ("nsvb", "NVB0230"): (
        "single-precision VOLUMELIBRARY and NVBC segfault on the required grid; "
        "double precision and CALCDIA/HT2TOPD complete"
    ),
}

# These are the five REGN/FORST/DIST combinations represented by smoke/input.csv.
# DIST is 01 throughout, but NVB_EcoProv resolves five ecological divisions from
# the region/forest context.
NVB_LOCATIONS = (
    (1, "16", "01"),
    (6, "12", "01"),
    (8, "01", "01"),
    (9, "09", "01"),
    (10, "04", "01"),
)

# Every Region 8 forest branch and each district exception in R8_CEQN.
# District 01 represents the default arm for forests with special districts.
R8_CLARK_LOCATIONS = (
    ("01", "01"), ("01", "03"),
    ("02", "01"),
    ("03", "01"), ("03", "08"),
    ("04", "01"),
    ("05", "01"),
    ("06", "01"),
    ("07", "01"), ("07", "06"), ("07", "07"), ("07", "17"),
    ("08", "01"),
    ("09", "01"),
    ("10", "01"), ("10", "07"),
    ("11", "01"), ("11", "03"), ("11", "10"),
    ("12", "01"), ("12", "02"), ("12", "05"),
    ("13", "01"),
    ("36", "01"),
    ("60", "01"),
)

# GETVARIANT's Region 9 forest groups. No Region 9 forest maps to the
# southern variant, so SN is exercised explicitly with forest 01.
R9_CLARK_VARIANT_FORESTS = (
    ("LS", ("02", "03", "04", "06", "07", "09", "10", "13")),
    ("CS", ("05", "08", "12")),
    ("NE", ("14", "19", "20", "21", "22")),
    ("SN", ("01",)),
)

INPUT_FIELDS = (
    "ROW_ID", "FIXTURE_SET", "FAMILY", "IDENTIFIER_SOURCE", "CALL_KIND",
    "call", "REGN", "FORST", "DIST", "SPEC", "FIASPCD", "PROD", "VOLEQ",
    "DBHOB", "HTTOT", "HTTYPE", "MTOPP", "MTOPS", "STUMP", "HT1PRD",
    "HT2PRD", "UPSHT1", "UPSHT2", "UPSD1", "UPSD2", "HTREF", "FCLASS",
    "DBTBH", "BTR", "CTYPE", "LIVE", "CONSPEC", "CR", "CULL", "DECAYCD", "SPFLG",
    "BRKHT", "BRKHTD", "BA", "SI", "HTUP", "STEMDIB", "CALCDIA_REQUESTED",
    "HT2TOPD_REQUESTED", "MRULEMOD", "NEWMAXLEN", "NEWMINLEN", "NEWTRIM",
    "NEWMTOPP",
)


def fmt(value):
    """Compact, stable formatting for inputs; driver controls output precision."""
    if isinstance(value, float):
        return format(value, ".9g")
    return str(value)


def ensure_binaries():
    single_lib = BUILD_DIR / "libnvel_single.so"
    double_lib = BUILD_DIR / "libnvel_double.so"
    if not single_lib.exists() or not double_lib.exists():
        subprocess.run([str(ORACLE / "build.sh")], cwd=ORACLE, check=True)
    single = DRIVER_DIR / "nvel_driver_single"
    double = DRIVER_DIR / "nvel_driver_double"
    checker = DRIVER_DIR / "check_debug"
    if not single.exists() or not double.exists() or not checker.exists():
        subprocess.run(["make", "-C", str(DRIVER_DIR)], check=True)
    subprocess.run(
        [str(checker), str(single_lib), str(double_lib)],
        cwd=WORK_DIR,
        check=True,
    )
    return single, double


def clean_outputs():
    for directory in (WORK_DIR, FULL_DIR, HOLDOUT_DIR):
        directory.mkdir(parents=True, exist_ok=True)
    for path in FULL_DIR.glob("*.csv.gz"):
        path.unlink()
    for path in HOLDOUT_DIR.glob("*.csv.gz"):
        path.unlink()
    for path in WORK_DIR.iterdir():
        if path.is_file():
            path.unlink()
    for path in (HERE / "SKIPPED.csv", HERE / "MANIFEST.csv"):
        if path.exists():
            path.unlink()


def run_driver(binary, input_path, output_path):
    for name in ("Debug.txt", "debug.txt", "DEBUG.TXT", "fort.1", "fort.2", "fort.3"):
        trace = WORK_DIR / name
        if trace.exists():
            trace.unlink()
    proc = subprocess.run(
        [str(binary), str(input_path), str(output_path)],
        cwd=WORK_DIR,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=1800,
    )
    return proc


def start_driver(binary, input_path, output_path):
    """Start one precision build for a family batch."""
    return subprocess.Popen(
        [str(binary), str(input_path), str(output_path)],
        cwd=WORK_DIR,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )


def read_inventory():
    with IDENTIFIERS.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    if len(rows) != 659:
        raise RuntimeError(f"expected 659 identifier rows, found {len(rows)}")
    return [
        row for row in rows
        if row["profile_based"].upper() == "TRUE" and row["family"] != "direct_volume"
    ]


def region_for(family, voleq):
    first = voleq[0]
    if first.isdigit():
        return int(first)
    if family == "r12_taper":
        return 12
    if family == "blm_taper":
        return 6
    if first in ("A", "a"):
        return 10
    # The I-prefixed inventory literals are Region 6 interior defaults, not the
    # FIA Region 1 alias. F is also the Region 6 alias in these rows.
    if first in ("F", "I", "i"):
        return 6
    if first in ("H", "h"):
        return 12
    return 0


def ordinary_context(family, voleq, source):
    return {
        "family": family,
        "voleq": voleq,
        "regn": region_for(family, voleq),
        "forst": voleq[1:3],
        "dist": "01",
        "spec": int(voleq[7:10]),
        "source": source,
    }


def concretize_inventory(rows, skipped):
    by_family = {}
    for row in rows:
        by_family.setdefault(row["family"], []).append(row)

    contexts = {}
    for family, family_rows in sorted(by_family.items()):
        if family == "nsvb" or family in ("clark_r8", "clark_r9"):
            continue
        donors = [
            row for row in family_rows
            if "?" not in row["voleq"] and "*" not in row["voleq"]
        ]
        resolved = {}
        for donor in donors:
            voleq = donor["voleq"]
            resolved[voleq] = ordinary_context(family, voleq, "inventory_literal")

        for template in family_rows:
            pattern = template["voleq"]
            if "?" not in pattern and "*" not in pattern:
                continue
            made = set()
            for donor in donors:
                donor_eq = donor["voleq"]
                candidate = "".join(
                    donor_char if pattern_char in "?*" else pattern_char
                    for pattern_char, donor_char in zip(pattern, donor_eq)
                )
                if "?" in candidate or "*" in candidate:
                    continue
                made.add(candidate)
                if candidate not in resolved:
                    source = f"template:{pattern};donor:{donor_eq}"
                    resolved[candidate] = ordinary_context(family, candidate, source)
            if not made:
                skipped.append({
                    "family": family,
                    "identifier": pattern,
                    "source": template["source"],
                    "reason": "no fully concrete same-family literal donor supplies forest/geo and species fields",
                })
        contexts[family] = [resolved[key] for key in sorted(resolved)]
    return contexts


def fortran_subroutine(source, name):
    match = re.search(
        rf"(?ims)^\s*SUBROUTINE\s+{re.escape(name)}\b(.*?)(?=^\s*SUBROUTINE\s+|\Z)",
        source,
    )
    if not match:
        raise RuntimeError(f"could not find {name} in voleqdef.f")
    return match.group(1)


def fortran_data_strings(body, name, count):
    match = re.search(
        rf"(?is)DATA\s*\(\s*{re.escape(name)}\(I\)\s*,\s*I\s*=\s*1\s*,\s*{count}\s*\)\s*/(.*?)/",
        body,
    )
    if not match:
        raise RuntimeError(f"could not find DATA {name}(1:{count}) in voleqdef.f")
    values = re.findall(r"['\"]([0-9]{3})['\"]", match.group(1))
    if len(values) != count:
        raise RuntimeError(f"expected {count} {name} values, found {len(values)}")
    return tuple(values)


def run_voleqdef_discovery(single, double, stem, rows):
    fields = (
        "call", "REGN", "FORST", "DIST", "VAR", "SPEC", "PROD", "VOLEQ",
        "DISCOVERY_MODE", "TABLE_VARIANT", "TABLE_SPECIES",
    )
    input_path = WORK_DIR / f"{stem}.input.csv"
    outputs = {
        "single": WORK_DIR / f"{stem}.single.csv",
        "double": WORK_DIR / f"{stem}.double.csv",
    }
    with input_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)

    ps = run_driver(single, input_path, outputs["single"])
    pd = run_driver(double, input_path, outputs["double"])
    if ps.returncode or pd.returncode:
        raise RuntimeError(
            f"{stem} VOLEQDEF discovery failed: "
            f"single={ps.returncode} {ps.stderr.strip()} "
            f"double={pd.returncode} {pd.stderr.strip()}"
        )

    with outputs["single"].open(newline="", encoding="utf-8") as hs, \
            outputs["double"].open(newline="", encoding="utf-8") as hd:
        single_rows = list(csv.DictReader(hs))
        double_rows = list(csv.DictReader(hd))
    if len(single_rows) != len(rows) or len(double_rows) != len(rows):
        raise RuntimeError(f"{stem} VOLEQDEF discovery returned an unexpected row count")
    for path in (input_path, outputs["single"], outputs["double"]):
        path.unlink()
    return tuple(zip(single_rows, double_rows))


def clark_family(voleq):
    if len(voleq) != 10 or voleq[3:6].upper() != "CLK":
        return None
    if voleq[0] == "9" or (voleq[0] == "8" and voleq[2] == "1"):
        return "clark_r9"
    if voleq[0] == "8":
        return "clark_r8"
    return None


def discover_clark(single, double, skipped):
    source_path = BUILD_DIR / "src" / "voleqdef.f"
    source = source_path.read_text(encoding="utf-8", errors="replace")
    r8_b_body = fortran_subroutine(source, "R8_BEQN")
    r8_body = fortran_subroutine(source, "R8_CEQN")
    r9_body = fortran_subroutine(source, "R9_EQN")
    r8_species = tuple(sorted(set(
        fortran_data_strings(r8_b_body, "SNSP", 92)
        + fortran_data_strings(r8_body, "SNSP", 110)
    )))
    top_match = re.search(r"(?is)DATA\s+TOPCODE\s*/(.*?)/", r8_body)
    if not top_match:
        raise RuntimeError("could not find R8_CEQN TOPCODE values in voleqdef.f")
    r8_top_codes = tuple(re.findall(r"['\"]([0-9])['\"]", top_match.group(1)))
    if r8_top_codes != ("1", "4", "7", "8", "9"):
        raise RuntimeError(f"unexpected R8_CEQN TOPCODE values: {r8_top_codes}")
    r9_species = {
        variant: tuple(sorted(set(fortran_data_strings(r9_body, f"{variant}SP", count))))
        for variant, count in (("LS", 69), ("CS", 97), ("NE", 108), ("SN", 92))
    }

    default_inputs = []
    for forst, dist in R8_CLARK_LOCATIONS:
        for species in r8_species:
            default_inputs.append({
                "call": "d", "REGN": 8, "FORST": forst, "DIST": dist,
                "VAR": "SN", "SPEC": int(species), "PROD": "01", "VOLEQ": "",
                "DISCOVERY_MODE": "default", "TABLE_VARIANT": "R8_CEQN",
                "TABLE_SPECIES": species,
            })
    for variant, forests in R9_CLARK_VARIANT_FORESTS:
        for forst in forests:
            for species in r9_species[variant]:
                default_inputs.append({
                    "call": "d", "REGN": 9, "FORST": forst, "DIST": "01",
                    "VAR": variant, "SPEC": int(species), "PROD": "01", "VOLEQ": "",
                    "DISCOVERY_MODE": "default", "TABLE_VARIANT": variant,
                    "TABLE_SPECIES": species,
                })

    records = {}

    def accept(srow, drow):
        if srow["ERRFLAG"] != drow["ERRFLAG"] or srow["VOLEQ_OUT"] != drow["VOLEQ_OUT"]:
            skipped.append({
                "family": "clark", "identifier": srow.get("VOLEQ_OUT", ""),
                "source": "VOLEQDEF_discovery",
                "reason": "single and double VOLEQDEF calls disagree",
            })
            return
        voleq = srow["VOLEQ_OUT"]
        family = clark_family(voleq)
        if srow["ERRFLAG"] != "0" or family is None:
            skipped.append({
                "family": "clark", "identifier": voleq,
                "source": (
                    f"VOLEQDEF:R{srow['REGN']}F{srow['FORST']}D{srow['DIST']}:"
                    f"SPEC{srow['SPEC']}:VAR{srow['VAR']}"
                ),
                "reason": f"VOLEQDEF returned ERRFLAG={srow['ERRFLAG']} or a non-Clark identifier",
            })
            return
        if voleq in records:
            return
        records[voleq] = {
            "family": family,
            "identifier": voleq,
            "REGN": srow["REGN"], "FORST": srow["FORST"], "DIST": srow["DIST"],
            "SPEC": srow["SPEC"], "VAR": srow["VAR"], "PROD": srow["PROD"],
            "VOLEQ_IN": srow["VOLEQ"], "discovery_mode": srow["DISCOVERY_MODE"],
            "table_variant": srow["TABLE_VARIANT"],
            "table_species": srow["TABLE_SPECIES"],
            "SINGLE_ERRFLAG": srow["ERRFLAG"], "DOUBLE_ERRFLAG": drow["ERRFLAG"],
            "SINGLE_VOLEQ_OUT": srow["VOLEQ_OUT"],
            "DOUBLE_VOLEQ_OUT": drow["VOLEQ_OUT"],
        }

    default_results = run_voleqdef_discovery(
        single, double, "clark_defaults", default_inputs
    )
    for srow, drow in default_results:
        accept(srow, drow)

    validation_inputs = []
    r8_defaults = [record for record in records.values() if record["REGN"] == "8"]
    for record in sorted(r8_defaults, key=lambda item: item["identifier"]):
        for top_code in r8_top_codes:
            if top_code == "1":
                continue
            candidate = record["identifier"][:2] + top_code + record["identifier"][3:]
            validation_inputs.append({
                "call": "d", "REGN": 8, "FORST": record["FORST"],
                "DIST": record["DIST"], "VAR": "SN", "SPEC": 9999,
                "PROD": "01", "VOLEQ": candidate,
                "DISCOVERY_MODE": "validation", "TABLE_VARIANT": "R8_CEQN",
                "TABLE_SPECIES": record["table_species"],
            })
    validation_results = run_voleqdef_discovery(
        single, double, "clark_validation", validation_inputs
    )
    for srow, drow in validation_results:
        accept(srow, drow)

    identifier_fields = (
        "family", "identifier", "REGN", "FORST", "DIST", "SPEC", "VAR", "PROD",
        "VOLEQ_IN", "discovery_mode", "table_variant", "table_species",
        "SINGLE_ERRFLAG", "DOUBLE_ERRFLAG", "SINGLE_VOLEQ_OUT", "DOUBLE_VOLEQ_OUT",
    )
    with (HERE / "clark_identifiers.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=identifier_fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(records[key] for key in sorted(records))

    contexts = {"clark_r8": [], "clark_r9": []}
    for voleq in sorted(records):
        record = records[voleq]
        family = record["family"]
        contexts[family].append({
            "family": family,
            "voleq": voleq,
            "regn": int(record["REGN"]),
            "forst": record["FORST"],
            "dist": record["DIST"],
            "spec": int(record["table_species"]),
            "source": (
                f"VOLEQDEF:{record['discovery_mode']}:R{record['REGN']}"
                f"F{record['FORST']}D{record['DIST']}:SPEC{record['SPEC']}:"
                f"VAR{record['VAR']}"
            ),
        })
    return contexts


def discover_nvb(single, double, skipped):
    input_path = WORK_DIR / "nvb_defaults.input.csv"
    single_out = WORK_DIR / "nvb_defaults.single.csv"
    double_out = WORK_DIR / "nvb_defaults.double.csv"
    with input_path.open("w", newline="", encoding="utf-8") as handle:
        fields = ("call", "REGN", "FORST", "DIST", "SPEC")
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for regn, forst, dist in NVB_LOCATIONS:
            for species in NVB_SPECIES:
                writer.writerow({
                    "call": "e", "REGN": regn, "FORST": forst,
                    "DIST": dist, "SPEC": species,
                })

    ps = run_driver(single, input_path, single_out)
    pd = run_driver(double, input_path, double_out)
    if ps.returncode or pd.returncode:
        raise RuntimeError(
            "NVB_DefaultEq discovery failed: "
            f"single={ps.returncode} {ps.stderr.strip()} "
            f"double={pd.returncode} {pd.stderr.strip()}"
        )

    with single_out.open(newline="", encoding="utf-8") as hs, \
            double_out.open(newline="", encoding="utf-8") as hd:
        sr = list(csv.DictReader(hs))
        dr = list(csv.DictReader(hd))
    if len(sr) != len(NVB_LOCATIONS) * len(NVB_SPECIES) or len(sr) != len(dr):
        raise RuntimeError("NVB_DefaultEq discovery returned an unexpected row count")

    contexts = {}
    for srow, drow in zip(sr, dr):
        location = f"R{srow['REGN']}F{srow['FORST']}D{srow['DIST']}"
        if srow["ERRFLAG"] != drow["ERRFLAG"] or srow["VOLEQ_OUT"] != drow["VOLEQ_OUT"]:
            skipped.append({
                "family": "nsvb",
                "identifier": srow.get("VOLEQ_OUT", ""),
                "source": f"NVB_DefaultEq:{location}:species{srow['SPEC']}",
                "reason": "single and double default-equation calls disagree",
            })
            continue
        if srow["ERRFLAG"] != "0" or not srow["VOLEQ_OUT"]:
            skipped.append({
                "family": "nsvb",
                "identifier": srow.get("VOLEQ_OUT", ""),
                "source": f"NVB_DefaultEq:{location}:species{srow['SPEC']}",
                "reason": f"NVB_DefaultEq returned ERRFLAG={srow['ERRFLAG']}",
            })
            continue
        voleq = srow["VOLEQ_OUT"]
        contexts.setdefault(voleq, {
            "family": "nsvb",
            "voleq": voleq,
            "regn": int(srow["REGN"]),
            "forst": srow["FORST"],
            "dist": srow["DIST"],
            "spec": int(srow["SPEC"]),
            "source": f"NVB_DefaultEq:{location}",
        })

    for path in (input_path, single_out, double_out):
        path.unlink()
    return contexts


def add_inventory_nvb(rows, contexts, skipped):
    nvb_rows = [row for row in rows if row["family"] == "nsvb"]
    donors = [
        row for row in nvb_rows
        if "?" not in row["voleq"] and "*" not in row["voleq"]
    ]
    prefix_context = {}
    for context in contexts.values():
        prefix_context.setdefault(context["voleq"][:7], context)

    for row in nvb_rows:
        pattern = row["voleq"]
        if "?" not in pattern and "*" not in pattern:
            candidates = (pattern,)
        else:
            candidates = tuple(sorted({
                "".join(
                    donor_char if pattern_char in "?*" else pattern_char
                    for pattern_char, donor_char in zip(pattern, donor["voleq"])
                )
                for donor in donors
            }))
        candidates = tuple(eq for eq in candidates if "?" not in eq and "*" not in eq)
        if not candidates:
            skipped.append({
                "family": "nsvb", "identifier": pattern, "source": row["source"],
                "reason": "no fully concrete same-family literal donor supplies division and species fields",
            })
            continue
        for voleq in candidates:
            if voleq in contexts:
                continue
            known = prefix_context.get(voleq[:7])
            contexts[voleq] = {
                "family": "nsvb",
                "voleq": voleq,
                "regn": known["regn"] if known else 0,
                "forst": known["forst"] if known else "00",
                "dist": known["dist"] if known else "00",
                "spec": int(voleq[7:10]),
                "source": "inventory_literal" if pattern == voleq else f"template:{pattern}",
            }
    return [contexts[key] for key in sorted(contexts)]


def point_variants(context, dbh, height):
    voleq = context["voleq"]
    if context["family"] == "flewelling_3pt" and len(voleq) >= 6 and voleq[5] == "3":
        return (
            ("upper17.3", 17.3, 0.85 * dbh),
            ("upper33", 33.0, 0.70 * dbh),
        )
    if context["family"] == "clark_r8" and voleq[2] in "479":
        # These identifiers take height to a fixed 4/7/9-inch top.
        fixed_top_height = 0.75 * height
        return ((f"fixed_top_{voleq[2]}", fixed_top_height, float(voleq[2])),)
    return (("none", 0.0, 0.0),)


def common_row(context, dbh, height, variant, upsht1, upsd1):
    return {
        "FAMILY": context["family"],
        "IDENTIFIER_SOURCE": context["source"],
        "REGN": context["regn"],
        "FORST": context["forst"],
        "DIST": context["dist"],
        "SPEC": context["spec"],
        "FIASPCD": context["spec"],
        "PROD": "01",
        "VOLEQ": context["voleq"],
        "DBHOB": dbh,
        "HTTOT": height,
        "HTTYPE": "F",
        "MTOPP": 6,
        "MTOPS": 4,
        "STUMP": 1,
        "HT1PRD": 0,
        "HT2PRD": 0,
        "UPSHT1": upsht1,
        "UPSHT2": 0,
        "UPSD1": upsd1,
        "UPSD2": 0,
        "HTREF": 0,
        "FCLASS": 0,
        "DBTBH": 0,
        "BTR": 0,
        "CTYPE": "C",
        "LIVE": "L",
        "CONSPEC": "",
        "CR": 0,
        "CULL": 0,
        "DECAYCD": 0,
        "SPFLG": "",
        "BRKHT": 0,
        "BRKHTD": 0,
        "BA": "",
        "SI": "",
        "HTUP": "",
        "STEMDIB": "",
        "CALCDIA_REQUESTED": "",
        "HT2TOPD_REQUESTED": "",
        "MRULEMOD": "N",
        "NEWMAXLEN": "",
        "NEWMINLEN": "",
        "NEWTRIM": "",
        "NEWMTOPP": "",
        "_variant": variant,
    }


def iter_family_rows(contexts):
    for context in contexts:
        for dbh in DBHS:
            for height in HEIGHTS:
                for variant, upsht1, upsd1 in point_variants(context, dbh, height):
                    base = common_row(context, dbh, height, variant, upsht1, upsd1)

                    si_ba_values = CLARK_SI_BA if context["family"].startswith("clark_") else (("", ""),)
                    for si, ba in si_ba_values:
                        volume = dict(base)
                        volume.update({
                            "CALL_KIND": "VOLUMELIBRARY", "call": "a", "SI": si, "BA": ba,
                            # R8CLARK has no >20 combined-log guard. Omitting
                            # secondary-product detail keeps the 200-foot grid
                            # inside its LOG* array bounds; primary volume and
                            # all profile probes remain covered.
                            "SPFLG": 0 if context["family"] == "clark_r8" else "",
                        })
                        yield volume

                    calc_heights = [value for value in CALC_HEIGHTS if value < height]
                    top_diameters = [value for value in TOP_DIAMETERS if value < dbh]
                    probe_count = max(len(calc_heights), len(top_diameters))
                    for index in range(probe_count):
                        calc_requested = index < len(calc_heights)
                        top_requested = index < len(top_diameters)
                        probe = dict(base)
                        probe.update({
                            "CALL_KIND": "PROFILE_PROBE",
                            "call": "b",
                            "HTUP": calc_heights[index] if calc_requested else calc_heights[0],
                            "STEMDIB": top_diameters[index] if top_requested else (
                                top_diameters[0] if top_diameters else dbh
                            ),
                            "CALCDIA_REQUESTED": "Y" if calc_requested else "N",
                            "HT2TOPD_REQUESTED": "Y" if top_requested else "N",
                        })
                        yield probe

                    if context["family"] == "nsvb":
                        live = dict(base)
                        live.update({"CALL_KIND": "NVBC_LIVE", "call": "c"})
                        yield live
                        if context["spec"] in NVB_DEAD_SPECIES:
                            dead = dict(base)
                            dead.update({
                                "CALL_KIND": "NVBC_DEAD", "call": "c", "LIVE": "D",
                                "CULL": 10, "DECAYCD": 3,
                            })
                            yield dead


def output_row(row, global_index, holdout, override):
    out = {field: "" for field in INPUT_FIELDS}
    for key, value in row.items():
        if not key.startswith("_"):
            out[key] = fmt(value)
    suffix = "M" if override else "B"
    out["ROW_ID"] = f"{global_index:07d}{suffix}"
    out["FIXTURE_SET"] = "holdout" if holdout else "full"
    if override:
        out.update({
            "MRULEMOD": "Y", "NEWMAXLEN": "32", "NEWMINLEN": "8",
            "NEWTRIM": "1", "NEWMTOPP": "6",
        })
        out["CALL_KIND"] += "_MRULE"
    return out


def write_family_input(family, contexts, start_index, holdout_indices, override_indices):
    input_path = WORK_DIR / f"{family}.input.csv"
    count = 0
    with input_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=INPUT_FIELDS, lineterminator="\n")
        writer.writeheader()
        for local_index, row in enumerate(iter_family_rows(contexts)):
            global_index = start_index + local_index
            is_holdout = global_index in holdout_indices
            writer.writerow(output_row(row, global_index, is_holdout, False))
            count += 1
            if global_index in override_indices:
                writer.writerow(output_row(row, global_index, is_holdout, True))
                count += 1
    return input_path, count


def open_gzip_writer(path, fields):
    raw = path.open("wb")
    gz = gzip.GzipFile(filename="", mode="wb", fileobj=raw, compresslevel=9, mtime=0)
    text = io.TextIOWrapper(gz, encoding="utf-8", newline="")
    writer = csv.DictWriter(text, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    return raw, gz, text, writer


def close_gzip_writer(parts):
    raw, gz, text, _writer = parts
    text.flush()
    text.detach()
    gz.close()
    raw.close()


def split_and_compress(raw_path, family, build):
    full_path = FULL_DIR / f"{family}.{build}.csv.gz"
    holdout_path = HOLDOUT_DIR / f"{family}.{build}.csv.gz"
    counts = {"full": 0, "holdout": 0}
    with raw_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        fields = list(reader.fieldnames or ()) + ["CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG"]
        full_parts = open_gzip_writer(full_path, fields)
        holdout_parts = open_gzip_writer(holdout_path, fields)
        writers = {"full": full_parts[3], "holdout": holdout_parts[3]}
        try:
            for row in reader:
                row["CALCDIA_ERRFLAG"] = ""
                row["HT2TOPD_ERRFLAG"] = ""
                if row["call"] == "b":
                    if row["CALCDIA_REQUESTED"] == "Y":
                        row["CALCDIA_ERRFLAG"] = row["ERRFLAG"]
                    if row["HT2TOPD_REQUESTED"] == "Y":
                        marker = row["VOLEQ_OUT"]
                        row["HT2TOPD_ERRFLAG"] = (
                            marker.split("=", 1)[1] if marker.startswith("H2T_ERR=") else "0"
                        )
                fixture_set = row["FIXTURE_SET"]
                if fixture_set not in writers:
                    raise RuntimeError(f"unknown fixture set {fixture_set!r}")
                writers[fixture_set].writerow(row)
                counts[fixture_set] += 1
        finally:
            close_gzip_writer(full_parts)
            close_gzip_writer(holdout_parts)
    return ((full_path, counts["full"]), (holdout_path, counts["holdout"]))


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            block = handle.read(1024 * 1024)
            if not block:
                break
            digest.update(block)
    return digest.hexdigest()


def write_skipped(skipped):
    fields = ("family", "identifier", "source", "reason")
    with (HERE / "SKIPPED.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(sorted(skipped, key=lambda row: (row["family"], row["identifier"], row["reason"])))


def write_manifest(entries):
    with (HERE / "MANIFEST.csv").open("w", newline="", encoding="utf-8") as handle:
        fields = ("file", "row_count", "sha256")
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for path, rows in sorted(entries, key=lambda item: str(item[0].relative_to(HERE))):
            writer.writerow({
                "file": str(path.relative_to(HERE)),
                "row_count": rows,
                "sha256": sha256(path),
            })


def main():
    started = time.monotonic()
    plan_only = "--plan" in sys.argv[1:]
    if plan_only:
        WORK_DIR.mkdir(parents=True, exist_ok=True)
    else:
        clean_outputs()
    single, double = ensure_binaries()
    inventory = read_inventory()
    skipped = []
    family_contexts = concretize_inventory(inventory, skipped)
    clark_contexts = discover_clark(single, double, skipped)
    family_contexts.update(clark_contexts)
    nvb_contexts = discover_nvb(single, double, skipped)
    family_contexts["nsvb"] = add_inventory_nvb(inventory, nvb_contexts, skipped)
    for family, contexts in list(family_contexts.items()):
        kept = []
        for context in contexts:
            voleq = context["voleq"]
            reason = BATCH_UNSAFE.get((family, voleq))
            if reason is None:
                for (prefix_family, prefix), prefix_reason in BATCH_UNSAFE_PREFIXES.items():
                    if family == prefix_family and voleq.startswith(prefix):
                        reason = prefix_reason
                        break
            if reason is None:
                kept.append(context)
            else:
                skipped.append({
                    "family": family,
                    "identifier": voleq,
                    "source": "observed_family_batch",
                    "reason": reason,
                })
        family_contexts[family] = kept
    family_contexts = {
        family: contexts for family, contexts in sorted(family_contexts.items()) if contexts
    }

    family_counts = {}
    total_base = 0
    for family, contexts in family_contexts.items():
        count = sum(1 for _ in iter_family_rows(contexts))
        family_counts[family] = count
        total_base += count

    rng = random.Random(SEED)
    holdout_indices = frozenset(rng.sample(range(total_base), round(total_base * 0.03)))
    override_indices = frozenset(rng.sample(range(total_base), round(total_base * 0.05)))

    print(f"base rows={total_base}; holdout={len(holdout_indices)}; merchant overrides={len(override_indices)}")
    for family, contexts in family_contexts.items():
        print(f"{family}: identifiers={len(contexts)} base_rows={family_counts[family]}")
    if plan_only:
        write_skipped(skipped)
        print(f"plan only; skipped rows written to {HERE / 'SKIPPED.csv'}")
        return 0

    entries = []
    start_index = 0
    for family, contexts in family_contexts.items():
        family_started = time.monotonic()
        input_path, expected = write_family_input(
            family, contexts, start_index, holdout_indices, override_indices
        )
        raw_outputs = {
            "single": WORK_DIR / f"{family}.single.csv",
            "double": WORK_DIR / f"{family}.double.csv",
        }
        failed = None
        processes = {
            "single": start_driver(single, input_path, raw_outputs["single"]),
            "double": start_driver(double, input_path, raw_outputs["double"]),
        }
        for build, proc in processes.items():
            try:
                stdout, stderr = proc.communicate(timeout=1800)
            except subprocess.TimeoutExpired:
                proc.kill()
                stdout, stderr = proc.communicate()
                failed = f"{build} driver exceeded 1800 seconds"
            if proc.returncode and failed is None:
                failed = (
                    f"{build} driver exited {proc.returncode}: "
                    f"{stderr.strip() or stdout.strip() or 'no diagnostic'}"
                )
        if failed:
            skipped.append({
                "family": family, "identifier": "*", "source": "family_batch",
                "reason": failed,
            })
            for proc in processes.values():
                if proc.poll() is None:
                    proc.kill()
                    proc.communicate()
            for path in raw_outputs.values():
                if path.exists():
                    path.unlink()
            input_path.unlink()
            start_index += family_counts[family]
            print(f"{family}: SKIPPED ({failed})")
            continue

        for build in ("single", "double"):
            split_entries = split_and_compress(raw_outputs[build], family, build)
            if sum(rows for _path, rows in split_entries) != expected:
                raise RuntimeError(f"{family}.{build}: output row count differs from input")
            entries.extend(split_entries)
            raw_outputs[build].unlink()
        input_path.unlink()
        start_index += family_counts[family]
        elapsed = time.monotonic() - family_started
        print(f"{family}: wrote {expected} rows/build in {elapsed:.1f}s")

    write_skipped(skipped)
    write_manifest(entries)
    total_size = sum(path.stat().st_size for path, _rows in entries)
    elapsed = time.monotonic() - started
    print(f"wrote {len(entries)} gzip files, {total_size / (1024 * 1024):.1f} MiB, in {elapsed:.1f}s")
    if total_size >= 1024 * 1024 * 1024:
        raise RuntimeError("compressed fixture total is not under 1 GiB")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
