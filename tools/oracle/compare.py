#!/usr/bin/env python3
"""Compare all single/double fixture pairs and write PRECISION.md."""

import csv
import os
import gzip
import itertools
import math
from pathlib import Path
import sys


fixture_root = os.environ.get("MERCHANDISER_FIXTURES",
                              os.environ.get("TREEVOLUME_FIXTURES", ""))
if not fixture_root:
    raise RuntimeError("Set MERCHANDISER_FIXTURES to the external fixture directory")
HERE = Path(fixture_root).resolve()
MANIFEST = HERE / "MANIFEST.csv"
DEFAULT_REPORT = HERE / "PRECISION.md"
STRING_OUTPUTS = frozenset(("VOLEQ_OUT", "MRULEMOD_AFTER"))


def load_pairs():
    with MANIFEST.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    paths = {row["file"] for row in rows}
    pairs = []
    for relative in sorted(paths):
        if not relative.endswith(".single.csv.gz"):
            continue
        double = relative.replace(".single.csv.gz", ".double.csv.gz")
        if double not in paths:
            raise RuntimeError(f"manifest has no double pair for {relative}")
        pairs.append((HERE / relative, HERE / double))
    if not pairs:
        raise RuntimeError("manifest contains no single/double fixture pairs")
    return pairs


def number(value):
    if value == "" or value is None:
        return None
    try:
        return float(value)
    except ValueError as exc:
        raise RuntimeError(f"expected numeric output, got {value!r}") from exc


def difference(single, double):
    if math.isnan(single) or math.isnan(double):
        return (0.0, 0.0) if math.isnan(single) and math.isnan(double) else (math.inf, math.inf)
    if single == double:
        return 0.0, 0.0
    absolute = abs(single - double)
    relative = absolute / abs(double) if double != 0 else math.inf
    return absolute, relative


def new_stat():
    return {
        "values": 0,
        "different": 0,
        "presence": 0,
        "max_abs": 0.0,
        "max_rel": 0.0,
        "abs_at": "",
        "rel_at": "",
    }


def shown(value):
    if math.isinf(value):
        return "inf"
    return f"{value:.9g}"


def is_discrete(column):
    return column in {
        "ERRFLAG", "NOLOGP", "NOLOGS", "VOL2", "VOL3",
        "CALCDIA_ERRFLAG", "HT2TOPD_ERRFLAG", "TLOGS",
    } or column.startswith("LOGLEN")


def compare_all(pairs):
    stats = {}
    output_order = []
    string_stats = {column: {"values": 0, "different": 0, "where": ""} for column in STRING_OUTPUTS}
    rejected = {}
    total_rows = 0

    for single_path, double_path in pairs:
        with gzip.open(single_path, "rt", newline="", encoding="utf-8") as hs, \
                gzip.open(double_path, "rt", newline="", encoding="utf-8") as hd:
            sr = csv.DictReader(hs)
            dr = csv.DictReader(hd)
            if sr.fieldnames != dr.fieldnames:
                raise RuntimeError(f"headers differ: {single_path} and {double_path}")
            fields = list(sr.fieldnames or ())
            if "BUILD" not in fields:
                raise RuntimeError(f"missing BUILD boundary in {single_path}")
            input_fields = fields[:fields.index("BUILD")]
            result_fields = fields[fields.index("BUILD") + 1:]
            if not output_order:
                output_order = result_fields
                for column in result_fields:
                    if column not in STRING_OUTPUTS:
                        stats[column] = new_stat()
            elif result_fields != output_order:
                raise RuntimeError(f"output schema differs in {single_path}")

            pair_name = str(single_path.relative_to(HERE)).replace(".single.csv.gz", "")
            for row_number, rows in enumerate(itertools.zip_longest(sr, dr), start=1):
                srow, drow = rows
                if srow is None or drow is None:
                    raise RuntimeError(f"row count differs in pair {pair_name}")
                if srow["ROW_ID"] != drow["ROW_ID"] or srow["VOLEQ"] != drow["VOLEQ"]:
                    raise RuntimeError(f"row alignment differs at {pair_name}:{row_number}")
                for column in input_fields:
                    if srow[column] != drow[column]:
                        raise RuntimeError(
                            f"echoed input {column} differs at {pair_name}:{row_number}"
                        )
                if srow["BUILD"] != "single" or drow["BUILD"] != "double":
                    raise RuntimeError(f"BUILD marker differs at {pair_name}:{row_number}")
                total_rows += 1
                location = f"{pair_name}:{row_number} {srow['VOLEQ']} {srow['CALL_KIND']}"

                identifier = (srow["FAMILY"], srow["VOLEQ"])
                reject = rejected.setdefault(identifier, {"calls": 0, "successful": 0})
                if srow["ERRFLAG"] != "" and drow["ERRFLAG"] != "":
                    reject["calls"] += 1
                    if srow["ERRFLAG"] == "0" or drow["ERRFLAG"] == "0":
                        reject["successful"] += 1

                for column in output_order:
                    if column in STRING_OUTPUTS:
                        string_stats[column]["values"] += 1
                        if srow[column] != drow[column]:
                            string_stats[column]["different"] += 1
                            if not string_stats[column]["where"]:
                                string_stats[column]["where"] = location
                        continue
                    sv = number(srow[column])
                    dv = number(drow[column])
                    stat = stats[column]
                    if sv is None and dv is None:
                        continue
                    if sv is None or dv is None:
                        stat["presence"] += 1
                        if not stat["abs_at"]:
                            stat["abs_at"] = location
                        continue
                    stat["values"] += 1
                    absolute, relative = difference(sv, dv)
                    if absolute != 0:
                        stat["different"] += 1
                    if absolute > stat["max_abs"]:
                        stat["max_abs"] = absolute
                        stat["abs_at"] = f"{location} ({srow[column]} vs {drow[column]})"
                    if relative > stat["max_rel"]:
                        stat["max_rel"] = relative
                        stat["rel_at"] = f"{location} ({srow[column]} vs {drow[column]})"

    fully_rejected = [
        (family, voleq, value["calls"])
        for (family, voleq), value in sorted(rejected.items())
        if value["calls"] and value["successful"] == 0
    ]
    return total_rows, output_order, stats, string_stats, fully_rejected


def write_report(path, pairs, total_rows, output_order, stats, string_stats, fully_rejected):
    maximum_abs = max((stat["max_abs"] for stat in stats.values()), default=0.0)
    maximum_rel = max((stat["max_rel"] for stat in stats.values()), default=0.0)
    max_abs_col = next((column for column in output_order if column in stats and stats[column]["max_abs"] == maximum_abs), "")
    max_rel_col = next((column for column in output_order if column in stats and stats[column]["max_rel"] == maximum_rel), "")

    lines = [
        "# Oracle fixture precision",
        "",
        f"Compared {total_rows:,} aligned rows in {len(pairs)} full/holdout family pairs. "
        "Single values use `%.9g`; double values use `%.17g`. Relative difference is "
        "`abs(single-double)/abs(double)`. A finite/non-finite mismatch is reported as infinite; "
        "matching NaNs are treated as equal.",
        "",
        f"Overall maximum absolute difference: **{shown(maximum_abs)}** in `{max_abs_col}`. "
        f"Overall maximum relative difference: **{shown(maximum_rel)}** in `{max_rel_col}`.",
        "",
        "## Every numeric output column",
        "",
        "| column | values | differing | presence mismatches | max absolute | max relative | maximum-relative row |",
        "|---|---:|---:|---:|---:|---:|---|",
    ]
    for column in output_order:
        if column not in stats:
            continue
        stat = stats[column]
        lines.append(
            f"| {column} | {stat['values']} | {stat['different']} | {stat['presence']} | "
            f"{shown(stat['max_abs'])} | {shown(stat['max_rel'])} | {stat['rel_at']} |"
        )

    lines.extend([
        "",
        "## Discrete outputs",
        "",
        "A mismatch means the numeric values differ exactly; no tolerance is applied.",
        "",
        "| column | rows compared | mismatching rows | presence mismatches |",
        "|---|---:|---:|---:|",
    ])
    for column in output_order:
        if column in stats and is_discrete(column):
            stat = stats[column]
            lines.append(
                f"| {column} | {stat['values']} | {stat['different']} | {stat['presence']} |"
            )

    lines.extend([
        "",
        "## String outputs",
        "",
        "`BUILD` is intentionally excluded because it names the precision build.",
        "",
        "| column | rows | mismatches | first mismatch |",
        "|---|---:|---:|---|",
    ])
    for column in sorted(string_stats):
        stat = string_stats[column]
        lines.append(f"| {column} | {stat['values']} | {stat['different']} | {stat['where']} |")

    lines.extend([
        "",
        "## Identifiers rejected by every call",
        "",
        "These identifiers had nonzero `ERRFLAG` in both builds on every generated call row.",
        "",
        "| family | VOLEQ | call rows |",
        "|---|---|---:|",
    ])
    if fully_rejected:
        for family, voleq, calls in fully_rejected:
            lines.append(f"| {family} | {voleq} | {calls} |")
    else:
        lines.append("| - | - | 0 |")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return maximum_abs, max_abs_col, maximum_rel, max_rel_col


def main():
    report = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else DEFAULT_REPORT
    pairs = load_pairs()
    results = compare_all(pairs)
    summary = write_report(report, pairs, *results)
    print(
        f"wrote {report}: max_abs={shown(summary[0])} ({summary[1]}), "
        f"max_rel={shown(summary[2])} ({summary[3]})"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
