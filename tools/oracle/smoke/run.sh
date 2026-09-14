#!/usr/bin/env bash
# Runs smoke/input.csv through both oracle drivers and writes COMPARE.md.
# Requires ../build.sh and `make -C ../driver` to have been run.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"
rm -f Debug.txt fort.1 fort.2 fort.3
../driver/check_debug ../build/libnvel_single.so ../build/libnvel_double.so
../driver/nvel_driver_single input.csv out_single.csv
../driver/nvel_driver_double input.csv out_double.csv
python3 compare.py out_single.csv out_double.csv COMPARE.md
echo "wrote $HERE/COMPARE.md"
