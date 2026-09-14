#!/usr/bin/env bash
# build.sh - build the NVEL Fortran test oracle as two shared libraries.
#
#   build/libnvel_single.so   native REAL*4 build
#   build/libnvel_double.so   -fdefault-real-8 -fdefault-double-8 build
#
# Idempotent: re-running reuses the checked-out source and rebuilds objects.
# Source: https://github.com/FMSC-Measurements/VolumeLibrary at the pinned
# commit below; falls back to the read-only local clone if the network fails.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD="$HERE/build"
SRC="$BUILD/src"
COMMIT="38548071d5aa652bb90c7f111f86b427f798a1c9"
UPSTREAM="https://github.com/FMSC-Measurements/VolumeLibrary"
LOCAL_CLONE="${NVEL_SOURCE:-}"
FC="${FC:-gfortran}"

# Compile flags. -std=legacy/-ffixed-form/-w are the flags known to compile every
# canonical file. -ffp-contract=off keeps the two builds free of FMA contraction so
# results depend only on the declared precision, not on instruction selection.
COMMON_FLAGS=(-std=legacy -ffixed-form -w -fPIC -O2 -fno-fast-math -ffp-contract=off)
SINGLE_FLAGS=("${COMMON_FLAGS[@]}")
# -freal-4-real-8 is REQUIRED in addition to the -fdefault-* pair: the Flewelling
# sources (sf_2pth.f:64 and others) declare some entities REAL*4 explicitly, which
# -fdefault-real-8 does not promote, and gfortran then rejects the build with
# "Return type mismatch of function sf_2pth1 (REAL(4)/REAL(8))". With all three
# flags every REAL, REAL*4, and unsuffixed real constant is 8 bytes and DOUBLE
# PRECISION stays 8 (verified with a kind-probe program, see REPORT.md).
DOUBLE_FLAGS=("${COMMON_FLAGS[@]}" -fdefault-real-8 -fdefault-double-8 -freal-4-real-8)

mkdir -p "$BUILD"
INFO="$BUILD/BUILD_INFO.txt"
: > "$INFO"
log() { printf '%s\n' "$*" | tee -a "$INFO"; }

# ---------------------------------------------------------------- 1. source
SOURCE_ORIGIN=""
if [ -d "$SRC/.git" ] && [ "$(git -C "$SRC" rev-parse HEAD 2>/dev/null)" = "$COMMIT" ]; then
  SOURCE_ORIGIN="reused existing checkout in build/src (HEAD = $COMMIT)"
else
  rm -rf "$SRC"
  if git clone --quiet "$UPSTREAM" "$SRC" 2>"$BUILD/clone.err" \
     && git -C "$SRC" checkout --quiet "$COMMIT" 2>>"$BUILD/clone.err"; then
    SOURCE_ORIGIN="git clone $UPSTREAM, checked out $COMMIT"
  else
    echo "network clone failed ($(tr '\n' ' ' < "$BUILD/clone.err")); copying local read-only clone" >&2
    rm -rf "$SRC"
    cp -a "$LOCAL_CLONE" "$SRC"
    LOCAL_HEAD="$(git -C "$SRC" rev-parse HEAD 2>/dev/null || echo unknown)"
    if [ "$LOCAL_HEAD" != "$COMMIT" ]; then
      git -C "$SRC" checkout --quiet "$COMMIT" || { echo "local clone lacks $COMMIT" >&2; exit 1; }
    fi
    SOURCE_ORIGIN="NETWORK FAILED - copied local clone $LOCAL_CLONE (HEAD $LOCAL_HEAD), checked out $COMMIT"
  fi
fi
ACTUAL_HEAD="$(git -C "$SRC" rev-parse HEAD)"
[ "$ACTUAL_HEAD" = "$COMMIT" ] || { echo "HEAD $ACTUAL_HEAD != pinned $COMMIT" >&2; exit 1; }

# ------------------------------------------------- 2. canonical file list
# Derived from vollib/vollib.vfproj: every <File RelativePath=...> that is not
# marked ExcludedFromBuild for the Release|x64 configuration and is a Fortran
# source (.f/.F/.for/.F90). .inc files are INCLUDEd, not compiled.
VFPROJ="$SRC/vollib/vollib.vfproj"
[ -f "$VFPROJ" ] || { echo "missing $VFPROJ" >&2; exit 1; }

awk '
  /<File RelativePath=/ {
    match($0, /RelativePath="[^"]*"/); p = substr($0, RSTART+14, RLENGTH-15);
    if ($0 ~ /RelativePath="[^"]*"[ \t]*\/>/) { print "INC\t" p; cur=""; next }
    cur = p; excl = 0; next
  }
  cur != "" && /ExcludedFromBuild="true"/ && /Release\|x64/ { excl = 1 }
  cur != "" && /<\/File>/ { print (excl ? "EXC" : "INC") "\t" cur; cur="" }
' "$VFPROJ" | tr -d '\r' > "$BUILD/vfproj_entries.tsv"

NREL="$(grep -o 'RelativePath="[^"]*"' "$VFPROJ" | wc -l)"
NCLS="$(wc -l < "$BUILD/vfproj_entries.tsv")"
[ "$NREL" = "$NCLS" ] || { echo "vfproj parse mismatch: $NREL RelativePath entries, $NCLS classified" >&2; exit 1; }

CANON="$BUILD/canonical_files.txt"
EXCLUDED="$BUILD/excluded_files.txt"
: > "$CANON"; : > "$EXCLUDED"
while IFS=$'\t' read -r status rel; do
  # ..\X -> repo root, .\X -> vollib/
  path="$(printf '%s' "$rel" | sed 's#\\#/#g; s#^\./#vollib/#; s#^\.\./##')"
  base="$(basename "$path")"; dir="$SRC/$(dirname "$path")"
  case "${base,,}" in *.f|*.for|*.f90) ;; *) continue ;; esac
  if [ "$status" = "EXC" ]; then echo "$rel" >> "$EXCLUDED"; continue; fi
  real="$(find "$dir" -maxdepth 1 -iname "$base" | head -1)"
  [ -n "$real" ] || { echo "vfproj entry not found on disk: $rel" >&2; exit 1; }
  echo "${real#$SRC/}" >> "$CANON"
done < "$BUILD/vfproj_entries.tsv"

# Files that must never be compiled (duplicate symbols / retired copies).
for bad in volumelibrary_20210727test.f voleqdef.f_20140325 wdbkwtdata_20190807.inc files_not_in_vollib; do
  if grep -q "$bad" "$CANON"; then echo "forbidden file in canonical list: $bad" >&2; exit 1; fi
done

# Module sources compile first (a file is a module source if it opens a MODULE).
MODS="$BUILD/module_files.txt"; NONMODS="$BUILD/nonmodule_files.txt"
: > "$MODS"; : > "$NONMODS"
while read -r f; do
  if grep -qiE '^[[:space:]]*MODULE[[:space:]]+[A-Za-z_]' "$SRC/$f"; then echo "$f" >> "$MODS"; else echo "$f" >> "$NONMODS"; fi
done < "$CANON"

# ---------------------------------------------------------------- 3. build
build_variant() {
  local name="$1"; shift
  local flags=("$@")
  local obj="$BUILD/obj_$name"
  rm -rf "$obj"; mkdir -p "$obj"
  local n=0
  for f in $(cat "$MODS") $(cat "$NONMODS"); do
    local o="$obj/$(basename "${f%.*}").o"
    "$FC" "${flags[@]}" -J "$obj" -I "$SRC" -c "$SRC/$f" -o "$o"
    n=$((n+1))
  done
  local nobj; nobj="$(ls "$obj"/*.o | wc -l)"
  local ncanon; ncanon="$(wc -l < "$CANON")"
  [ "$nobj" = "$ncanon" ] || { echo "$name: $nobj objects but $ncanon canonical files" >&2; exit 1; }
  "$FC" -shared -fPIC -o "$BUILD/libnvel_$name.so" "$obj"/*.o
  # Full-link check: no unresolved symbols once libgfortran/libc are considered.
  local undef
  undef="$(ldd -r "$BUILD/libnvel_$name.so" 2>&1 | grep -i 'undefined symbol' || true)"
  log ""
  log "=== libnvel_$name.so"
  log "flags: ${flags[*]}"
  log "objects compiled and linked: $nobj of $ncanon canonical files"
  if [ -n "$undef" ]; then log "UNRESOLVED SYMBOLS (link NOT clean):"; log "$undef"; exit 1; else log "ldd -r: no undefined symbols (full link confirmed)"; fi
  log "exported text symbols: $(nm -D --defined-only "$BUILD/libnvel_$name.so" | awk '$2=="T"' | wc -l)"
  log "key entry points:"
  local missing=0
  for s in volumelibrary_ vollibcs_ calcdia_ ht2topd_ nvbc_ getvoleq_ voleqdef_ vernum_ nvb_defaulteq_ getvoleq_r_ vernum_r_; do
    if nm -D --defined-only "$BUILD/libnvel_$name.so" | awk '{print $3}' | grep -qx "$s"; then log "  $s  present"; else log "  $s  MISSING"; missing=1; fi
  done
  log "module variables (D/B symbols):"
  nm -D --defined-only "$BUILD/libnvel_$name.so" | awk '$2=="D"||$2=="B"' | awk '{print "  " $3}' | grep -i '_MOD_' | tee -a "$INFO" >/dev/null
  [ "$missing" = 0 ] || { echo "$name: key entry point missing" >&2; exit 1; }
}

log "NVEL oracle build - $(date -u +%Y-%m-%dT%H:%M:%SZ)"
log "source: $SOURCE_ORIGIN"
log "pinned commit: $COMMIT (verified HEAD)"
log "compiler: $("$FC" --version | head -1)"
log "canonical Fortran sources (from vollib/vollib.vfproj, Release|x64): $(wc -l < "$CANON")"
log "module sources compiled first: $(tr '\n' ' ' < "$MODS")"
log "vfproj entries excluded from build: $(tr '\n' ' ' < "$EXCLUDED")"

build_variant single "${SINGLE_FLAGS[@]}"
build_variant double "${DOUBLE_FLAGS[@]}"

log ""
log "canonical file list: build/canonical_files.txt"
echo "done: $BUILD/libnvel_single.so $BUILD/libnvel_double.so"
