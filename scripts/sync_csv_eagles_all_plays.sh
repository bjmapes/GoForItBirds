#!/usr/bin/env bash

set -euo pipefail

SRC="assets/data/pbp"
DST="_data/pbp"

echo "Syncing per-game PBP CSVs from $SRC -> $DST ..."
mkdir -p "$DST"

# Unzip each .csv.gz into matching season subfolders under _data/pbp
find "$SRC" -type f -name '*.csv.gz' -print0 | while IFS= read -r -d '' f; do
  rel="${f#$SRC/}"                          # e.g. 2024/2024_10_PHI_DAL.csv.gz
  season="${rel%%/*}"                       # e.g. 2024
  base="${rel##*/}"                         # e.g. 2024_10_PHI_DAL.csv.gz
  out="${base%.csv.gz}.csv"                 # e.g. 2024_10_PHI_DAL.csv
  mkdir -p "$DST/$season"
  gzip -cd "$f" > "$DST/$season/$out"
done

# Sync index JSON for Liquid
if [[ -f "assets/data/games_index.json" ]]; then
  mkdir -p "_data"
  cp -f "assets/data/games_index.json" "_data/games_index.json"
fi

echo "Done."