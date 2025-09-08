#!/usr/bin/env bash
set -euo pipefail

DEV_DIR="_data_dev"
SRC_DATA="_data"
SRC_PBP="$SRC_DATA/pbp"

rm -rf "$DEV_DIR"/*
mkdir -p "$DEV_DIR/pbp"

# find most recent season(folder) and game(csv)
latest_year=$(ls -1 "$SRC_PBP" | grep -E '^[0-9]{4}$' | sort -n | tail -1)
latest_csv=$(ls -1 "$SRC_PBP/$latest_year"/*.csv | sort -V | tail -1)

# extract year folder from relative path
rel="${latest_csv#"$SRC_PBP/"}"
year="${rel%%/*}"
mkdir -p "$DEV_DIR/pbp/$latest_year"
cp "$latest_csv" "$DEV_DIR/pbp/$latest_year/"

cp "$SRC_DATA/games_index.json" "$DEV_DIR/games_index.json"

echo "Dev data ready in $DEV_DIR"