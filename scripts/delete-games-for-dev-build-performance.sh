#!/bin/bash

# Only keep games from 1999, 2024, 2025
cd "$(dirname "$0")/../games"

for file in *.md; do
  if [[ ! "$file" =~ ^(1999|2024|2025)- ]]; then
    echo "Deleting $file"
    rm "$file"
  fi
done