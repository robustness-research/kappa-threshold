#!/bin/bash

set -euo pipefail

# Resolve project paths from script location
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Support both repository layouts used in this project.
if [ -d "$PROJECT_ROOT/results/most_important_attr/by_dataset" ]; then
  BY_DATASET_DIR="$PROJECT_ROOT/results/most_important_attr/by_dataset"
  OUTPUT_DIR="$PROJECT_ROOT/results/most_important_attr"
elif [ -d "$PROJECT_ROOT/data/results/most_important_attr/by_dataset" ]; then
  BY_DATASET_DIR="$PROJECT_ROOT/data/results/most_important_attr/by_dataset"
  OUTPUT_DIR="$PROJECT_ROOT/data/results/most_important_attr"
else
  echo "Could not find by_dataset directory in either:"
  echo "  $PROJECT_ROOT/results/most_important_attr/by_dataset"
  echo "  $PROJECT_ROOT/data/results/most_important_attr/by_dataset"
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

combine_csv_group() {
  local pattern="$1"
  local output_file="$2"

  mapfile -t files < <(find "$BY_DATASET_DIR" -maxdepth 1 -type f -name "$pattern" | sort)

  if [ "${#files[@]}" -eq 0 ]; then
    echo "No files found for pattern: $pattern"
    return 1
  fi

  # Use a temp file so partial writes do not corrupt prior outputs.
  tmp_file="$(mktemp)"

  # Write header from first file.
  head -n 1 "${files[0]}" > "$tmp_file"

  # Append rows from all files (skip headers).
  for file in "${files[@]}"; do
    tail -n +2 "$file" >> "$tmp_file"
  done

  mv "$tmp_file" "$output_file"
  echo "Created: $output_file (${#files[@]} source files)"
}

echo "Combining MIA files from: $BY_DATASET_DIR"
combine_csv_group "*_mia.csv" "$OUTPUT_DIR/mia.csv"
combine_csv_group "*_miaUnique.csv" "$OUTPUT_DIR/miaUnique.csv"

echo "Done. Combined files are in: $OUTPUT_DIR"
