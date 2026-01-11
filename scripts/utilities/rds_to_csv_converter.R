#!/usr/bin/env Rscript

# Script to convert .rds files to .csv files
# Converts all .rds files in data/datasets/ to .csv format

# Get the directory paths
datasets_dir <- "data/datasets"

# Get all .rds files in the datasets directory
rds_files <- list.files(
  path = datasets_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

# Check if any .rds files were found
if (length(rds_files) == 0) {
  cat("No .rds files found in", datasets_dir, "\n")
  quit(status = 1)
}

cat("Found", length(rds_files), ".rds files to convert\n\n")

# Convert each .rds file to .csv
for (rds_file in rds_files) {
  # Generate the output .csv filename
  csv_file <- sub("\\.rds$", ".csv", rds_file)
  
  cat("Converting:", basename(rds_file), "->", basename(csv_file), "... ")
  
  tryCatch({
    # Read the .rds file
    data <- readRDS(rds_file)
    
    # Write to .csv file
    write.csv(data, csv_file, row.names = FALSE)
    
    cat("✓ Done\n")
  }, error = function(e) {
    cat("✗ Failed\n")
    cat("  Error:", conditionMessage(e), "\n")
  })
}

cat("\nConversion complete!\n")
