#!/usr/bin/env Rscript

# Script to convert .arff files to .csv files
# Converts all .arff files in data/datasets/ to .csv format

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    foreign    # for file path handling
)


# Get the directory paths
datasets_dir <- "data/datasets/arff/"

# Get all .arff files in the datasets directory
arff_files <- list.files(
  path = datasets_dir,
  pattern = "\\.arff$",
  full.names = TRUE
)

# Check if any .arff files were found
if (length(arff_files) == 0) {
  cat("No .arff files found in", datasets_dir, "\n")
  quit(status = 1)
}

cat("Found", length(arff_files), ".arff files to convert\n\n")

# Convert each .arff file to .csv
for (arff_file in arff_files) {
  # Generate the output .csv filename
  csv_file <- sub("\\.arff$", ".csv", arff_file)
  
  cat("Converting:", basename(arff_file), "->", basename(csv_file), "... ")
  
  tryCatch({
    # Read the .arff file
    data <- read.arff(arff_file)
    
    # Write to .csv file
    write.csv(data, csv_file, row.names = FALSE)
    
    cat("✓ Done\n")
  }, error = function(e) {
    cat("✗ Failed\n")
    cat("  Error:", conditionMessage(e), "\n")
  })
}

cat("\nConversion complete!\n")
