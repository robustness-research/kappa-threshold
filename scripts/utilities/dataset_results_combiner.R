#!/usr/bin/env Rscript

# Load required libraries
library(dplyr)
library(readr)

# Read parameters to get list of models and datasets
params <- read_csv("data/files/parameters.csv", show_col_types = FALSE)
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
datasets <- strsplit(params$values[params$parameter == "dataset_name"], "\\|")[[1]]

# Directory containing individual results
results_dir <- "results/instances/original/by_dataset"

# Expected number of models
expected_models <- length(models)
cat("Expected number of models:", expected_models, "\n")
cat("Models:", paste(models, collapse=", "), "\n\n")

# Process each dataset
for (dataset in datasets) {
  cat("Processing dataset:", dataset, "\n")
  
  # Find all result files for this dataset
  pattern <- paste0("^", dataset, "_.*_results\\.csv$")
  all_files <- list.files(results_dir, pattern = pattern, full.names = TRUE)
  
  # Count how many model files exist for this dataset
  n_models <- length(all_files)
  cat("  Found", n_models, "model files\n")
  
  # Only process if all models are complete
  if (n_models == expected_models) {
    cat("  All models complete! Combining files...\n")
    
    # Read and combine all model files
    combined_data <- NULL
    for (file in all_files) {
      data <- read_csv(file, show_col_types = FALSE)
      if (is.null(combined_data)) {
        combined_data <- data
      } else {
        combined_data <- bind_rows(combined_data, data)
      }
    }
    
    # Write combined results
    output_file <- file.path(results_dir, paste0(dataset, "_results.csv"))
    write_csv(combined_data, output_file)
    cat("  Written combined file:", output_file, "\n")
    
    # Delete individual model files
    cat("  Deleting individual model files...\n")
    for (file in all_files) {
      file.remove(file)
      cat("    Deleted:", basename(file), "\n")
    }
    
    cat("  Dataset", dataset, "completed successfully!\n\n")
  } else {
    cat("  Skipping - only", n_models, "out of", expected_models, "models complete\n\n")
  }
}

cat("Script completed!\n")
