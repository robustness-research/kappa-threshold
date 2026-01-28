#!/usr/bin/env Rscript

# Libraries
library(tidyverse)

# Parameters
params <- read_csv("data/parameters.csv", show_col_types = FALSE)
datasets <- strsplit(params$values[params$parameter == "dataset_name"], "\\|")[[1]]
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
instance_levels <- strsplit(params$values[params$parameter == "instance_level"], "\\|")[[1]]

# Noise list
noise_levels <- c(0.1, 0.2, 0.3)

# Function to read and process results for a dataset type
process_results <- function(dataset, dataset_type) {
  cat(sprintf("\n=== Processing %s results ===\n", dataset))
  
  results_dir <- file.path("data", "results", "instances", dataset_type, "by_dataset")
  
  if (!dir.exists(results_dir)) {
    cat(sprintf("Directory not found: %s\n", results_dir))
    return(NULL)
  }
  
  # Get cross-fold results
  pattern <- paste0("^", dataset, "_results_avg\\.csv$")
  result_file <- list.files(results_dir, pattern = pattern, full.names = TRUE)

  # Skip if file doesn't exist
  if (length(result_file) == 0) {
    cat("  Skipping - results file not found\n")
    return(invisible(NULL))
  }

  # Get the file
  data <- read_csv(result_file, show_col_types = FALSE)

  # Filter for specified noise levels only
  cat("\nFiltering for noise levels: 0.1, 0.2, 0.3...\n")
  filtered_data <- all_data %>%
    filter(noise_levels %in% noise_levels)

}

# Read original and train_noise results
test_noise <- process_results("original")
train_test_noise <- process_results("train_noise")

# Combine both datasets
all_data <- bind_rows(test_noise, train_test_noise)

if (is.null(all_data) || nrow(all_data) == 0) {
  cat("No data found. Exiting.\n")
  quit(status = 1)
}

# Get KLC plots
cat("\nGenerating KLC plots...\n")

ggplot2 