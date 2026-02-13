#!/usr/bin/env Rscript

# Libraries
library(tidyverse)

# Parameters
params <- read_csv("data/parameters.csv", show_col_types = FALSE)
datasets <- strsplit(params$values[params$parameter == "dataset_name"], "\\|")[[1]]
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
models <- models[models != "rda"] # Exclude 'rda' if present
thresholds <- as.numeric(strsplit(params$values[params$parameter == "threshold_level"], "\\|")[[1]])

# Results directories
dir1 <- "data/results/instances/original/by_dataset"
dir2 <- "data/results/instances/train_noise/by_dataset"

# Process each dataset
process_dataset <- function(dataset, directory){
  cat("Processing dataset:", dataset, "\n")
  
  # Find result files for this dataset
  pattern <- paste0("^", dataset, "_results\\.csv$")
  result_file <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  # Skip if file doesn't exist
  if (length(result_file) == 0) {
    cat("  Skipping - results file not found\n")
    return(invisible(NULL))
  }

  data <- read_csv(result_file, show_col_types = FALSE)

  averaged_results <- data %>%
    group_by(dataset, method, noise_levels, instance_level) %>%
    summarise(
      avg_accuracy = round(mean(accuracy, na.rm = TRUE), 3),
      avg_kappa = round(mean(kappa, na.rm = TRUE), 3),
      avg_kappa_loss = round(1 - mean(kappa, na.rm = TRUE), 3),
      sd_accuracy = round(sd(accuracy, na.rm = TRUE), 3),
      sd_kappa = round(sd(kappa, na.rm = TRUE), 3),
      .groups = "drop"
    )

  # Write combined results
  output_file <- file.path(directory, paste0(dataset, "_results_avg.csv"))
  write_csv(averaged_results, output_file)
  cat("  Written new file:", output_file, "\n")
}

# Process each dataset and threshold for dir2
process_dataset_threshold <- function(dataset, threshold, directory){
  cat("Processing threshold:", threshold, "\n")
  cat("Processing dataset:", dataset, "\n")

  threshold_dir <- paste0(directory, "/threshold_", threshold)
  
  # Find result files for this dataset
  pattern <- paste0("^", dataset, "_results\\.csv$")
  result_file <- list.files(threshold_dir, pattern = pattern, full.names = TRUE)
  
  # Skip if file doesn't exist
  if (length(result_file) == 0) {
    cat("  Skipping - results file not found\n")
    return(invisible(NULL))
  }

  data <- read_csv(result_file, show_col_types = FALSE)

  averaged_results <- data %>%
    group_by(dataset, method, noise_levels, instance_level) %>%
    summarise(
      avg_accuracy = round(mean(accuracy, na.rm = TRUE), 3),
      avg_kappa = round(mean(kappa, na.rm = TRUE), 3),
      avg_kappa_loss = round(1 - mean(kappa, na.rm = TRUE), 3),
      sd_accuracy = round(sd(accuracy, na.rm = TRUE), 3),
      sd_kappa = round(sd(kappa, na.rm = TRUE), 3),
      .groups = "drop"
    )

  # Write combined results
  output_file <- file.path(threshold_dir, paste0(dataset, "_results_avg.csv"))
  write_csv(averaged_results, output_file)
  cat("  Written new file:", output_file, "\n")
}

lapply(datasets, process_dataset, directory = dir1)
lapply(datasets, function(dataset) {
  lapply(thresholds, function(threshold) {
    process_dataset_threshold(dataset, threshold, directory = dir2)
  })
})

cat("Completed!\n")
