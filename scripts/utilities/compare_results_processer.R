#!/usr/bin/env Rscript

library(tidyverse)

# Get parameters from parameters.csv
parameters <- read_csv("data/files/parameters.csv", show_col_types = FALSE)

# Dataset list
datasets <- parameters %>%
  filter(parameter == "dataset_name") %>%
  pull(values) %>%
  str_split("\\|") %>%
  unlist()

# Model list
models <- parameters %>%
  filter(parameter == "technique_name") %>%
  pull(values) %>%
  str_split("\\|") %>%
  unlist()

# Noise list
noise_levels <- c(0.1, 0.2, 0.3)

# Function to read and process results for a dataset type (original or train_noise)
process_results <- function(dataset_type) {
  cat(sprintf("\n=== Processing %s results ===\n", dataset_type))
  
  results_dir <- file.path("results", "instances", dataset_type, "by_dataset")
  
  if (!dir.exists(results_dir)) {
    cat(sprintf("Directory not found: %s\n", results_dir))
    return(NULL)
  }
  
  # Get finished results
  all_files <- list.files(results_dir, pattern = "_results\\.csv$", full.names = TRUE)
  
  # Filter out model_results files
  result_files <- all_files[!grepl("_model_results\\.csv$", all_files)]
  
  cat(sprintf("Found %d result files\n", length(result_files)))
  
  if (length(result_files) == 0) {
    return(NULL)
  }
  
  # Read and combine all result files
  all_data <- map_df(result_files, function(file) {
    cat(sprintf("Reading: %s\n", basename(file)))
    tryCatch({
      read_csv(file, show_col_types = FALSE) %>%
        mutate(
          dataset_type = dataset_type,
          source_file = basename(file)
        )
    }, error = function(e) {
      cat(sprintf("Error reading %s: %s\n", basename(file), e$message))
      return(NULL)
    })
  })
  
  return(all_data)
}

# Read original and train_noise results
original_data <- process_results("original")
train_noise_data <- process_results("train_noise")

# Combine both datasets
all_data <- bind_rows(original_data, train_noise_data)

if (is.null(all_data) || nrow(all_data) == 0) {
  cat("No data found. Exiting.\n")
  quit(status = 1)
}

cat(sprintf("\nTotal rows read: %d\n", nrow(all_data)))

# Filter for specified noise levels only
cat("\nFiltering for noise levels: 0.1, 0.2, 0.3...\n")
filtered_data <- all_data %>%
  filter(noise_levels %in% noise_levels)

# Calculate average accuracy and kappa across folds
averaged_results <- filtered_data %>%
  group_by(dataset, method, noise_levels, instance_level, dataset_type) %>%
  summarise(
    avg_accuracy = mean(accuracy, na.rm = TRUE),
    avg_kappa = mean(kappa, na.rm = TRUE),
    n_folds = n(),
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    sd_kappa = sd(kappa, na.rm = TRUE),
    .groups = "drop"
  )


