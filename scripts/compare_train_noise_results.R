#!/usr/bin/env Rscript

# Load required packages
pacman::p_load(dplyr, tidyr, data.table)

# Set working directory to project root
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  match <- grep("^--file=", cmdArgs)
  if (length(match) > 0) {
    return(normalizePath(sub("^--file=", "", cmdArgs[match])))
  } else {
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}
script.dir <- dirname(thisFile())
project.root <- normalizePath(file.path(script.dir, ".."))
setwd(project.root)
cat("Current working directory:", getwd(), "\n")

# Load parameters to get dataset and model names
params <- read.csv("data/files/parameters.csv", stringsAsFactors = FALSE)
datasets <- unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|"))
models <- unlist(strsplit(subset(params, parameter == "technique_name")$values, "\\|"))

# Noise levels to compare (only matching levels)
noise_levels_to_compare <- c(0.1, 0.2, 0.3)

# Create output directory for per-dataset comparisons
dir.create("results/instances/comparisons_by_dataset", showWarnings = FALSE, recursive = TRUE)

# Process each dataset separately
cat("\n=== Processing Datasets ===\n")

for (dataset in datasets) {
  cat("\nProcessing dataset:", dataset, "\n")
  
  # Try different file naming patterns for original results
  original_file1 <- paste0("results/instances/original/by_dataset/", dataset, "_results.csv")
  train_noise_file1 <- paste0("results/instances/train_noise/by_dataset/", dataset, "_results.csv")
  
  original_data_list <- list()
  train_noise_data_list <- list()
  
  # Load original results for this dataset
  if (file.exists(original_file1)) {
    cat("  Loading original from:", original_file1, "\n")
    df <- read.csv(original_file1, stringsAsFactors = FALSE)
    df <- df %>% filter(noise_levels %in% noise_levels_to_compare)
    original_data_list[[1]] <- df
  } else {
    # Try model-specific files
    for (model in models) {
      original_file2 <- paste0("results/instances/original/by_dataset/", dataset, "_", model, "_results.csv")
      if (file.exists(original_file2)) {
        df <- read.csv(original_file2, stringsAsFactors = FALSE)
        df <- df %>% filter(noise_levels %in% noise_levels_to_compare)
        original_data_list[[length(original_data_list) + 1]] <- df
      }
    }
  }
  
  # Load train_noise results for this dataset
  if (file.exists(train_noise_file1)) {
    cat("  Loading train_noise from:", train_noise_file1, "\n")
    df <- read.csv(train_noise_file1, stringsAsFactors = FALSE)
    df <- df %>% filter(noise_levels %in% noise_levels_to_compare)
    train_noise_data_list[[1]] <- df
  } else {
    # Try model-specific files
    for (model in models) {
      train_noise_file2 <- paste0("results/instances/train_noise/by_dataset/", dataset, "_", model, "_results.csv")
      if (file.exists(train_noise_file2)) {
        df <- read.csv(train_noise_file2, stringsAsFactors = FALSE)
        df <- df %>% filter(noise_levels %in% noise_levels_to_compare)
        train_noise_data_list[[length(train_noise_data_list) + 1]] <- df
      }
    }
  }
  
  # Skip if no data found for either experiment
  if (length(original_data_list) == 0 || length(train_noise_data_list) == 0) {
    cat("  Skipping dataset (missing original or train_noise data)\n")
    next
  }

  
  # Combine data for this dataset
  original_df <- rbindlist(original_data_list, fill = TRUE)
  train_noise_df <- rbindlist(train_noise_data_list, fill = TRUE)
  
  # Step 1: Compute average kappa and accuracy across folds for each model-noise-instance combination
  cat("  Computing averages across folds\n")
  
  original_avg <- original_df %>%
    group_by(dataset, method, noise_levels, instance_level) %>%
    summarise(
      mean_accuracy_original = mean(accuracy, na.rm = TRUE),
      mean_kappa_original = mean(kappa, na.rm = TRUE),
      sd_accuracy_original = sd(accuracy, na.rm = TRUE),
      sd_kappa_original = sd(kappa, na.rm = TRUE),
      n_folds_original = n(),
      .groups = "drop"
    )
  
  train_noise_avg <- train_noise_df %>%
    group_by(dataset, method, noise_levels, instance_level) %>%
    summarise(
      mean_accuracy_train_noise = mean(accuracy, na.rm = TRUE),
      mean_kappa_train_noise = mean(kappa, na.rm = TRUE),
      sd_accuracy_train_noise = sd(accuracy, na.rm = TRUE),
      sd_kappa_train_noise = sd(kappa, na.rm = TRUE),
      n_folds_train_noise = n(),
      .groups = "drop"
    )
  
  # Step 2: Join to compare same dataset-model-noise-instance combinations
  cat("  Joining and comparing results\n")
  
  comparison <- original_avg %>%
    inner_join(train_noise_avg, 
              by = c("dataset", "method", "noise_levels", "instance_level"))
  
  # Calculate 1-kappa values and differences
  comparison <- comparison %>%
    mutate(
      one_minus_kappa_original = 1 - mean_kappa_original,
      one_minus_kappa_train_noise = 1 - mean_kappa_train_noise,
      diff_accuracy = mean_accuracy_train_noise - mean_accuracy_original,
      diff_kappa = mean_kappa_train_noise - mean_kappa_original,
      diff_one_minus_kappa = one_minus_kappa_train_noise - one_minus_kappa_original
    ) %>%
    arrange(method, noise_levels, instance_level)
  
  # Step 3: Save separate file for this dataset
  out_file <- paste0("results/instances/comparisons_by_dataset/", dataset, "_comparison.csv")
  write.csv(comparison, file = out_file, row.names = FALSE)
  cat("  Saved comparison to:", out_file, "\n")
  cat("  Number of comparisons:", nrow(comparison), "\n")
}

cat("\n=== Comparison Complete ===\n")
cat("Results saved to: results/instances/comparisons_by_dataset/\n")
