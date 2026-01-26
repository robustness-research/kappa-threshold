#!/usr/bin/env Rscript

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,       # for data manipulation
  purrr,       # for functional programming
  here         # for file path handling
)

# Define noise levels
noise_levels <- c(0.1, 0.2, 0.3)

# Define thresholds for Kappa Loss
kl_thresholds <- c(0.05, 0.10, 0.15)

# Load parameters from CSV
parameters <- read.csv(here("data", "files", "parameters.csv"), stringsAsFactors = FALSE)

# Extract dataset names and techniques from parameters
dataset_names <- strsplit(parameters$values[parameters$parameter == "dataset_name"], "\\|")[[1]]
techniques <- strsplit(parameters$values[parameters$parameter == "technique_name"], "\\|")[[1]]

cat("Found", length(dataset_names), "datasets from parameters.csv\n")
cat("Found", length(techniques), "techniques from parameters.csv\n")

# Load CSV files from results/instances/original/by_dataset
csv_dir <- here("results", "instances", "original", "by_dataset")

# Build file paths for each dataset
csv_files <- file.path(csv_dir, paste0(dataset_names, "_results.csv"))

# Filter to only existing files
csv_files <- csv_files[file.exists(csv_files)]

cat("Found", length(csv_files), "dataset CSV files to process\n\n")

# Read and combine all CSV files
all_data <- purrr::map_df(csv_files, function(file) {
  cat("Reading:", basename(file), "\n")
  read.csv(file, stringsAsFactors = FALSE)
})

# Add baseline rows: 0% instance manipulation with kappa = 1
baseline_rows <- expand.grid(
  dataset = dataset_names,
  method = techniques,
  noise_levels = noise_levels,
  instance_level = 0,
  stringsAsFactors = FALSE
) %>%
  mutate(
    fold = NA,
    accuracy = 1,
    kappa = 1
  )

# Combine baseline with actual data
all_data <- bind_rows(baseline_rows, all_data)

# Calculate kappa loss for each combination
# Group by dataset, technique (method), noise_levels, and instance_level
# Calculate average kappa across folds, then compute kappa loss
klc_data <- all_data %>%
  group_by(dataset, method, noise_levels, instance_level) %>%
  summarise(
    kappa = mean(kappa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate kappa_loss relative to baseline (0% instance manipulation, kappa = 1)
  group_by(dataset, method, noise_levels) %>%
  mutate(
    baseline_kappa = 1,  # Hardcoded: 0% instance manipulation has kappa = 1
    kappa_loss = baseline_kappa - kappa
  ) %>%
  ungroup() %>%
  rename(
    dataset_name = dataset,
    technique = method,
    noise = noise_levels,
    percentage = instance_level
  ) %>%
  # Convert to percentage scale for readability
  mutate(
    noise = noise * 100,
    percentage = percentage * 100
  )

# Function to find the last instance percentage where kappa_loss is under the threshold
find_threshold <- function(data, threshold) {
  if (nrow(data) == 0) {
    return(list(critical_percentage = NA, kappa_loss = NA))
  }
  
  # Order data by percentage of instances
  data <- data[order(data$percentage), ]
  
  # Find all points where kappa_loss is under or equal to the threshold (vectorized)
  under_threshold <- data$kappa_loss <= threshold
  
  # If any points are under threshold, return the last one
  if (any(under_threshold)) {
    last_idx <- max(which(under_threshold))
    return(list(
      critical_percentage = data$percentage[last_idx],
      kappa_loss = data$kappa_loss[last_idx]
    ))
  } else {
    # If no points are under threshold, return max percentage
    return(list(
      critical_percentage = max(data$percentage),
      kappa_loss = data$kappa_loss[nrow(data)]
    ))
  }
}

# Create grid for parameters
threshold_grid <- expand.grid(
  dataset = dataset_names,
  tech = techniques,
  noise_lvl = noise_levels * 100,  # Convert to percentage scale
  thresh_lvl = kl_thresholds,
  stringsAsFactors = FALSE
)

cat("\nProcessing", nrow(threshold_grid), "combinations...\n\n")

# Apply function (tidyverse)
results_list <- purrr::pmap(threshold_grid, function(dataset, tech, noise_lvl, thresh_lvl) {
  # Filter data
  current_data <- klc_data %>%
    filter(dataset_name == dataset, technique == tech, noise == noise_lvl)
  
  if (nrow(current_data) > 0) {
    # Apply find_threshold function
    result <- find_threshold(current_data, thresh_lvl)
    
    # Return results as a data frame row
    return(data.frame(
      dataset_name = dataset,
      technique = tech,
      noise_level = noise_lvl,
      threshold = thresh_lvl,
      critical_percentage = result$critical_percentage,
      kappa_at_critical = result$kappa_loss,
      stringsAsFactors = FALSE
    ))
  } else {
    # Return NA entries if no data is available
    return(data.frame(
      dataset_name = dataset,
      technique = tech,
      noise_level = noise_lvl,
      threshold = thresh_lvl,
      critical_percentage = NA,
      kappa_at_critical = NA,
      stringsAsFactors = FALSE
    ))
  }
})
# Combine all results into one data frame
threshold_results <- do.call(rbind, results_list)

# Save the results
results_path <- here("results", "threshold_instance_results.csv")
write.csv(threshold_results, file = results_path, row.names = FALSE)

cat("\n================================\n")
cat("Processing complete!\n")
cat("Results saved to:", results_path, "\n")
cat("================================\n")
