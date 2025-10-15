#!/usr/bin/env Rscript

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # for data manipulation and visualization
  dplyr,       # for data manipulation
  tidyr,       # for data reshaping
  ggplot2,     # for visualization
  here         # for file path handling
)

# Load dataset names, techniques, and instances levels
dataset_names <- readRDS(here("data", "files", "dataset_name.rds"))
techniques <- readRDS(here("data", "files", "technique_name.rds"))
instance_levels <- readRDS(here("data", "files", "instance_level.rds"))

# Define noise levels
noise_levels <- c(10, 20, 30)

# Define thresholds for Kappa Loss
kl_thresholds <- c(0.05, 0.10, 0.15)

# Load the KLC curves
klc_data <- readRDS(here("results", "KLC_plot_deciles.rds"))
klc_data <- klc_data %>%
  filter(noise %in% c(10, 20, 30)) %>%
  select(-c(accuracy, kappa, dataset_order, method_order))

# Function to find the last instance percentage where kappa_loss is under the threshold
find_threshold <- function(data, threshold) {
  
  # Order data by percentage of instances
  data <- data[order(data$percentage), ]

  # Find the last instance where kappa_loss is under or equal to the threshold
  last_under_threshold <- NULL
  for (i in seq_len(nrow(data))) {
    if (data$kappa_loss[i] <= threshold) {
      last_under_threshold <- i
    }
  }
  
  # Return the last point under threshold if found
  if (!is.null(last_under_threshold)) {
    i <- last_under_threshold
    cat("Last kappa loss under threshold:", data$kappa_loss[i], ", at instance:", data$percentage[i], "\n")
    return(list(
      critical_percentage = data$percentage[i],
      kappa_loss = data$kappa_loss[i]
    ))
  }
  
  # If no point exceeds threshold, return 100%
  if(nrow(data) > 0) {
    cat("Kappa loss at critical: 0, Critical instance: 100%", "\n")
    return(list(
      critical_percentage = 100,  # Return 100% as last point
      kappa_loss = data$kappa_loss[nrow(data)]
    ))
  } else { # If not found, return NA
    cat("NA", "\n")
    return(list(
      critical_percentage = NA,
      kappa_loss = NA
    ))
  }
}

# Store threshold results
threshold_results <- data.frame(
  dataset_name = character(),
  technique = character(),
  noise_level = numeric(),
  threshold = numeric(),
  critical_percentage = numeric(),
  kappa_at_critical = numeric(),
  stringsAsFactors = FALSE
)

# Create grid for parameters
threshold_grid <- expand.grid(
  dataset = dataset_names,
  tech = techniques,
  noise_lvl = noise_levels,
  thresh_lvl = kl_thresholds,
  stringsAsFactors = FALSE
)

# Apply function (tidyverse)
results_list <- purrr::pmap(threshold_grid, function(dataset, tech, noise_lvl, thresh_lvl) {
  # Filter data
  current_data <- klc_data %>%
    filter(dataset_name == dataset, technique == tech, noise == noise_lvl)
  cat("*------------------------------------*\n")
  cat("Processing:", dataset, "-", tech, "| Noise Level:", noise_lvl, "| Threshold:", thresh_lvl, "\n")
  
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
cat("*------------------------------------*\n")

# Combine all results into one data frame
threshold_results <- do.call(rbind, results_list)

# Save the results
results_path <- here("results", "threshold_instance_results.csv")
write.csv(threshold_results, file = results_path, row.names = FALSE)
cat("Results recorded in:", results_path, "\n")
