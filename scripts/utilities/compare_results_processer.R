#!/usr/bin/env Rscript

# Libraries
library(tidyverse)
library(gridExtra)

# Parameters
params <- read_csv("data/parameters.csv", show_col_types = FALSE)
datasets <- strsplit(params$values[params$parameter == "dataset_name"], "\\|")[[1]]
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
target_noise_levels <- c(0.1, 0.2, 0.3) # Noise list to filter
#instances <- strsplit(params$values[params$parameter == "instance_level"], "\\|")[[1]]

# Create output directory
output_dir <- "data/results/plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Function to read and process results for a dataset
process_dataset_results <- function(dataset) {
  cat(sprintf("Processing dataset: %s\n", dataset))
  
  all_results <- list()
  
  # Process both original and train_noise directories
  for (dataset_type in c("original", "train_noise")) {

    if(dataset_type == "original") {
      results_dir <- file.path("data", "results", "instances", dataset_type, "by_dataset")
    } else {
      results_dir <- file.path("data", "results", "instances", dataset_type, "by_dataset", "threshold_15")
    }
    
    
    if (!dir.exists(results_dir)) {
      cat(sprintf("  Directory not found: %s\n", results_dir))
      all_results[[dataset_type]] <- NULL
      next
    }
    
    # Get averaged results
    pattern <- paste0("^", dataset, "_results_avg\\.csv$")
    result_file <- list.files(results_dir, pattern = pattern, full.names = TRUE)
    
    # Skip if file doesn't exist
    if (length(result_file) == 0) {
      cat(sprintf("  Skipping %s - results file not found\n", dataset_type))
      all_results[[dataset_type]] <- NULL
      next
    }
    
    # Read the file
    data <- read_csv(result_file, show_col_types = FALSE)
    data$dataset_type <- dataset_type
    all_results[[dataset_type]] <- data
  }
  
  # Check if both original and train_noise results exist
  if (is.null(all_results[["original"]]) || is.null(all_results[["train_noise"]])) {
    cat("  Skipping dataset - both original and train_noise results required\n")
    return(NULL)
  }
  
  # Combine results
  combined_data <- bind_rows(all_results)
  
  # Filter for specified noise levels only
  filtered_data <- combined_data %>%
    filter(noise_levels %in% target_noise_levels)
  
  return(filtered_data)
}

# Process all datasets
all_data <- map_dfr(datasets, process_dataset_results)

if (is.null(all_data) || nrow(all_data) == 0) {
  cat("No data found. Exiting.\n")
  quit(status = 1)
}

# Load threshold instance results for threshold = 0.15
threshold_results <- read_csv("data/results/threshold_instance_results.csv", show_col_types = FALSE) %>%
  filter(threshold == 0.15) %>%
  rename(dataset = dataset_name, method = technique, noise_levels = noise_level)

# Create plots for each dataset
cat("\nGenerating plots...\n")

for (ds in unique(all_data$dataset)) {
  dataset_data <- all_data %>% filter(dataset == ds)
  
  # Check if both dataset types are present
  available_types <- unique(dataset_data$dataset_type)
  if (!all(c("original", "train_noise") %in% available_types)) {
    cat(sprintf("  Skipping %s - missing one or both dataset types\n", ds))
    next
  }
  
  cat(sprintf("  Creating plots for %s\n", ds))
  
  # Lists to store all plots for this dataset
  kappa_plots <- list()
  bar_plots <- list()
  
  # Create plots for each method
  for (model in unique(dataset_data$method)) {
    method_data <- dataset_data %>% filter(method == model)
    
    # Add (0,0) starting points for each noise level and dataset type combination
    zero_points <- method_data %>%
      select(dataset, method, noise_levels, dataset_type) %>%
      distinct() %>%
      mutate(instance_level = 0, avg_kappa_loss = 0, avg_accuracy = NA)
    
    method_data <- bind_rows(zero_points, method_data) %>%
      arrange(dataset_type, noise_levels, instance_level)
    
    if (nrow(method_data) == 0) next
    
    # Plot 1: Original dataset type
    # Get threshold data for this dataset and method
    threshold_data <- threshold_results %>%
      filter(dataset == ds, method == model, noise_levels %in% target_noise_levels)
    
    p1 <- ggplot(method_data %>% filter(dataset_type == "original"), 
                 aes(x = factor(instance_level), y = avg_kappa_loss, 
                     color = noise_levels, group = noise_levels)) +
      geom_vline(data = threshold_data, aes(xintercept = critical_percentage), 
                 linetype = "dotted", color = "orange", alpha = 0.7, linewidth = 0.8) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(title = sprintf("%s - Original", model),
           x = "Instance Level", y = "Kappa Loss", color = "Noise Levels") +
      theme_bw() +
      scale_y_continuous(limits = c(0.0, 0.3), breaks = seq(0, 0.3, by = 0.05)) +
      theme(legend.position = "bottom")
    
    # Plot 2: Train noise dataset type
    p2 <- ggplot(method_data %>% filter(dataset_type == "train_noise"), 
                 aes(x = factor(instance_level), y = avg_kappa_loss, 
                     color = noise_levels, group = noise_levels)) +
      geom_vline(data = threshold_data, aes(xintercept = critical_percentage), 
                 linetype = "dotted", color = "orange", alpha = 0.7, linewidth = 0.8) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(title = sprintf("%s - Train Noise", model),
           x = "Instance Level", y = "Kappa Loss", color = "Noise Levels") +
      theme_bw() +
      scale_y_continuous(limits = c(0.0, 0.3), breaks = seq(0, 0.3, by = 0.05)) +
      theme(legend.position = "bottom")
    
    # Combine plots side by side for this method
    combined_plot <- grid.arrange(p1, p2, ncol = 2)
    
    # Store the combined kappa plot
    kappa_plots[[model]] <- combined_plot
    
    # Prepare data for bar chart comparison
    comparison_data <- method_data %>%
      filter(!is.na(avg_accuracy)) %>%  # Filter out the zero points added for kappa plots
      select(dataset_type, noise_levels, instance_level, avg_accuracy) %>%
      mutate(noise_levels = factor(noise_levels, levels = c("0.1", "0.2", "0.3")))
    
    # Create grouped bar chart with noise levels as separate plots
    p_bar <- ggplot(comparison_data, 
                    aes(x = factor(instance_level), y = avg_accuracy, 
                        fill = dataset_type)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.7, width = 0.7) +
      facet_wrap(~ noise_levels, ncol = 1, labeller = labeller(noise_levels = function(x) paste("Noise Level:", x))) +
      labs(title = sprintf("%s - %s: Accuracy Comparison", ds, model),
           x = "Instance Level", 
           y = "Average Accuracy", 
           fill = "Dataset Type") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                        labels = c("original" = "Original", "train_noise" = "Train Noise")) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            strip.text = element_text(size = 11, face = "bold"))
    
    # Store the bar plot
    bar_plots[[model]] <- p_bar
  }
  
  # Save all kappa loss plots in a grid
  if (length(kappa_plots) > 0) {
    n_plots <- length(kappa_plots)
    n_cols <- min(2, n_plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    output_file_kappa <- file.path(output_dir, 
                                    sprintf("%s_all_kappa_loss.png", ds))
    
    # Arrange all kappa plots in a grid
    all_kappa_grid <- grid.arrange(grobs = kappa_plots, ncol = n_cols)
    ggsave(output_file_kappa, all_kappa_grid, 
           width = 12 * n_cols, height = 6 * n_rows, dpi = 300, limitsize = FALSE)
    
    cat(sprintf("    Saved kappa loss grid: %s\n", output_file_kappa))
  }
  
  # Save all bar plots in a grid
  if (length(bar_plots) > 0) {
    n_plots <- length(bar_plots)
    n_cols <- min(2, n_plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    output_file_bars <- file.path(output_dir, 
                                   sprintf("%s_all_accuracy_comparison.png", ds))
    
    # Arrange all bar plots in a grid
    all_bars_grid <- grid.arrange(grobs = bar_plots, ncol = n_cols)
    ggsave(output_file_bars, all_bars_grid, 
           width = 10 * n_cols, height = 12 * n_rows, dpi = 300, limitsize = FALSE)
    
    cat(sprintf("    Saved accuracy comparison grid: %s\n", output_file_bars))
  }
}

cat("\nCompleted!\n") 