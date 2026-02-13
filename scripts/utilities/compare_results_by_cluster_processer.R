#!/usr/bin/env Rscript

# Libraries
library(tidyverse)
library(gridExtra)

# Parameters
params <- read_csv("data/parameters.csv", show_col_types = FALSE)
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
target_noise_levels <- c(0.1, 0.2, 0.3) # Noise list to filter
threshold_levels <- as.numeric(strsplit(params$values[params$parameter == "threshold_level"], "\\|")[[1]])

# Load cluster information
clusters_df <- read_csv("data/results/clustering/groups.csv", show_col_types = FALSE)

# Create output directory
output_dir <- "data/results/plots/by_cluster"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Function to read and process results for datasets in a cluster
process_cluster_results <- function(cluster_id, cluster_datasets, threshold) {
  cat(sprintf("Processing cluster: %d (datasets: %s) (threshold: %s)\n", 
              cluster_id, paste(cluster_datasets, collapse = ", "), threshold))
  
  all_results <- list()
  
  # Process both original and train_noise directories
  for (dataset_type in c("original", "train_noise")) {
    
    if (dataset_type == "original") {
      results_dir <- file.path("data", "results", "instances", dataset_type, "by_dataset")
    } else {
      threshold_dir <- paste0("threshold_", gsub("\\.", ".", threshold))
      results_dir <- file.path("data", "results", "instances", dataset_type, "by_dataset", threshold_dir)
    }
    
    if (!dir.exists(results_dir)) {
      cat(sprintf("  Directory not found: %s\n", results_dir))
      all_results[[dataset_type]] <- NULL
      next
    }
    
    # Load and combine results for all datasets in this cluster
    cluster_data_list <- list()
    
    for (dataset in cluster_datasets) {
      pattern <- paste0("^", dataset, "_results_avg\\.csv$")
      result_file <- list.files(results_dir, pattern = pattern, full.names = TRUE)
      
      if (length(result_file) > 0) {
        data <- read_csv(result_file, show_col_types = FALSE)
        data$dataset <- dataset
        cluster_data_list[[dataset]] <- data
      }
    }
    
    if (length(cluster_data_list) > 0) {
      combined_data <- bind_rows(cluster_data_list)
      combined_data$dataset_type <- dataset_type
      all_results[[dataset_type]] <- combined_data
    } else {
      all_results[[dataset_type]] <- NULL
    }
  }
  
  # Check if both original and train_noise results exist
  if (is.null(all_results[["original"]]) || is.null(all_results[["train_noise"]])) {
    cat("  Skipping cluster - both original and train_noise results required\n")
    return(NULL)
  }
  
  # Combine results
  combined_data <- bind_rows(all_results)
  
  # Filter for specified noise levels only
  filtered_data <- combined_data %>%
    filter(noise_levels %in% target_noise_levels)
  
  return(filtered_data)
}

# Load all threshold instance results
all_threshold_results <- read_csv("data/results/threshold_instance_results.csv", show_col_types = FALSE) %>%
  rename(dataset = dataset_name, method = technique, noise_levels = noise_level)

# Get unique clusters and their datasets
clusters_info <- clusters_df %>%
  select(dataset_name, kmeans_4) %>%
  group_by(kmeans_4) %>%
  summarize(datasets = list(dataset_name), .groups = 'drop') %>%
  rename(cluster_id = kmeans_4)

# Loop through each threshold
for (threshold in threshold_levels) {
  cat(sprintf("\n=== Processing threshold: %s ===\n", threshold))
  
  # Loop through each cluster
  for (i in seq_len(nrow(clusters_info))) {
    cluster_id <- clusters_info$cluster_id[i]
    cluster_datasets <- clusters_info$datasets[[i]]
    
    cat(sprintf("\n--- Cluster %d ---\n", cluster_id))
    
    # Process this cluster for the current threshold
    all_data <- process_cluster_results(cluster_id, cluster_datasets, threshold = threshold)
    
    if (is.null(all_data) || nrow(all_data) == 0) {
      cat(sprintf("No data found for cluster %d with threshold %s. Skipping.\n", cluster_id, threshold))
      next
    }
    
    # Filter threshold results for current threshold and datasets in this cluster
    threshold_results <- all_threshold_results %>%
      filter(threshold == !!threshold, dataset %in% cluster_datasets)
    
    cat(sprintf("Generating plots for cluster %d, threshold %s...\n", cluster_id, threshold))
    
    # Lists to store all plots for this cluster
    kappa_plots <- list()
    bar_plots <- list()
    
    # Create plots for each method
    for (model in unique(all_data$method)) {
      method_data <- all_data %>% filter(method == model)
      
      # Add (0,0) starting points for each noise level and dataset type combination
      zero_points <- method_data %>%
        select(method, noise_levels, dataset_type) %>%
        distinct() %>%
        mutate(instance_level = 0, avg_kappa_loss = 0, avg_accuracy = NA)
      
      method_data <- bind_rows(zero_points, method_data) %>%
        arrange(dataset_type, noise_levels, instance_level)
      
      if (nrow(method_data) == 0) next
      
      # Process both dataset types (original and train_noise)
      for (ds_type in c("original", "train_noise")) {
        dataset_type_data <- method_data %>% filter(dataset_type == ds_type)
        
        # Calculate average kappa loss and accuracy across all datasets in the cluster
        plot_data <- dataset_type_data %>%
          group_by(noise_levels, instance_level) %>%
          summarize(
            avg_kappa_loss = mean(avg_kappa_loss, na.rm = TRUE),
            avg_accuracy = mean(avg_accuracy, na.rm = TRUE),
            sd_kappa_loss = sd(avg_kappa_loss, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # Get average instance level (critical_percentage) for this cluster
        threshold_data <- threshold_results %>%
          filter(method == model, noise_levels %in% target_noise_levels) %>%
          group_by(noise_levels) %>%
          summarize(avg_critical_percentage = mean(critical_percentage, na.rm = TRUE),
                    .groups = 'drop')
        
        # Convert critical_percentage to instance level format
        instance_levels_sorted <- sort(unique(plot_data$instance_level))
        threshold_data <- threshold_data %>%
          mutate(
            critical_instance = avg_critical_percentage / 100,
            critical_pos = sapply(critical_instance, function(x) {
              closest_idx <- which.min(abs(instance_levels_sorted - x))
              if (length(closest_idx) == 0 || is.na(closest_idx)) return(NA)
              as.numeric(closest_idx)
            })
          ) %>%
          filter(!is.na(critical_pos))
        
        # Create base kappa loss plot
        p_kappa <- ggplot(plot_data, 
                         aes(x = factor(instance_level), y = avg_kappa_loss, 
                             color = noise_levels, group = noise_levels)) +
          geom_point(size = 3) +
          geom_line(linewidth = 1)
        
        # Add vertical lines for each valid threshold
        if (nrow(threshold_data) > 0) {
          for (i in seq_len(nrow(threshold_data))) {
            p_kappa <- p_kappa +
              geom_vline(xintercept = threshold_data$critical_pos[i], 
                        linetype = "dashed", color = "red", 
                        alpha = 0.6, linewidth = 1.2, show.legend = FALSE)
          }
        }
        
        p_kappa <- p_kappa +
          labs(title = sprintf("%s - %s", model, ifelse(ds_type == "original", "Original", "Train Noise")),
               x = "Instance Level", y = "Kappa Loss", color = "Noise Levels",
               caption = "Red dashed line = avg instance level before degradation") +
          theme_bw() +
          scale_y_continuous(limits = c(0.0, 0.3), breaks = seq(0, 0.3, by = 0.05)) +
          theme(legend.position = "bottom",
                plot.caption = element_text(size = 9, hjust = 0))
        
        # Store plot
        plot_key <- sprintf("%s_%s", model, ds_type)
        kappa_plots[[plot_key]] <- p_kappa
        
        # Create bar chart for accuracy comparison
        bar_data <- plot_data %>%
          filter(!is.na(avg_accuracy)) %>%
          mutate(noise_levels = factor(noise_levels, levels = c("0.1", "0.2", "0.3")))
        
        if (nrow(bar_data) > 0) {
          p_bar <- ggplot(bar_data, 
                         aes(x = factor(instance_level), y = avg_accuracy)) +
            geom_bar(stat = "identity", fill = "#42c0f6", alpha = 0.7, width = 0.7) +
            facet_wrap(~ noise_levels, ncol = 1, 
                      labeller = labeller(noise_levels = function(x) paste("Noise Level:", x))) +
            labs(title = sprintf("%s - %s: Accuracy", model, ifelse(ds_type == "original", "Original", "Train Noise")),
                 x = "Instance Level", 
                 y = "Average Accuracy") +
            theme_bw() +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(angle = 0, hjust = 0.5),
                  strip.text = element_text(size = 11, face = "bold"))
          
          bar_plots[[plot_key]] <- p_bar
        }
      }
    }
    
    # Save all kappa loss plots in a grid
    if (length(kappa_plots) > 0) {
      n_plots <- length(kappa_plots)
      n_cols <- 2
      n_rows <- ceiling(n_plots / n_cols)
      
      output_file_kappa <- file.path(output_dir, 
                                      sprintf("cluster_%d_threshold_%s_kappa_loss.png", 
                                              cluster_id, gsub("\\.", "_", threshold)))
      
      # Arrange all kappa plots in a grid
      all_kappa_grid <- grid.arrange(grobs = kappa_plots, ncol = n_cols)
      ggsave(output_file_kappa, all_kappa_grid, 
             width = 14, height = 4.5 * n_rows, dpi = 300, limitsize = FALSE)
      
      cat(sprintf("    Saved kappa loss grid: %s\n", output_file_kappa))
    }
    
    # Save all bar plots in a grid
    if (length(bar_plots) > 0) {
      n_plots <- length(bar_plots)
      n_cols <- 2
      n_rows <- ceiling(n_plots / n_cols)
      
      output_file_bars <- file.path(output_dir, 
                                     sprintf("cluster_%d_threshold_%s_accuracy.png", 
                                             cluster_id, gsub("\\.", "_", threshold)))
      
      # Arrange all bar plots in a grid
      all_bars_grid <- grid.arrange(grobs = bar_plots, ncol = n_cols)
      ggsave(output_file_bars, all_bars_grid, 
             width = 12, height = 5 * n_rows, dpi = 300, limitsize = FALSE)
      
      cat(sprintf("    Saved accuracy grid: %s\n", output_file_bars))
    }
  }
}

cat("\n=== CLUSTER-BASED ANALYSIS COMPLETE ===\n")
cat("Plots saved to: data/results/plots/by_cluster/\n")
