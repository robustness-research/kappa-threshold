#!/usr/bin/env Rscript

# Libraries
library(tidyverse)
library(gridExtra)

# Parameters
params <- read_csv("data/parameters.csv", show_col_types = FALSE)
datasets <- strsplit(params$values[params$parameter == "dataset_name"], "\\|")[[1]]
models <- strsplit(params$values[params$parameter == "technique_name"], "\\|")[[1]]
target_noise_levels <- c(0.1, 0.2, 0.3) # Noise list to filter

# Create output directory
output_dir <- "data/results"
plots_dir <- "data/results/summary_plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

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
cat("\n=== Loading Data ===\n")
all_data <- map_dfr(datasets, process_dataset_results)

if (is.null(all_data) || nrow(all_data) == 0) {
  cat("No data found. Exiting.\n")
  quit(status = 1)
}

cat(sprintf("\nTotal records loaded: %d\n", nrow(all_data)))

# ==================== Summary by Dataset ====================
cat("\n=== Generating Summary by Dataset ===\n")

summary_by_dataset <- all_data %>%
  group_by(dataset, dataset_type) %>%
  summarise(
    n_methods = n_distinct(method),
    n_noise_levels = n_distinct(noise_levels),
    n_instance_levels = n_distinct(instance_level),
    mean_kappa_loss = round(mean(avg_kappa_loss, na.rm = TRUE), 3),
    sd_kappa_loss = round(sd(avg_kappa_loss, na.rm = TRUE), 3),
    min_kappa_loss = round(min(avg_kappa_loss, na.rm = TRUE), 3),
    max_kappa_loss = round(max(avg_kappa_loss, na.rm = TRUE), 3),
    mean_accuracy = round(mean(avg_accuracy, na.rm = TRUE), 3),
    sd_accuracy = round(sd(avg_accuracy, na.rm = TRUE), 3),
    min_accuracy = round(min(avg_accuracy, na.rm = TRUE), 3),
    max_accuracy = round(max(avg_accuracy, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(dataset, dataset_type)

# Save summary by dataset
output_file_dataset <- file.path(output_dir, "summary_by_dataset.csv")
write_csv(summary_by_dataset, output_file_dataset)
cat(sprintf("Saved: %s\n", output_file_dataset))

# Print summary by dataset
cat("\n--- Summary by Dataset ---\n")
print(summary_by_dataset, n = Inf)

# Plot: Kappa Loss by Dataset
plot_kappa_dataset <- ggplot(summary_by_dataset, 
                              aes(x = reorder(dataset, mean_kappa_loss), 
                                  y = mean_kappa_loss, 
                                  fill = dataset_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kappa_loss - sd_kappa_loss, 
                    ymax = mean_kappa_loss + sd_kappa_loss),
                position = position_dodge(0.9), width = 0.25) +
  coord_flip() +
  labs(title = "Mean Kappa Loss by Dataset",
       x = "Dataset", 
       y = "Mean Kappa Loss",
       fill = "Dataset Type") +
  scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                    labels = c("original" = "Original", "train_noise" = "Train Noise")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(plots_dir, "kappa_loss_by_dataset.png"), 
       plot_kappa_dataset, width = 10, height = 8, dpi = 300)

# Plot: Accuracy by Dataset
plot_accuracy_dataset <- ggplot(summary_by_dataset, 
                                 aes(x = reorder(dataset, -mean_accuracy), 
                                     y = mean_accuracy, 
                                     fill = dataset_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_accuracy - sd_accuracy, 
                    ymax = mean_accuracy + sd_accuracy),
                position = position_dodge(0.9), width = 0.25) +
  coord_flip() +
  labs(title = "Mean Accuracy by Dataset",
       x = "Dataset", 
       y = "Mean Accuracy",
       fill = "Dataset Type") +
  scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                    labels = c("original" = "Original", "train_noise" = "Train Noise")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(plots_dir, "accuracy_by_dataset.png"), 
       plot_accuracy_dataset, width = 10, height = 8, dpi = 300)

cat("Saved plots: kappa_loss_by_dataset.png, accuracy_by_dataset.png\n")

# ==================== Summary by Model ====================
cat("\n=== Generating Summary by Model ===\n")

summary_by_model <- all_data %>%
  group_by(method, dataset_type) %>%
  summarise(
    n_datasets = n_distinct(dataset),
    n_noise_levels = n_distinct(noise_levels),
    n_instance_levels = n_distinct(instance_level),
    mean_kappa_loss = round(mean(avg_kappa_loss, na.rm = TRUE), 3),
    sd_kappa_loss = round(sd(avg_kappa_loss, na.rm = TRUE), 3),
    min_kappa_loss = round(min(avg_kappa_loss, na.rm = TRUE), 3),
    max_kappa_loss = round(max(avg_kappa_loss, na.rm = TRUE), 3),
    mean_accuracy = round(mean(avg_accuracy, na.rm = TRUE), 3),
    sd_accuracy = round(sd(avg_accuracy, na.rm = TRUE), 3),
    min_accuracy = round(min(avg_accuracy, na.rm = TRUE), 3),
    max_accuracy = round(max(avg_accuracy, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(method, dataset_type)

# Save summary by model
output_file_model <- file.path(output_dir, "summary_by_model.csv")
write_csv(summary_by_model, output_file_model)
cat(sprintf("Saved: %s\n", output_file_model))

# Print summary by model
cat("\n--- Summary by Model ---\n")
print(summary_by_model, n = Inf)

# Plot: Kappa Loss by Model
plot_kappa_model <- ggplot(summary_by_model, 
                            aes(x = reorder(method, mean_kappa_loss), 
                                y = mean_kappa_loss, 
                                fill = dataset_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kappa_loss - sd_kappa_loss, 
                    ymax = mean_kappa_loss + sd_kappa_loss),
                position = position_dodge(0.9), width = 0.25) +
  coord_flip() +
  labs(title = "Mean Kappa Loss by Model",
       x = "Model", 
       y = "Mean Kappa Loss",
       fill = "Dataset Type") +
  scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                    labels = c("original" = "Original", "train_noise" = "Train Noise")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(plots_dir, "kappa_loss_by_model.png"), 
       plot_kappa_model, width = 10, height = 6, dpi = 300)

# Plot: Accuracy by Model
plot_accuracy_model <- ggplot(summary_by_model, 
                               aes(x = reorder(method, -mean_accuracy), 
                                   y = mean_accuracy, 
                                   fill = dataset_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_accuracy - sd_accuracy, 
                    ymax = mean_accuracy + sd_accuracy),
                position = position_dodge(0.9), width = 0.25) +
  coord_flip() +
  labs(title = "Mean Accuracy by Model",
       x = "Model", 
       y = "Mean Accuracy",
       fill = "Dataset Type") +
  scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                    labels = c("original" = "Original", "train_noise" = "Train Noise")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(plots_dir, "accuracy_by_model.png"), 
       plot_accuracy_model, width = 10, height = 6, dpi = 300)

cat("Saved plots: kappa_loss_by_model.png, accuracy_by_model.png\n")

# ==================== Overall Summary ====================
cat("\n=== Generating Overall Summary ===\n")

overall_summary <- all_data %>%
  group_by(dataset_type) %>%
  summarise(
    n_datasets = n_distinct(dataset),
    n_methods = n_distinct(method),
    n_noise_levels = n_distinct(noise_levels),
    n_instance_levels = n_distinct(instance_level),
    total_records = n(),
    mean_kappa_loss = round(mean(avg_kappa_loss, na.rm = TRUE), 3),
    sd_kappa_loss = round(sd(avg_kappa_loss, na.rm = TRUE), 3),
    min_kappa_loss = round(min(avg_kappa_loss, na.rm = TRUE), 3),
    max_kappa_loss = round(max(avg_kappa_loss, na.rm = TRUE), 3),
    mean_accuracy = round(mean(avg_accuracy, na.rm = TRUE), 3),
    sd_accuracy = round(sd(avg_accuracy, na.rm = TRUE), 3),
    min_accuracy = round(min(avg_accuracy, na.rm = TRUE), 3),
    max_accuracy = round(max(avg_accuracy, na.rm = TRUE), 3),
    .groups = "drop"
  )

# Save overall summary
output_file_overall <- file.path(output_dir, "summary_overall.csv")
write_csv(overall_summary, output_file_overall)
cat(sprintf("Saved: %s\n", output_file_overall))

# Print overall summary
cat("\n--- Overall Summary ---\n")
print(overall_summary, n = Inf)

# Plot: Overall comparison
overall_long <- overall_summary %>%
  select(dataset_type, mean_kappa_loss, mean_accuracy) %>%
  pivot_longer(cols = c(mean_kappa_loss, mean_accuracy),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = case_when(
    metric == "mean_kappa_loss" ~ "Kappa Loss",
    metric == "mean_accuracy" ~ "Accuracy",
    TRUE ~ metric
  ))

plot_overall <- ggplot(overall_long, 
                       aes(x = dataset_type, y = value, fill = dataset_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Overall Performance Comparison",
       x = "Dataset Type", 
       y = "Value",
       fill = "Dataset Type") +
  scale_fill_manual(values = c("original" = "#42c0f6", "train_noise" = "#2E86AB"),
                    labels = c("original" = "Original", "train_noise" = "Train Noise")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(file.path(plots_dir, "overall_comparison.png"), 
       plot_overall, width = 10, height = 5, dpi = 300)

cat("Saved plot: overall_comparison.png\n")

# ==================== Comparison: Original vs Train Noise ====================
cat("\n=== Generating Comparison: Original vs Train Noise ===\n")

comparison_summary <- all_data %>%
  group_by(dataset, method, noise_levels, instance_level) %>%
  summarise(
    original_kappa_loss = round(mean(avg_kappa_loss[dataset_type == "original"], na.rm = TRUE), 3),
    train_noise_kappa_loss = round(mean(avg_kappa_loss[dataset_type == "train_noise"], na.rm = TRUE), 3),
    kappa_loss_diff = round(train_noise_kappa_loss - original_kappa_loss, 3),
    original_accuracy = round(mean(avg_accuracy[dataset_type == "original"], na.rm = TRUE), 3),
    train_noise_accuracy = round(mean(avg_accuracy[dataset_type == "train_noise"], na.rm = TRUE), 3),
    accuracy_diff = round(train_noise_accuracy - original_accuracy, 3),
    .groups = "drop"
  ) %>%
  arrange(dataset, method, noise_levels, instance_level)

# Save comparison
output_file_comparison <- file.path(output_dir, "comparison_original_vs_train_noise.csv")
write_csv(comparison_summary, output_file_comparison)
cat(sprintf("Saved: %s\n", output_file_comparison))

# Summary statistics of differences
cat("\n--- Difference Statistics (Train Noise - Original) ---\n")
diff_stats <- comparison_summary %>%
  summarise(
    mean_kappa_loss_diff = round(mean(kappa_loss_diff, na.rm = TRUE), 3),
    sd_kappa_loss_diff = round(sd(kappa_loss_diff, na.rm = TRUE), 3),
    min_kappa_loss_diff = round(min(kappa_loss_diff, na.rm = TRUE), 3),
    max_kappa_loss_diff = round(max(kappa_loss_diff, na.rm = TRUE), 3),
    mean_accuracy_diff = round(mean(accuracy_diff, na.rm = TRUE), 3),
    sd_accuracy_diff = round(sd(accuracy_diff, na.rm = TRUE), 3),
    min_accuracy_diff = round(min(accuracy_diff, na.rm = TRUE), 3),
    max_accuracy_diff = round(max(accuracy_diff, na.rm = TRUE), 3)
  )

print(diff_stats)

# Plot: Distribution of differences
plot_kappa_diff <- ggplot(comparison_summary, aes(x = kappa_loss_diff)) +
  geom_histogram(bins = 30, fill = "#2E86AB", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Distribution of Kappa Loss Differences (Train Noise - Original)",
       x = "Kappa Loss Difference",
       y = "Frequency") +
  theme_bw()

plot_accuracy_diff <- ggplot(comparison_summary, aes(x = accuracy_diff)) +
  geom_histogram(bins = 30, fill = "#2E86AB", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Distribution of Accuracy Differences (Train Noise - Original)",
       x = "Accuracy Difference",
       y = "Frequency") +
  theme_bw()

combined_diff_plot <- grid.arrange(plot_kappa_diff, plot_accuracy_diff, ncol = 2)

ggsave(file.path(plots_dir, "difference_distributions.png"), 
       combined_diff_plot, width = 12, height = 5, dpi = 300)

cat("Saved plot: difference_distributions.png\n")

cat("\n=== Summary Complete ===\n")
cat(sprintf("All summary files saved to: %s\n", output_dir))
cat(sprintf("All plots saved to: %s\n", plots_dir))
