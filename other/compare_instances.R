# Compare kappa values between results/instances and results/instances/with_noise

# Get list of all dataset CSV files
base_dir <- "../results/instances"
noise_dir <- "../results/instances/with_noise"

# Get all CSV files from base directory
base_files <- list.files(base_dir, pattern = "*_results.csv$", full.names = FALSE)

cat("=== Comparing files ===\n")
cat("Found", length(base_files), "files to compare\n\n")

# Create a list to store all comparison results
all_comparisons <- list()

for (file_name in base_files) {
  base_file <- file.path(base_dir, file_name)
  noise_file <- file.path(noise_dir, file_name)
  
  # Check if both files exist
  if (!file.exists(base_file)) {
    cat("Skipping", file_name, "- base file not found\n")
    next
  }
  
  if (!file.exists(noise_file)) {
    cat("Skipping", file_name, "- noise file not found\n")
    next
  }
  
  cat("Comparing:", file_name, "\n")
  
  # Read both files
  base_data <- read.csv(base_file, stringsAsFactors = FALSE)
  noise_data <- read.csv(noise_file, stringsAsFactors = FALSE)
  
  cat("  Base file rows:", nrow(base_data), "\n")
  cat("  Noise file rows:", nrow(noise_data), "\n")
  
  # Merge the two datasets on common keys
  merged <- merge(
    base_data,
    noise_data,
    by = c("dataset", "fold", "method", "noise_levels"),
    suffixes = c("_base", "_noise"),
    all = FALSE  # Only keep matching rows
  )
  
  cat("  Merged rows:", nrow(merged), "\n")
  
  if (nrow(merged) > 0) {
    # Find rows where kappa or instance_level are different
    differences <- merged[
      merged$kappa_base != merged$kappa_noise | 
      merged$instance_level_base != merged$instance_level_noise,
    ]
    
    if (nrow(differences) > 0) {
      cat("  Differences found:", nrow(differences), "\n")
      
      # Select and rename columns for output
      comparison <- data.frame(
        dataset = differences$dataset,
        fold = differences$fold,
        method = differences$method,
        noise_levels = differences$noise_levels,
        instance_level_base = differences$instance_level_base,
        instance_level_noise = differences$instance_level_noise,
        kappa_base = differences$kappa_base,
        kappa_noise = differences$kappa_noise,
        kappa_diff = differences$kappa_base - differences$kappa_noise,
        instance_diff = differences$instance_level_base - differences$instance_level_noise,
        stringsAsFactors = FALSE
      )
      
      all_comparisons[[length(all_comparisons) + 1]] <- comparison
    } else {
      cat("  No differences found\n")
    }
  }
  
  cat("\n")
}

# Combine all comparisons
if (length(all_comparisons) > 0) {
  final_comparison <- do.call(rbind, all_comparisons)
  
  cat("=== Summary ===\n")
  cat("Total differences found:", nrow(final_comparison), "\n")
  cat("Datasets with differences:", length(unique(final_comparison$dataset)), "\n")
  
  # Show a sample of the differences
  cat("\n=== Sample of differences (first 20 rows) ===\n")
  print(head(final_comparison, 20))
  
  # Write to CSV
  output_file <- "../results/kappa_comparison.csv"
  write.csv(final_comparison, output_file, row.names = FALSE)
  cat("\n=== Output saved to:", output_file, "===\n")
  cat("Total rows in output:", nrow(final_comparison), "\n")
  
  # Summary statistics
  cat("\n=== Difference Statistics ===\n")
  cat("Kappa differences:\n")
  cat("  Mean:", mean(abs(final_comparison$kappa_diff), na.rm = TRUE), "\n")
  cat("  Max:", max(abs(final_comparison$kappa_diff), na.rm = TRUE), "\n")
  cat("  Min:", min(abs(final_comparison$kappa_diff), na.rm = TRUE), "\n")
  
  cat("\nInstance level differences:\n")
  cat("  Mean:", mean(abs(final_comparison$instance_diff), na.rm = TRUE), "\n")
  cat("  Max:", max(abs(final_comparison$instance_diff), na.rm = TRUE), "\n")
  cat("  Min:", min(abs(final_comparison$instance_diff), na.rm = TRUE), "\n")
  
  # Show breakdown by dataset
  cat("\n=== Differences by Dataset ===\n")
  diff_by_dataset <- table(final_comparison$dataset)
  print(diff_by_dataset)
  
} else {
  cat("=== No differences found across all datasets ===\n")
}
