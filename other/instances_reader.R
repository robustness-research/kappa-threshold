# Load the RDS file
data <- readRDS("../data/prev/instancesCM_list.rds")

# Print the structure of the data to understand its format
cat("=== Structure of the data (first 2 levels) ===\n")
str(data, max.level = 2)

cat("\n=== Dataset names ===\n")
dataset_names <- names(data)
print(dataset_names)

# Extract kappa values for noise levels 10, 20, and 30
# The structure is: data[[dataset]][[fold]][[method]][[noise_level]][[instance]]
# noise_level is stored as "noise_10", "noise_20", "noise_30", etc.

# Target noise levels
target_noise_levels <- c("noise_10", "noise_20", "noise_30")

# Process each dataset
for (dataset_name in dataset_names) {
  cat("\n=== Processing dataset:", dataset_name, "===\n")
  
  # Create a list to store all rows for this dataset
  result_rows <- list()
  
  dataset_data <- data[[dataset_name]]
  fold_names <- names(dataset_data)
  
  for (fold_name in fold_names) {
    fold_data <- dataset_data[[fold_name]]
    method_names <- names(fold_data)
    
    for (method_name in method_names) {
      method_data <- fold_data[[method_name]]
      noise_level_names <- names(method_data)
      
      # Filter for target noise levels
      for (noise_level_name in noise_level_names) {
        if (noise_level_name %in% target_noise_levels) {
          # Extract the numeric noise level (e.g., "noise_10" -> 10)
          noise_numeric <- as.numeric(sub("noise_", "", noise_level_name))
          noise_fraction <- noise_numeric / 100  # Convert to 0.1, 0.2, 0.3 format
          
          noise_data <- method_data[[noise_level_name]]
          instance_names <- names(noise_data)
          
          for (instance_name in instance_names) {
            instance_data <- noise_data[[instance_name]]
            
            # Extract accuracy and kappa from the confusion matrix
            if ("overall" %in% names(instance_data)) {
              accuracy <- instance_data$overall["Accuracy"]
              kappa <- instance_data$overall["Kappa"]
              
              # Create a row
              row <- data.frame(
                dataset = dataset_name,
                fold = fold_name,
                method = method_name,
                noise_levels = noise_fraction,
                instance_level = as.numeric(instance_name) / 100,
                accuracy = accuracy,
                kappa = kappa,
                stringsAsFactors = FALSE
              )
              
              result_rows[[length(result_rows) + 1]] <- row
            }
          }
        }
      }
    }
  }
  
  # Combine all rows into a data frame
  if (length(result_rows) > 0) {
    result_df <- do.call(rbind, result_rows)
    
    # Sort by fold, method, noise_levels, instance_level
    result_df <- result_df[order(result_df$fold, result_df$method, 
                                  result_df$noise_levels, result_df$instance_level), ]
    
    # Print first few rows
    cat("First few rows for", dataset_name, ":\n")
    print(head(result_df, 20))
    
    # Create output directory if it doesn't exist
    output_dir <- "../results/instances"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Write to CSV file
    output_file <- file.path(output_dir, paste0(dataset_name, "_results.csv"))
    write.csv(result_df, output_file, row.names = FALSE)
    cat("Saved to:", output_file, "\n")
    cat("Total rows:", nrow(result_df), "\n")
  } else {
    cat("No data found for", dataset_name, "\n")
  }
}