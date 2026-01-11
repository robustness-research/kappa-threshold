#!/usr/bin/env Rscript

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,  # for data manipulation and visualization
    skimr,      # for data summary
    here        # for file path handling
)

# Load dataset names using here package
dataset_names <- readRDS(here("data", "files", "dataset_name.rds"))

#==============================================================

# Function to get the number of instances from a dataset
get_num_instances <- function(dataset) {
  num_instances <- nrow(dataset)
  return(num_instances)
}

# Function to determine the type of each attribute
get_attribute_type <- function(dataset) {
  # Exclude the "class" column
  dataset_subset <- dataset[, !names(dataset) %in% "class"]
  
  # Initialize counts
  numerical_count <- 0
  nominal_count <- 0
  
  # Loop through columns
  for (col in names(dataset_subset)) {
    if (is.numeric(dataset_subset[[col]])) {
      numerical_count <- numerical_count + 1
    } else {
      nominal_count <- nominal_count + 1
    }
  }
  
  # Return counts
  return(list(numerical = numerical_count, nominal = nominal_count))
}

# Function to determine if dataset is binary or multiclass
is_binary <- function(dataset) {
  unique_classes <- unique(dataset$class)
  num_unique_classes <- length(unique_classes)
  
  if (num_unique_classes == 2) {
    return("binary")
  } else if (num_unique_classes > 2) {
    return("multiclass")
  } else {
    stop("Invalid number of unique classes")
  }
}
#=============================================================================

# Function to print dataset characteristics
print_dataset_info <- function(dataset_name) {
  # Datasets are stored as RDS files in the datasets directory
  dataset_path <- here("data", "datasets", paste0(dataset_name, ".rds"))
  
  # Try to load dataset
  tryCatch({
    dataset <- readRDS(dataset_path)
    
    # Get characteristics using existing functions
    num_instances <- get_num_instances(dataset)
    attr_types <- get_attribute_type(dataset)
    class_type <- is_binary(dataset)
    num_classes <- length(unique(dataset$class))
    
    # Print characteristics in a formatted way
    cat("\n====== Dataset:", dataset_name, "======\n")
    cat("Number of instances:", num_instances, "\n")
    cat("Number of attributes:", attr_types$numerical + attr_types$nominal, "\n")
    cat("  - Numerical attributes:", attr_types$numerical, "\n")
    cat("  - Nominal attributes:", attr_types$nominal, "\n")
    cat("Class type:", class_type, "\n")
    cat("Number of classes:", num_classes, "\n")
    cat("Class distribution:\n")
    print(table(dataset$class))
    cat("\n")
  }, error = function(e) {
    cat("Error processing dataset", dataset_name, ":", e$message, "\n")
  })
}

# Function to print info for all datasets
print_all_datasets_info <- function() {
  for (name in dataset_names) {
    print_dataset_info(name)
  }
}

# Execute the function when script is run directly
if (!interactive()) {
  print_all_datasets_info()
}