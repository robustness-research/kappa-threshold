#!/usr/bin/env Rscript

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
project.root <- normalizePath(file.path(script.dir, "..", ".."))
setwd(project.root)

# Load dataset names from parameters.csv
#params <- read.csv("data/parameters.csv", stringsAsFactors = FALSE)
#dataset_names <- unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|"))

dataset_names <- c("mfeat-pixel", "monks-problems-2", "steel-plates-fault", "vehicle", "wall-robot-navigation")

#==============================================================

# Function to get the number of instances from a dataset
get_num_instances <- function(dataset) {
  num_instances <- nrow(dataset)
  return(num_instances)
}

# Function to determine the type of each attribute
get_attribute_type <- function(dataset) {
  # Exclude the class column (case-insensitive)
  class_col <- colnames(dataset)[tolower(colnames(dataset)) == "class"]
  cols_to_exclude <- c(class_col)
  dataset_subset <- dataset[, !names(dataset) %in% cols_to_exclude, drop = FALSE]
  
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

# Function to normalize class column (handle case variants)
normalize_class_column <- function(dataset) {
  class_col <- colnames(dataset)[tolower(colnames(dataset)) == "class"]
  
  if (length(class_col) == 0) {
    return(NULL)  # No class column found
  }
  
  if (length(class_col) > 1) {
    stop(paste("Multiple columns match 'class' (case-insensitive):", paste(class_col, collapse = ", ")))
  }
  
  if (class_col != "class") {
    names(dataset)[names(dataset) == class_col] <- "class"
    cat("  (Normalized column '", class_col, "' to 'class')\n", sep = "")
  }
  
  return(dataset)
}

# Function to print dataset characteristics
print_dataset_info <- function(dataset_name) {
  # Datasets are stored as CSV files
  dataset_path <- file.path("data", "datasets", "arff", paste0(dataset_name, ".csv"))
  
  # Try to load dataset
  tryCatch({
    dataset <- read.csv(dataset_path, stringsAsFactors = FALSE)
    dataset <- normalize_class_column(dataset)
    
    if (is.null(dataset)) {
      cat("Error processing dataset", dataset_name, ": no 'class' column found\n")
      return(invisible(NULL))
    }
    
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