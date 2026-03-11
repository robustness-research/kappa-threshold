#!/usr/bin/env Rscript

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

args <- commandArgs(trailingOnly = TRUE)

# Parse arguments
k_value <- 4
output_file <- NULL
datasets <- c()

for (i in seq_along(args)) {
  arg <- args[i]
  datasets <- c(datasets, arg)
}

output_file <- "data/results/clustering/predicted_group.csv"

get_dataset_path <- function(input_value) {
  is_dataset_name <- !grepl("[/\\\\]", input_value) && tools::file_ext(input_value) == ""

  candidates <- c(
    file.path("data", "datasets", paste0(input_value, ".csv")),
    file.path("data", "datasets", "arff", paste0(input_value, ".csv"))
  )

  if (is_dataset_name) {
    for (candidate in candidates) {
      if (file.exists(candidate) && !dir.exists(candidate)) {
        return(normalizePath(candidate))
      }
    }
  }

  if (file.exists(input_value) && !dir.exists(input_value)) {
    return(normalizePath(input_value))
  }

  if (!is_dataset_name) {
    for (candidate in candidates) {
      if (file.exists(candidate) && !dir.exists(candidate)) {
        return(normalizePath(candidate))
      }
    }
  }

  stop(paste0("Could not resolve dataset input to a CSV file: ", input_value))
}

normalize_class_column <- function(dataset) {
  normalized_names <- tolower(trimws(colnames(dataset)))
  class_col <- colnames(dataset)[normalized_names == "class"]
  
  if (length(class_col) == 0) {
    stop("Dataset must contain a 'class' column (case-insensitive)")
  }
  if (length(class_col) > 1) {
    stop(paste("Multiple columns match 'class' (case-insensitive):", paste(class_col, collapse = ", ")))
  }
  
  if (class_col != "class") {
    names(dataset)[names(dataset) == class_col] <- "class"
    cat("Renamed column '", class_col, "' to 'class'\n", sep = "")
  }
  
  return(dataset)
}

get_attribute_type <- function(dataset) {
  dataset_subset <- dataset[, !names(dataset) %in% "class", drop = FALSE]

  numerical_count <- 0
  nominal_count <- 0

  for (col in names(dataset_subset)) {
    if (is.numeric(dataset_subset[[col]])) {
      numerical_count <- numerical_count + 1
    } else {
      nominal_count <- nominal_count + 1
    }
  }

  return(list(numerical = numerical_count, nominal = nominal_count))
}

get_dataset_type <- function(dataset) {
  if (!"class" %in% names(dataset)) {
    stop("Dataset must contain a 'class' column")
  }

  num_unique_classes <- length(unique(dataset$class))

  if (num_unique_classes == 2) {
    return(0)
  }
  if (num_unique_classes > 2) {
    return(1)
  }

  stop("Invalid dataset: class column has fewer than 2 unique values")
}

compute_characteristics <- function(dataset_name, dataset) {
  attr_types <- get_attribute_type(dataset)

  data.frame(
    dataset_name = dataset_name,
    instances_n = nrow(dataset),
    attributes_num = attr_types$numerical,
    attributes_nom = attr_types$nominal,
    dataset_type = get_dataset_type(dataset),
    stringsAsFactors = FALSE
  )
}

characteristics_reference <- read.csv("data/results/clustering/characteristics.csv", stringsAsFactors = FALSE)
centroids <- read.csv("data/results/clustering/centroids.csv", stringsAsFactors = FALSE)

feature_cols <- c("instances_n", "attributes_num", "attributes_nom", "dataset_type")

for (col in feature_cols) {
  characteristics_reference[[col]] <- as.numeric(characteristics_reference[[col]])
  centroids[[col]] <- as.numeric(centroids[[col]])
}

means <- sapply(characteristics_reference[, feature_cols, drop = FALSE], mean)
sds <- sapply(characteristics_reference[, feature_cols, drop = FALSE], sd)

if (any(sds == 0)) {
  stop("At least one feature has zero standard deviation in reference characteristics")
}

centroids_k <- centroids[centroids$K == k_value, c(feature_cols, "K"), drop = FALSE]
if (nrow(centroids_k) == 0) {
  stop(paste0("No centroids found for K=", k_value))
}

distance_to_centroid <- function(centroid_row, x) {
  sqrt(sum((as.numeric(centroid_row[feature_cols]) - x) ^ 2))
}

# Process each dataset and accumulate results
all_predictions <- data.frame()

for (dataset_input in datasets) {
  tryCatch({
    dataset_path <- get_dataset_path(dataset_input)
    dataset_name <- tools::file_path_sans_ext(basename(dataset_path))
    
    dataset_df <- read.csv(dataset_path, stringsAsFactors = FALSE)
    dataset_df <- normalize_class_column(dataset_df)
    
    dataset_characteristics <- compute_characteristics(dataset_name, dataset_df)
    
    scaled_features <- (as.numeric(dataset_characteristics[1, feature_cols]) - means) / sds
    names(scaled_features) <- feature_cols
    
    distances <- apply(centroids_k[, feature_cols, drop = FALSE], 1, distance_to_centroid, x = scaled_features)
    predicted_group <- which.min(distances)
    
    prediction <- data.frame(
      dataset_name = dataset_name,
      K = k_value,
      predicted_group = predicted_group,
      distance_to_group = as.numeric(distances[predicted_group]),
      instances_n = dataset_characteristics$instances_n,
      attributes_num = dataset_characteristics$attributes_num,
      attributes_nom = dataset_characteristics$attributes_nom,
      dataset_type = dataset_characteristics$dataset_type,
      stringsAsFactors = FALSE
    )
    
    all_predictions <- rbind(all_predictions, prediction)
    
    # Print results for this dataset
    cat("Predicted dataset group\n")
    cat("-----------------------\n")
    cat("Dataset:", dataset_name, "\n")
    cat("K:", k_value, "\n")
    cat("Group:", predicted_group, "\n")
    cat("Distance:", round(as.numeric(distances[predicted_group]), 6), "\n")
    cat("\n")
    
  }, error = function(e) {
    cat("Error processing dataset '", dataset_input, "': ", e$message, "\n\n", sep = "")
  })
}

# Save all predictions to output file
if (nrow(all_predictions) > 0) {
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  write.csv(all_predictions, output_file, row.names = FALSE)
  cat("All predictions saved to:", output_file, "\n")
}

