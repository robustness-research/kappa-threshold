# /usr/bin/env/Rscript

# Print current working directory and parameters.csv path for debugging
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
project.root <- normalizePath(file.path(script.dir, ".."))
setwd(project.root)
cat("Current working directory:", getwd(), "\n")
cat("parameters.csv exists:", file.exists("data/parameters.csv"), "\n")
cat("parameters.csv absolute path:", normalizePath("data/parameters.csv", mustWork=FALSE), "\n")

# Packages that need to be loaded
pacman::p_load(caret, iml, citation, dplyr, earth, lime, data.table)

# Set the seed 
set.seed(1)

# Load parameters
load_parameters <- function(params_file) {
  # Datasets from command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  datasets <- args[1]
  models <- args[2]
  # Parameters
  params <- read.csv(params_file, stringsAsFactors = FALSE)
  # Control parameters
  control = trainControl(method = "none", number = 1)
  list(
    datasets = datasets,
    #datasets = unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|")),
    fold_names = unlist(strsplit(subset(params, parameter == "fold_name")$values, "\\|")),
    models = models,
    #models = unlist(strsplit(subset(params, parameter == "technique_name")$values, "\\|")),
    noise_levels = as.numeric(unlist(strsplit(subset(params, parameter == "noise_levels")$values, "\\|"))),
    instances = as.numeric(unlist(strsplit(subset(params, parameter == "instance_level")$values, "\\|"))),
    control = control
  )
}
 
# Model training function with noise injection in training data
train_model_noise <- function(method, train_df, train_df_indices, dataset, noise_level, threshold_results, mia_df, control) {
    # Look up the critical_percentage for this dataset/method/noise_level
    noise_level_pct <- noise_level * 100
    threshold_row <- subset(threshold_results, 
                            dataset_name == dataset & 
                            technique == method & 
                            noise_level == noise_level_pct)

    if(nrow(threshold_row) == 0) {
      stop(paste("No threshold found for dataset:", dataset, "method:", method, "noise_level:", noise_level_pct))
    }

    critical_percentage <- threshold_row$critical_percentage[1] / 100  # Convert to decimal
    cat("Critical percentage for training noise:", critical_percentage * 100, "%\n")

    # If critical_percentage is 0, train on clean data
    if(critical_percentage == 0) {
      cat("Training on clean data (critical_percentage = 0)\n")
      noisy_train_df <- train_df
    } else {
      # Get MIA for this dataset/method combination
      mia <- as.character(subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute[1])

      # Load noise data from CSV file
      noise_csv_file <- paste0("data/results/noise_injection/by_dataset/", dataset, "_", mia, "_noise", noise_level, ".csv")
      cat("Loading training noise data from:", noise_csv_file, "\n")

      if(!file.exists(noise_csv_file)) {
        stop(paste("Noise CSV file not found:", noise_csv_file))
      }

      noise_full_df <- read.csv(noise_csv_file, stringsAsFactors = FALSE)

      # Remove metadata columns if present
      noise_full_df$dataset_name <- NULL
      noise_full_df$altered_attribute <- NULL
      noise_full_df$noise_level <- NULL

      # Convert class column to factor
      noise_full_df$class <- as.factor(noise_full_df$class)

      # Select only the training set rows from the noisy dataset
      noise_train_df <- noise_full_df[train_df_indices, ]

      # Calculate number of instances to alter
      n_instances <- round(nrow(train_df) * critical_percentage, 0)
      cat("Altering", n_instances, "out of", nrow(train_df), "training instances\n")

      # Randomly sample indices to alter
      indices_to_alter <- sample(1:nrow(train_df), n_instances)

      # Create noisy training data
      noisy_train_df <- train_df
      noisy_train_df[indices_to_alter, ] <- noise_train_df[indices_to_alter, ]
    }

    # Train model with the noisy training data
    model_params <- list(
      "C5.0" = list(method = "C5.0"),
      "ctree" = list(method = "ctree"),
      "fda" = list(method = "fda"),
      "gbm" = list(method = "gbm"),
      "gcvEarth" = list(method = "gcvEarth"),
      "JRip" = list(method = "JRip"),
      "lvq" = list(method = "lvq"),
      "mlpML" = list(method = "mlpML"),
      "multinom" = list(method = "multinom", trControl = control, 
                       tuneGrid = expand.grid(decay = c(0)), MaxNWts = 10000),
      "naive_bayes" = list(method = "naive_bayes"),
      "PART" = list(method = "PART"),
      "rbfDDA" = list(method = "rbfDDA", trControl = control),
      "rda" = list(method = "rda"),
      "rf" = list(method = "rf"),
      "rpart" = list(method = "rpart"),
      "simpls" = list(method = "simpls"),
      "svmLinear" = list(method = "svmLinear"),
      "svmRadial" = list(method = "svmRadial"),
      "rfRules" = list(method = "rfRules"),
      "knn" = list(method = "knn", tuneGrid = expand.grid(k = 5:5), 
                  preProcess = c("center", "scale"), trControl = control),
      "bayesglm" = list(method = "bayesglm", trControl = control)
    )

    params <- model_params[[method]]
    if(is.null(params)) {
      stop(paste("Unsupported method:", method))
    }

    model <- do.call(caret::train, c(list(class ~ ., data = noisy_train_df), params))

    # Ensure the directory exists BEFORE saving
    dir.create(paste0("data/results/instances/train_noise/threshold_", noise_level), recursive = TRUE, showWarnings = FALSE)
    
    # Save the model
    saveRDS(model, file = paste0("data/results/instances/train_noise/threshold_", noise_level, "/", dataset, "_", method, "_noise", noise_level, ".rds"))
    
    return(model)
}

# Calculate predictions and generate confusion matrix
calculate_predictions <- function(fit, test_df, instances_df, noise) {
  cat("Calculate prediction\n")
  
  # Prediction without noise
  predict_unseen <- predict(fit, test_df)
  
  # Prediction with noise 
  noise_predict <- predict(fit, instances_df)
  
  # Confusion matrix
  cat("Confusion matrix\n")
  if(noise == 0) {
    conf_matrix <- caret::confusionMatrix(predict_unseen, predict_unseen)
  } else {
    conf_matrix <- caret::confusionMatrix(predict_unseen, noise_predict)
  }
  
  # Extract metrics from confusion matrix
  if(conf_matrix$overall["Kappa"] == "NaN") { 
    conf_matrix$overall["Kappa"] <- 1 
  }
  
  # Return the confusion matrix
  conf_matrix
}

# Instances-Noise
process_instance <- function(dataset, fold_index, method, mia, noise, percent, test_df, test_df_indices, fit, indices) {
  # Print relevant information for the iteration
  cat("Dataset:", dataset, "\n")
  cat("Fold:", fold_index, "\n")
  cat("Method:", method, "\n")
  cat("Noise level: ", noise * 100, "%\n")
  cat("Percentage of altered instances: ", percent * 100, "%\n")
  cat("Using", length(indices), "indices (first", length(indices), "from shuffled list)\n")

  # Load noise data from CSV file created by noise_injector.R
  noise_csv_file <- paste0("data/results/noise_injection/by_dataset/", dataset, "_", mia, "_noise", noise, ".csv")
  cat("Loading noise data from:", noise_csv_file, "\n")
  
  if(!file.exists(noise_csv_file)) {
    stop(paste("Noise CSV file not found:", noise_csv_file))
  }
  
  noise_full_df <- read.csv(noise_csv_file, stringsAsFactors = FALSE)
  
  # Remove metadata columns if present
  noise_full_df$dataset_name <- NULL
  noise_full_df$altered_attribute <- NULL
  noise_full_df$noise_level <- NULL
  
  # Convert class column to factor to match test_df
  noise_full_df$class <- as.factor(noise_full_df$class)
  
  # Select only the test set rows from the noisy dataset
  noise_df <- noise_full_df[test_df_indices, ]
  
  # Create a new df with the altered number of instances desired
  instances_df <- test_df
  
  # Convert to data.table and add indices 
  setDT(test_df)[, index := .I]
  setDT(noise_df)[, index := .I]
  setDT(instances_df)[, index := .I]
  
  cat("Using", length(indices), "indices for ", percent * 100, "% of instances\n")
  
  # Create auxiliary dataframe and manipulate rows efficiently with data.table
  aux_df <- noise_df[index %in% indices]
  
  # Remove and combine rows efficiently
  instances_df <- rbindlist(list(
    instances_df[!index %in% indices],
    aux_df
  ))
  
  # Reorder dataframe efficiently using data.table
  setorder(instances_df, index)
  
  # Remove index columns efficiently
  instances_df[, index := NULL]
  aux_df[, index := NULL]
  test_df[, index := NULL]
  noise_df[, index := NULL]
  
  # Calculate predictions and get confusion matrix
  conf_matrix <- calculate_predictions(fit, test_df, instances_df, noise)
  
  # Return results
  data.frame(
    dataset = dataset,
    fold = paste0("Fold_", fold_index),
    method = method,
    noise_levels = noise,
    instance_level = percent,
    accuracy = conf_matrix$overall["Accuracy"],
    kappa = conf_matrix$overall["Kappa"],
    stringsAsFactors = FALSE
  )
}

# Models
process_model <- function(dataset, fold_index, train_df, train_df_indices, test_df, test_df_indices, method, mia_df, noise_levels, instances, threshold_results, control) {
  # Get MIA
  mia <- as.character(subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute[1])

  # Load indices from original execution to ensure consistent train/test partitions
  indices_file <- paste0("data/results/instances/original/vectors/", 
                         dataset, "_", method, "_all_folds.csv")
  
  if(!file.exists(indices_file)) {
    stop(paste("Original indices file not found:", indices_file, "\nPlease run the original instances script first."))
  }
  
  # Load existing indices for this fold
  existing_data <- read.csv(indices_file, stringsAsFactors = FALSE)
  
  if(!fold_index %in% existing_data$fold) {
    stop(paste("Fold", fold_index, "not found in", indices_file))
  }
  
  # Load indices for this fold
  all_indices <- existing_data$index[existing_data$fold == fold_index]
  cat("Loaded", length(all_indices), "indices for fold", fold_index, "from:", indices_file, "\n")

  # Store results for all noise/instance level combinations
  method_results <- list()
  
  # Iterate through noise levels - train a separate model for each noise level
  for (noise in noise_levels) {
    cat("\n=== Training model with", noise * 100, "% noise in training data ===\n")
    
    # Train model with noise in training data based on critical_percentage
    fit <- train_model_noise(method, train_df, train_df_indices, dataset, noise, 
                             threshold_results, mia_df, control)
    
    # Test with varying percentages of noisy test instances
    for (percent in instances) {
      # Calculate sample size for this percentage
      sample_size <- round(nrow(test_df) * percent, 0)
      
      # Use subset of all_indices based on percentage
      indices_to_use <- all_indices[1:sample_size]
      
      result <- process_instance(dataset, fold_index, method, mia, noise, percent, 
                                test_df, test_df_indices, fit, indices_to_use)
      method_results <- append(method_results, list(result))
    }
  }
  do.call(rbind, method_results)
}

# Load and extract parameters
parameters <- load_parameters("data/parameters.csv")
datasets <- parameters$datasets
fold_names <- parameters$fold_names
models <- parameters$models 
noise_levels <- c(0.1, 0.2, 0.3)  # Only use these specific noise levels for training noise
instances <- parameters$instances
control <- parameters$control

# Set number of folds
n_folds <- 5

# Load most important attribute table
mia_df <- read.csv("data/results/most_important_attr/mia.csv", stringsAsFactors = FALSE)

# Load threshold instance results
threshold_results <- read.csv("data/results/threshold_instance_results.csv", stringsAsFactors = FALSE)

# Datasets
dataset <- datasets
filename <- paste0("data/datasets/", dataset, ".csv")
df <- read.csv(filename, stringsAsFactors = FALSE)

# Convert class column to factor for classification
df$class <- as.factor(df$class)

# Load train/test partitions from original execution
# We need to reconstruct the fold indices from the original vector files
# Read one of the vector files to get the original test indices for all folds
sample_vector_file <- paste0("data/results/instances/original/vectors/", dataset, "_", models, "_all_folds.csv")
if(!file.exists(sample_vector_file)) {
  stop(paste("Cannot find original vector file:", sample_vector_file, "\nPlease run the original instances script first."))
}

vector_data <- read.csv(sample_vector_file, stringsAsFactors = FALSE)

# Reconstruct fold_test_indices from the vector file
fold_test_indices <- lapply(1:n_folds, function(fold_i) {
  unique(vector_data$original_test_index[vector_data$fold == fold_i])
})

# Calculate fold_train_indices as complement of test indices
fold_train_indices <- lapply(fold_test_indices, function(test_idx) {
  setdiff(1:nrow(df), test_idx)
})

# Process each model separately and save results per dataset-model combination
for(model in models) {
  cat("Processing model:", model, "for dataset:", dataset, "\n")
  
  # Call functions (iterate folds) for this specific model
  model_results <- do.call(rbind, lapply(1:n_folds, function(fold_i) {
    process_model(dataset, fold_i, 
                  df[fold_train_indices[[fold_i]], ],
                  fold_train_indices[[fold_i]],
                  df[fold_test_indices[[fold_i]], ], 
                  fold_test_indices[[fold_i]], 
                  model, mia_df, noise_levels, instances, threshold_results, control)
  }))
  
  # Save results per dataset-model combination
  out_filename <- paste0("data/results/instances/train_noise/by_dataset/", dataset, "_", model, "_results.csv")
  write.csv(model_results, file = out_filename, row.names = FALSE)
  cat("Saved results to:", out_filename, "\n")
  cat("----------------\n")
}

cat("****************************\n")
cat("Instances altered with noise\n")
cat("****************************\n")