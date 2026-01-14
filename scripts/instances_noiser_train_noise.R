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
cat("parameters.csv exists:", file.exists("data/files/parameters.csv"), "\n")
cat("parameters.csv absolute path:", normalizePath("data/files/parameters.csv", mustWork=FALSE), "\n")

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
 
# Model training function (instead of if-else block)
train_model <- function(method, train_df, control) {
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
  
  do.call(caret::train, c(list(class ~ ., data = train_df), params))
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
process_instance <- function(dataset, fold_index, method, mia, noise, percent, test_df, test_df_indices, noise_df, fit, indices) {
  # Print relevant information for the iteration
  cat("Dataset:", dataset, "\n")
  cat("Fold:", fold_index, "\n")
  cat("Method:", method, "\n")
  cat("Noise level: ", noise * 100, "%\n")
  cat("Percentage of altered instances: ", percent * 100, "%\n")
  cat("Using", length(indices), "indices (first", length(indices), "from shuffled list)\n")

  # Load noise data from CSV file created by noise_injector.R
  noise_csv_file <- paste0("results/noise_injection/by_dataset/", dataset, "_", mia, "_noise", noise, ".csv")
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
process_model <- function(dataset, fold_index, train_df, test_df, test_df_indices, method, mia_df, noise_levels, instances, control) {
  # Train model on clean data
  cat("Training model on clean data\n")
  fit <- train_model(method, train_df, control)

  # Get MIA
  mia <- as.character(subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute[1])

  # Generate or load indices for this (dataset, fold, method) combination
  indices_file <- paste0("results/instances/original/vectors/", 
                         dataset, "_", method, "_all_folds.csv")
  
  if(!file.exists(indices_file)) {
    # First time: sample ALL test indices and save with fold info
    all_indices <- sample(1:nrow(test_df))
    indices_data <- data.frame(
      fold = fold_index,
      index = all_indices, 
      original_test_index = test_df_indices[all_indices]
    )
    write.csv(indices_data, file = indices_file, row.names = FALSE)
    cat("Generated and saved", length(all_indices), "indices for fold", fold_index, "to:", indices_file, "\n")
  } else {
    # Load existing indices and check if this fold exists
    existing_data <- read.csv(indices_file, stringsAsFactors = FALSE)
    
    if(fold_index %in% existing_data$fold) {
      # Load indices for this fold
      all_indices <- existing_data$index[existing_data$fold == fold_index]
      cat("Loaded", length(all_indices), "indices for fold", fold_index, "from:", indices_file, "\n")
    } else {
      # Generate new indices for this fold and append
      all_indices <- sample(1:nrow(test_df))
      new_data <- data.frame(
        fold = fold_index,
        index = all_indices, 
        original_test_index = test_df_indices[all_indices]
      )
      combined_data <- rbind(existing_data, new_data)
      write.csv(combined_data, file = indices_file, row.names = FALSE)
      cat("Generated and appended", length(all_indices), "indices for fold", fold_index, "to:", indices_file, "\n")
    }
  }

  # Store results for all noise/instance level combinations
  method_results <- list()
  for (noise in noise_levels) {
    for (percent in instances) {
      # Calculate sample size for this percentage
      sample_size <- round(nrow(test_df) * percent, 0)
      
      # Use subset of all_indices based on percentage
      indices_to_use <- all_indices[1:sample_size]
      
      noise_df <- NULL  # Will be loaded in process_instance
      result <- process_instance(dataset, fold_index, method, mia, noise, percent, 
                                test_df, test_df_indices, noise_df, fit, indices_to_use)
      method_results <- append(method_results, list(result))
    }
  }
  do.call(rbind, method_results)
}

# Folds
process_fold <- function(dataset, fold_index, train_df, test_df, test_indices, model, mia_df, noise_levels, instances, control) {
  # Process single model for this fold
  fold_results <- process_model(dataset, fold_index, train_df, test_df, test_indices, model, mia_df, noise_levels, instances, control)
  fold_results
}

# Load and extract parameters
parameters <- load_parameters("data/files/parameters.csv")
datasets <- parameters$datasets
fold_names <- parameters$fold_names
models <- parameters$models 
noise_levels <- parameters$noise_levels 
instances <- parameters$instances
control <- parameters$control

# Set number of folds
n_folds <- 5

# Load most important attribute table
mia_df <- read.csv("results/most_important_attr/mia.csv", stringsAsFactors = FALSE)

# Initialize empty results dataframe
results_df <- data.frame(
  dataset = character(),
  fold = character(),
  method = character(),
  noise_level = numeric(),
  instance_level = numeric(),
  accuracy = numeric(),
  kappa = numeric(),
  stringsAsFactors = FALSE
)

# Datasets
dataset <- datasets
filename <- paste0("data/datasets/", dataset, ".csv")
df <- read.csv(filename, stringsAsFactors = FALSE)

# Convert class column to factor for classification
df$class <- as.factor(df$class)

# Partition data into train/test with cross validation folds
fold_train_indices <- createFolds(df$class, k = 5, list = TRUE, returnTrain = TRUE)
fold_test_indices <- lapply(fold_train_indices, function(index) setdiff(1:nrow(df), index))

# Process each model separately and save results per dataset-model combination
for(model in models) {
  cat("Processing model:", model, "for dataset:", dataset, "\n")
  
  # Call functions (iterate folds) for this specific model
  model_results <- do.call(rbind, lapply(1:n_folds, function(fold_i) {
    process_model(dataset, fold_i, 
                  df[fold_train_indices[[fold_i]], ], 
                  df[fold_test_indices[[fold_i]], ], 
                  fold_test_indices[[fold_i]], 
                  model, mia_df, noise_levels, instances, control)
  }))
  
  # Save results per dataset-model combination
  out_filename <- paste0("results/instances/original/by_dataset/", dataset, "_", model, "_results.csv")
  write.csv(model_results, file = out_filename, row.names = FALSE)
  cat("Saved results to:", out_filename, "\n")
  cat("----------------\n")
}

cat("****************************\n")
cat("Instances altered with noise\n")
cat("****************************\n")