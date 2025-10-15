# /usr/bin/env/Rscript

# Packages that need to be loaded
pacman::p_load(caret, iml, citation, dplyr, earth, lime, data.table)

# Set the seed 
set.seed(1)

# Load parameters
load_parameters <- function(params_file) {
  # Datasets from command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  datasets <- args
  # Parameters
  params <- read.csv(params_file, stringsAsFactors = FALSE)
  # Control parameters
  control = trainControl(method = "none", number = 1)
  list(
    datasets = datasets,
    #datasets = unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|")),
    fold_names = unlist(strsplit(subset(params, parameter == "fold_name")$values, "\\|")),
    models = unlist(strsplit(subset(params, parameter == "technique_name")$values, "\\|")),
    noise_levels = as.numeric(unlist(strsplit(subset(params, parameter == "noise_level")$values, "\\|"))),
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

# Model training function with noise from thresholds
train_model_noise <- function(method, train_df, control, dataset_name, mia_df, noiseMIA_list) {
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

  # Lookup threshold values for this dataset and method
  row <- thresholds_df[thresholds_df$dataset_name == dataset_name & thresholds_df$technique == method, ]
  if(nrow(row) == 0) {
    stop(paste("No threshold found for dataset:", dataset_name, "and method:", method))
  }
  noise_level <- as.numeric(row$noise_level[1])
  critical_percentage <- as.numeric(row$critical_percentage[1])

  # Get most important attribute for this dataset and method
  mia_row <- subset(mia_df, dataset_name == dataset_name & technique == method)
  if(nrow(mia_row) == 0) {
    stop(paste("No MIA found for dataset:", dataset_name, "and method:", method))
  }
  mia <- as.character(mia_row$most_important_attribute[1])

  # Apply noise to the training data according to the thresholds
  train_df_noisy <- train_df
  if(noise_level > 0 && critical_percentage > 0) {
    noiselvl <- paste0("noise_", as.integer(noise_level))
    # Get the noise data for the training set
    noise_df <- noiseMIA_list[[dataset_name]][[mia]][[noiselvl]]
    # Add index columns for sampling
    train_df_noisy <- as.data.table(train_df_noisy)
    noise_df <- as.data.table(noise_df)
    train_df_noisy[, index := .I]
    noise_df[, index := .I]
    # Number of instances to alter
    sample_size <- round(nrow(train_df_noisy) * (critical_percentage / 100), 0)
    if(sample_size > 0) {
      indices <- sample(train_df_noisy$index, sample_size)
      # Replace selected rows with noisy rows
      aux_df <- noise_df[index %in% indices]
      if (nrow(aux_df) == 0) {
        train_df_noisy[, index := NULL]
      } else {
        # Ensure aux_df has the same columns as train_df_noisy
        aux_df <- aux_df[, names(train_df_noisy), with=FALSE]
        train_df_noisy <- rbindlist(list(
          train_df_noisy[!index %in% indices],
          aux_df
        ))
        setorder(train_df_noisy, index)
        train_df_noisy[, index := NULL]
      }
    } else {
      train_df_noisy[, index := NULL]
    }
  }

  do.call(caret::train, c(list(class ~ ., data = train_df_noisy), params))
}

# Obtain vector of altered instances
get_instances <- function(instances_df, index_list, index_counter, percent) {
  
  # Number of values to be altered (instances)
  sample_size = round(nrow(instances_df) * percent, 0)

  # Sample of ids that we want from the test df
  indices <- sample(instances_df[, index], sample_size)
  
  # Store vector of indices in list
  index_list[[index_counter]] <- indices
  index_counter = index_counter + 1
  
  # Return updated list and counter
  return(list(
    index_list = index_list,
    index_counter = index_counter,
    indices = indices
  ))
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
process_instance <- function(dataset, fold_index, method, mia, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter) {
  # Print relevant information for the iteration
  cat("Dataset:", dataset, "\n")
  cat("Fold:", fold_index, "\n")
  cat("Method:", method, "\n")
  cat("Noise level: ", noise * 100, "%\n")
  cat("Percentage of altered instances: ", percent * 100, "%\n")

  # Create a new dataframe with the noise from noiseMIA_list
  noiselvl <- paste0("noise_", noise * 100)
  noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
  
  # Create a new df with the altered number of instances desired
  instances_df <- test_df
  
  # Convert to data.table and add indices 
  setDT(test_df)[, index := .I]
  setDT(noise_df)[, index := .I]
  setDT(instances_df)[, index := .I]
  
  # Get instances indices only once if semaphore is TRUE
  indices <- NULL
  if(semaphore) {
    result <- get_instances(instances_df, index_list, index_counter, percent)
    index_list <- result$index_list
    index_counter <- result$index_counter
    indices <- result$indices
    semaphore <- FALSE
  }

  # Number of values to be altered (instances)
  sample_size <- round(nrow(instances_df) * percent, 0)
  
  # Sample of ids that we want from the test df
  indices <- tail(index_list, n = 1)
  indices <- indices[[1]][1:sample_size]
  
  cat("Alter ", percent * 100, "% of instances with noise\n")
  
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
  
  # Return results and updated control variables
  list(
    result = data.frame(
      dataset = dataset,
      fold = paste0("Fold_", fold_index),
      method = method,
      noise_levels = noise,
      instance_level = percent,
      accuracy = conf_matrix$overall["Accuracy"],
      kappa = conf_matrix$overall["Kappa"],
      stringsAsFactors = FALSE
    ),
    index_list = index_list,
    index_counter = index_counter,
    semaphore = semaphore
  )
}

# Models

process_model <- function(dataset, fold_index, train_df, test_df, method, mia_df, noise_levels, instances, noiseMIA_list, index_list, index_counter, control) {
  # Only use noise/instance levels present in threshold_instance_results.csv for this dataset/method
  rows <- thresholds_df[thresholds_df$dataset_name == dataset & thresholds_df$technique == method, ]
  if (nrow(rows) == 0) {
    stop(paste("No threshold rows for dataset:", dataset, "and method:", method))
  }
  # Train model with noise thresholds (first row, as before)
  fit <- train_model_noise(method, train_df, control, dataset, mia_df, noiseMIA_list)

  # Get MIA
  mia <- as.character(subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute[1])

  # Control variable for sampling
  semaphore <- TRUE

  # Store results only for the noise/instance levels in the CSV
  method_results <- list()
  for (i in seq_len(nrow(rows))) {
    noise <- as.numeric(rows$noise_level[i]) / 100
    percent <- as.numeric(rows$critical_percentage[i]) / 100
    noiselvl <- paste0("noise_", as.integer(noise * 100))
    noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
    result <- process_instance(dataset, fold_index, method, mia, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter)
    index_list <- result$index_list
    index_counter <- result$index_counter
    semaphore <- result$semaphore
    method_results <- append(method_results, list(result$result))
  }
  do.call(rbind, method_results)
}

# Folds
process_fold <- function(dataset, fold_index, train_indices, test_indices, df, models, mia_df, noise_levels, instances, noiseMIA_list, index_list, index_counter, control) {

  # Print probability table for the fold
  #cat("Dataset:", dataset, "\n")
  #cat("Probability table:\n")
  #prob_table <- prop.table(table(train_df$class))
  #cat(paste(capture.output(prob_table), collapse="\n"), "\n")

  # Get train and test indices for this fold
  train_df <- df[train_indices, ]
  test_df <- df[test_indices, ]

  # Call function to iterate models
  fold_results <- do.call(rbind, lapply(models, function(method) {
    process_model(dataset, fold_index, train_df, test_df, method, mia_df, noise_levels, instances, noiseMIA_list, index_list, index_counter, control)
  }))
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

# Load previous most important attribute table and
mia_df <- readRDS("data/prev/mia_df.rds") # TODO store as csv
noiseMIA_list <- readRDS("data/prev/noise_list.rds") #TODO flatten list and store as csv

# Read threshold_instance_results.csv once at the top
thresholds_df <- read.csv("results/threshold_instance_results.csv", stringsAsFactors = FALSE)

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

# Control variables before iterations
index_list <- list()
index_counter <- 1

# Iterate datasets
results_list <- lapply(datasets, function(dataset) {
  filename <- paste0("data/datasets/", dataset, ".rds")
  df <- readRDS(filename)

  # Get train and test indices for this dataset
  # Partition data into train/test with cross validation folds
  fold_train_indices <- createFolds(df$class, k = 5, list = TRUE, returnTrain = TRUE)
  fold_test_indices <- lapply(fold_train_indices, function(index) setdiff(1:nrow(df), index))

  # Call functions (iterate folds)
  dataset_results <- do.call(rbind, lapply(1:n_folds, function(fold_i) {
    process_fold(dataset, fold_i, fold_train_indices[[fold_i]], fold_test_indices[[fold_i]], df, models, mia_df, noise_levels, instances, noiseMIA_list, index_list, index_counter, control)
  }))

  # Safeguard store by dataset
  out_filename <- paste0("results/instances/with_noise/", dataset, "_results.csv")
  write.csv(dataset_results, file = out_filename, row.names = FALSE)
  cat("Altered instances with noise recorded\n")
  cat("----------------\n")
  dataset_results
})
# Combine all results into a single data frame
#results_df <- do.call(rbind, results_list)
#out_filename <- paste0("results/altered_instances_results.csv")
#write.csv(results_df, file = out_filename, row.names = FALSE)

cat("****************\n")
cat("RESULTS RECORDED\n")
cat("****************\n")