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
pacman::p_load(caret, iml, citation, dplyr, earth, lime)

# Set the seed to make the experiment reproducible
set.seed(1)

# Load parameters
load_parameters <- function(params_file) {
  # Datasets from command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # If no arguments provided, read from parameters.csv
  if(length(args) == 0) {
    params <- read.csv(params_file, stringsAsFactors = FALSE)
    datasets <- unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|"))
  } else {
    # Use command line arguments as datasets
    datasets <- args
    params <- read.csv(params_file, stringsAsFactors = FALSE)
  }
  
  # Load models from parameters file
  models <- unlist(strsplit(subset(params, parameter == "technique_name")$values, "\\|"))
  
  # Control parameters
  control <- trainControl(method = "none", number = 1)
  
  list(
    datasets = datasets,
    models = models,
    control = control
  )
}

# Model training function (instead of if-else block)
train_model <- function(method, df, control) {
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
                trControl = control),
    "bayesglm" = list(method = "bayesglm", trControl = control)
  )
  
  params <- model_params[[method]]
  if(is.null(params)) {
    stop(paste("Unsupported method:", method))
  }
  
  do.call(caret::train, c(list(class ~ ., data = df), params))
}

# Calculate feature importance
calculate_feature_importance <- function(fit) {
  featImp <- FeatureImp$new(Predictor$new(fit), loss = "ce")
  mia <- featImp$results[which.max(featImp$results$importance),'feature']
  mia
}

# Models
process_method <- function(dataset, method, df, control) {
  cat("Dataset:", dataset, "\n")
  cat("Technique:", method, "\n")
  cat("Probability table:\n")
  print(prop.table(table(df$class)))
  
  cat("Begin training\n")
  fit <- train_model(method, df, control)
  cat("Model trained\n")
  
  # Calculate feature importance
  mia <- calculate_feature_importance(fit)
  cat(paste0("The most important attribute: ", mia, " (for technique: ", method, ")\n"))
  cat("________________\n")
  
  data.frame(
    dataset_name = dataset,
    technique = method,
    most_important_attribute = mia,
    stringsAsFactors = FALSE
  )
}

# Datasets
process_dataset <- function(dataset, models, control) {
  # Load dataset
  ds_filename <- paste0("data/datasets/", dataset, ".csv")
  df <- read.csv(ds_filename, stringsAsFactors = FALSE)
  
  # Scale all numeric features for consistent preprocessing across all models
  df_processed <- df %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
  
  # Process all methods for this dataset
  dataset_results <- do.call(rbind, lapply(models, function(method) {
    process_method(dataset, method, df_processed, control)
  }))
  
  # Save most important attribute results by dataset
  filename <- paste0("results/most_important_attr/by_dataset/", dataset, "_mia.csv")
  write.csv(dataset_results, file = filename, row.names = FALSE)
  
  # Save unique most important attribute results by dataset
  dataset_results_unique <- distinct(dataset_results, dataset_name, most_important_attribute)
  filename_unique <- paste0("results/most_important_attr/by_dataset/", dataset, "_miaUnique.csv")
  write.csv(dataset_results_unique, file = filename_unique, row.names = FALSE)
  
  cat("Most important attribute obtained with all models\n")
  cat("----------------\n")
  
  dataset_results
}

# Load and extract parameters
parameters <- load_parameters("data/files/parameters.csv")
datasets <- parameters$datasets
models <- parameters$models
control <- parameters$control

# Iterate datasets
results_list <- lapply(datasets, function(dataset) {
  process_dataset(dataset, models, control)
})

cat("**********************************\n")
cat("Most important attributes obtained\n")
cat("**********************************\n")