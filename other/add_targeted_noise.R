#!/usr/bin/env Rscript

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # for data manipulation and visualization
  dplyr,       # for data manipulation
  tidyr,       # for data reshaping
  ggplot2,     # for visualization
  here         # for file path handling
)

# Set the seed for reproducibility
set.seed(1)

# Load threshold data
threshold_results <- read.csv(here("results", "threshold_instance_results.csv"))

# Load dataset names, techniques, and instances levels
dataset_names <- readRDS(here("data", "files", "dataset_name.rds"))
techniques <- readRDS(here("data", "files", "technique_name.rds"))
folds <- readRDS(here("data", "files", "fold_name.rds"))
noise_level <- c(0.1, 0.2, 0.3)

# Get the instances levels from the threshold results

# Per dataset
for(dataset in datasets) {
  
  # Load dataset
  filename <- here("data", "datasets",  dataset, ".rds")
  df <- readRDS(filename)
  
  # Create fold indices, generating the training folds and the testing folds
  fold_train_indices <- createFolds(df$class, k = 5, list = TRUE, returnTrain = TRUE)
  fold_test_indices <- lapply(fold_train_indices, function(index) setdiff(1:nrow(df), index))
  
  # Create a list to store the attribute lists per dataframe
  folds_list <- list()
  folds_CM <- list()
  folds_counter = 1
  
  # Loop through the folds and create a dataframe for training
  for (i in 1:length(fold_train_indices)) {
    
    # Get the training indices for the current fold
    train_indices <- fold_train_indices[[i]]
    test_indices <- fold_test_indices[[i]]
    
    # Create the corresponding dataframes using the current fold
    train_df <- df[train_indices, ]
    test_df <- df[test_indices, ]
    
    # Create a frequency table for the class in the 'Training' dataset
    # Calculate the frequency table as a proportion of all values
    print(paste("Dataset:", dataset))
    print("Probability table:")
    print(prop.table(table(train_df$class)))
    
    # Create a list to store the noise lists per method
    method_list <- list()
    method_CM <- list()
    method_counter = 1
    if(FALSE) {
      # Train for each method selected
      for(method in methods) {
        # Train to obtain model
        print(paste("Dataset:", dataset))
        print(paste("Method:", method))
        print("BEGINNING TRAINING")
        if(method == "C5.0"){
          fit = caret::train(class ~ ., data = train_df, method = "C5.0")
        }else if(method == "ctree"){
          fit = caret::train(class~., data = train_df, method = "ctree")
        }else if(method == "fda"){
          fit = caret::train(class~., data = train_df, method = "fda")
        }else if(method == "gbm"){
          fit = caret::train(class~., data = train_df, method = "gbm")
        }else if(method == "gcvEarth"){
          fit = caret::train(class ~ ., data = train_df, method = "gcvEarth")
        }else if(method == "JRip"){
          fit = caret::train(class~., data = train_df, method = "JRip")
        }else if(method == "lvq"){
          fit = caret::train(class~., data = train_df, method = "lvq")
        }else if(method == "mlpML"){
          fit = caret::train(class ~ ., data = train_df, method = "mlpML")
        }else if(method == "multinom"){
          fit = caret::train(class ~ ., data = train_df, method = "multinom", trControl = control, tuneGrid = expand.grid(decay = c(0)), MaxNWts = 10000)
        }else if(method == "naive_bayes"){
          fit = caret::train(class ~ ., data = train_df, method = "naive_bayes")
        }else if(method == "PART"){
          fit = caret::train(class ~ ., data = train_df, method = "PART")
        }else if(method == "rbfDDA"){
          fit = caret::train(class ~ ., data = train_df, method = "rbfDDA", trControl = control)
        }else if(method == "rda"){
          fit = caret::train(class~., data = train_df, method = "rda")
        }else if(method == "rf"){
          fit = caret::train(class ~ ., data = train_df, method = "rf")
        }else if(method == "rpart"){
          fit = caret::train(class ~ ., data = train_df, method = "rpart")
        }else if(method == "simpls"){
          fit = caret::train(class ~ ., data = train_df, method = "simpls")
        }else if(method == "svmLinear"){
          fit = caret::train(class ~ ., data = train_df, method = "svmLinear")
        }else if(method == "svmRadial"){
          fit = caret::train(class ~ ., data = train_df, method = "svmRadial")
        }else if(method == "rfRules"){
          fit = caret::train(class ~ ., data = train_df, method = "rfRules")
        }
        #print(fit)
        print("TRAINING SUCCESSFULL")
        
        # Select the MIA we will get the noise from
        mia <- subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute
        
        # Create a list to store the % instance lists per noise level
        noise_list <- list()
        noise_CM <- list()
        noise_counter = 1
        
        # Control variable to make sure sampling only happens on the first iteration
        semaphore = TRUE
        
        for(noise in noise_level){
          
          # Create a list to store the dataframe with instances per % of instances
          instances_list <- list()
          confMatrix_list <- list()
          instances_counter = 1
          
          for(percent in instances) {
            
            # Print relevant information for the iteration
            print(paste("Dataset:", dataset))
            print(paste("Fold:", i))
            print(paste("Method:", method))
            print(paste0("Noise level: ", noise * 100, "%"))
            print(paste0("Percentage of altered instances: ", percent * 100, "%"))
            
            # Create a new dataframe with the noise from noiseMIA_list
            noiselvl <- paste0("noise_", noise * 100)
            noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
            #noise_df <- noiseMIA_list[[mia]][[noiselvl]]
            
            # Create a new df with the altered number of instances desired
            instances_df <- test_df
            
            # Add rownames to manipulate dataframes
            test_df <- cbind(index = rownames(test_df), test_df)
            noise_df <- cbind(index = rownames(noise_df), noise_df)
            instances_df <- cbind(index = rownames(instances_df), instances_df)
            
            # Determine the indices list to alter the same instances consistently
            if(semaphore) {
              print("Obtain vector of altered instances")
              
              # Number of values to be altered (instances)
              sample_size = round(nrow(instances_df) * 1, 0)
              
              # Sample of ids that we want from the test df
              indices <- instances_df$index[sample(nrow(instances_df), sample_size)]
              
              # Store vector of indices in list
              index_list[[index_counter]] <- indices
              
              # Advance index counter
              index_counter = index_counter + 1
              
              # Set control variable to 0
              semaphore = FALSE
            }
            
            print(paste0("Alter ", percent * 100, "% of instances with noise"))
            
            # Number of values to be altered (instances)
            sample_size = round(nrow(instances_df) * percent, 0)
            
            # Sample of ids that we want from the test df
            indices <- tail(index_list, n = 1)
            indices <- indices[[1]][1:sample_size]
            
            # Create auxiliary dataframe to extract rows from and clean NAs
            aux_df <- noise_df[indices,] 
            
            # Eliminate corresponding rows
            instances_df <- instances_df[!(instances_df$index %in% indices),]
            
            # Insert noised sample into clean dataframe
            instances_df <- rbind(instances_df, aux_df)
            
            # Reorder dataframe
            instances_df$index = as.numeric(as.character(instances_df$index))
            sapply(instances_df, class)
            instances_df = instances_df %>% arrange(index)
            rownames(instances_df) <- instances_df[,1]
            
            # Eliminate auxiliary index column
            instances_df <- instances_df %>% select(-index)
            aux_df <- aux_df %>% select(-index)
            test_df <- test_df %>% select(-index)
            noise_df <- noise_df %>% select(-index)
            
            # Prediction
            print("Calculate prediction")
            
            # Prediction without noise
            predict_unseen = predict(fit, test_df)
            predict_unseen
            
            # Prediction with noise 
            noise_predict = predict(fit, instances_df)
            noise_predict
            
            # Confusion matrix
            print("Confusion matrix")
            if(noise == 0) {
              conf_matrix = caret::confusionMatrix(predict_unseen, predict_unseen)
              #print(conf_matrix)
            }else {
              conf_matrix = caret::confusionMatrix(predict_unseen, noise_predict)
              #print(conf_matrix)
            }
            
            # Store df with noise in list
            instances_list[[instances_counter]] <- instances_df
            
            # Store Accuracy and Kappa results for % instances from the confusion matrix
            if(conf_matrix$overall["Kappa"] == "NaN"){ conf_matrix$overall["Kappa"] = 1 }
            #instances_AK[[instances_counter]] <- list(Accuracy = conf_matrix$overall["Accuracy"], Kappa = conf_matrix$overall["Kappa"])
            confMatrix_list[[instances_counter]] <- conf_matrix
            
            # Advance instances counter
            instances_counter = instances_counter + 1
            
            print("-")
          }
          
          # Alter names to make it easily readable
          names(instances_list) <- instances_names
          names(confMatrix_list) <- instances_names
          
          # Store % list with noise in list
          noise_list[[noise_counter]] <- instances_list
          noise_CM[[noise_counter]] <- confMatrix_list
          
          # Advance noise counter
          noise_counter = noise_counter + 1
        }
        
        # Alter names to make it easily readable
        names(noise_list) <- noise_names
        names(noise_CM) <- noise_names
        
        # Store noise list in method
        method_list[[method_counter]] <- noise_list
        method_CM[[method_counter]] <- noise_CM
        
        # Advance method counter
        method_counter = method_counter + 1
      }
    }

    # Standardize DF for KNN and GLM
    #train_df =  train_df %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
    #test_df =  test_df %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
    
    #########
    ## KNN ##
    #########
    if(TRUE){
      method = "knn"
      
      # Train to obtain model
      print(paste("Dataset:", dataset))
      print(paste("Method:", method))
      print("BEGIN TRAINING")
      fit = caret::train(class ~ ., data = df, method="knn", tuneGrid = expand.grid(k = 5:5), preProcess = c("center", "scale"), trControl = control)
      print("TRAINING SUCCESSFULL")
      
      # Select the MIA we will get the noise from
      mia <- subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute

      # Create a list to store the % instance lists per noise level
      noise_list <- list()
      noise_CM <- list()
      noise_counter = 1
      
      # Control variable to make sure sampling only happens on the first iteration
      semaphore = TRUE
      
      for(noise in noise_level){
        
        # Create a list to store the dataframe with instances per % of instances
        instances_list <- list()
        confMatrix_list <- list()
        instances_counter = 1
        
        for(percent in instances) {
          
          # Print relevant information for the iteration
          print(paste("Dataset:", dataset))
          print(paste("Fold:", i))
          print(paste("Method:", method))
          print(paste0("Noise level: ", noise * 100, "%"))
          print(paste0("Percentage of altered instances: ", percent * 100, "%"))
          
          # Create a new dataframe with the noise from noiseMIA_list
          noiselvl <- paste0("noise_", noise * 100)
          noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
          #noise_df <- noiseMIA_list[[mia]][[noiselvl]]
          
          # Create a new df with the altered number of instances desired
          instances_df <- test_df
          
          # Add rownames to manipulate dataframes
          test_df <- cbind(index = rownames(test_df), test_df)
          noise_df <- cbind(index = rownames(noise_df), noise_df)
          instances_df <- cbind(index = rownames(instances_df), instances_df)
          
          # Determine the indices list to alter the same instances consistently
          if(semaphore) {
            print("Obtain vector of altered instances")
            
            # Number of values to be altered (instances)
            sample_size = round(nrow(instances_df) * 1, 0)
            
            # Sample of ids that we want from the test df
            indices <- instances_df$index[sample(nrow(instances_df), sample_size)]
            
            # Store vector of indices in list
            index_list[[index_counter]] <- indices
            
            # Advance index counter
            index_counter = index_counter + 1
            
            # Set control variable to 0
            semaphore = FALSE
          }
          
          print(paste0("Alter ", percent * 100, "% of instances with noise"))
          
          # Number of values to be altered (instances)
          sample_size = round(nrow(instances_df) * percent, 0)
          
          # Sample of ids that we want from the test df
          indices <- tail(index_list, n = 1)
          indices <- indices[[1]][1:sample_size]
          
          # Create auxiliary dataframe to extract rows from and clean NAs
          aux_df <- noise_df[indices,] 
          
          # Eliminate corresponding rows
          instances_df <- instances_df[!(instances_df$index %in% indices),]
          
          # Insert noised sample into clean dataframe
          instances_df <- rbind(instances_df, aux_df)
          
          # Reorder dataframe
          instances_df$index = as.numeric(as.character(instances_df$index))
          sapply(instances_df, class)
          instances_df = instances_df %>% arrange(index)
          rownames(instances_df) <- instances_df[,1]
          
          # Eliminate auxiliary index column
          instances_df <- instances_df %>% select(-index)
          aux_df <- aux_df %>% select(-index)
          test_df <- test_df %>% select(-index)
          noise_df <- noise_df %>% select(-index)
          
          # Prediction
          print("Calculate prediction")
          
          # Prediction without noise
          predict_unseen = predict(fit, test_df)
          predict_unseen
          
          # Prediction with noise 
          noise_predict = predict(fit, instances_df)
          noise_predict
          
          # Confusion matrix
          print("Confusion matrix")
          if(noise == 0) {
            conf_matrix = caret::confusionMatrix(predict_unseen, predict_unseen)
            #print(conf_matrix)
          }else {
            conf_matrix = caret::confusionMatrix(predict_unseen, noise_predict)
            #print(conf_matrix)
          }
          
          # Store df with noise in list
          instances_list[[instances_counter]] <- instances_df
          
          # Store Accuracy and Kappa results for % instances from the confusion matrix
          if(conf_matrix$overall["Kappa"] == "NaN"){ conf_matrix$overall["Kappa"] = 1 }
          #instances_AK[[instances_counter]] <- list(Accuracy = conf_matrix$overall["Accuracy"], Kappa = conf_matrix$overall["Kappa"])
          confMatrix_list[[instances_counter]] <- conf_matrix
          
          # Advance instances counter
          instances_counter = instances_counter + 1
          
          print("-")
        }
        
        # Alter names to make it easily readable
        names(instances_list) <- instances_names
        names(confMatrix_list) <- instances_names
        
        # Store % list with noise in list
        noise_list[[noise_counter]] <- instances_list
        noise_CM[[noise_counter]] <- confMatrix_list
        
        # Advance noise counter
        noise_counter = noise_counter + 1
      }
      
      # Alter names to make it easily readable
      names(noise_list) <- noise_names
      names(noise_CM) <- noise_names
      
      # Store noise list in method
      method_list[[method_counter]] <- noise_list
      method_CM[[method_counter]] <- noise_CM
      
      # Advance method counter
      method_counter = method_counter + 1
    
    }
    #########
    ## GLM ##
    #########
    if(FALSE){
      method = "bayesglm"
      
      # Train to obtain model
      print(paste("Dataset:", dataset))
      print(paste("Method:", method))
      print("BEGINNING TRAINING")
      fit = caret::train(class ~ ., data = train_df, method="bayesglm", trControl = control)
      #print(fit)
      print("TRAINING SUCCESSFULL")
      
      # Select the MIA we will get the noise from 
      mia <- subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute
      #mia <- mia_df
    
      # Create a list to store the % instance lists per noise level
      noise_list <- list()
      noise_CM <- list()
      noise_counter = 1
      
      # Control variable to make sure sampling only happens on the first iteration
      semaphore = TRUE
      
      for(noise in noise_level){
        
        # Create a list to store the dataframe with instances per % of instances
        instances_list <- list()
        confMatrix_list <- list()
        instances_counter = 1
        
        for(percent in instances) {
          
          # Print relevant information for the iteration
          print(paste("Dataset:", dataset))
          print(paste("Fold:", i))
          print(paste("Method:", method))
          print(paste0("Noise level: ", noise * 100, "%"))
          print(paste0("Percentage of altered instances: ", percent * 100, "%"))
          
          # Create a new dataframe with the noise from noiseMIA_list
          noiselvl <- paste0("noise_", noise * 100)
          noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
          #noise_df <- noiseMIA_list[[mia]][[noiselvl]]
          
          # Create a new df with the altered number of instances desired
          instances_df <- test_df
          
          # Add rownames to manipulate dataframes
          test_df <- cbind(index = rownames(test_df), test_df)
          noise_df <- cbind(index = rownames(noise_df), noise_df)
          instances_df <- cbind(index = rownames(instances_df), instances_df)
          
          # Determine the indices list to alter the same instances consistently
          if(semaphore) {
            print("Obtain vector of altered instances")
            
            # Number of values to be altered (instances)
            sample_size = round(nrow(instances_df) * 1, 0)
            
            # Sample of ids that we want from the test df
            indices <- instances_df$index[sample(nrow(instances_df), sample_size)]
            
            # Store vector of indices in list
            index_list[[index_counter]] <- indices
            
            # Advance index counter
            index_counter = index_counter + 1
            
            # Set control variable to 0
            semaphore = FALSE
          }
          
          print(paste0("Alter ", percent * 100, "% of instances with noise"))
          
          # Number of values to be altered (instances)
          sample_size = round(nrow(instances_df) * percent, 0)
          
          # Sample of ids that we want from the test df
          indices <- tail(index_list, n = 1)
          indices <- indices[[1]][1:sample_size]
          
          # Create auxiliary dataframe to extract rows from and clean NAs
          aux_df <- noise_df[indices,] 
          
          # Eliminate corresponding rows
          instances_df <- instances_df[!(instances_df$index %in% indices),]
          
          # Insert noised sample into clean dataframe
          instances_df <- rbind(instances_df, aux_df)
          
          # Reorder dataframe
          instances_df$index = as.numeric(as.character(instances_df$index))
          sapply(instances_df, class)
          instances_df = instances_df %>% arrange(index)
          rownames(instances_df) <- instances_df[,1]
          
          # Eliminate auxiliary index column
          instances_df <- instances_df %>% select(-index)
          aux_df <- aux_df %>% select(-index)
          test_df <- test_df %>% select(-index)
          noise_df <- noise_df %>% select(-index)
          
          # Prediction
          print("Calculate prediction")
          
          # Prediction without noise
          predict_unseen = predict(fit, test_df)
          predict_unseen
          
          # Prediction with noise 
          noise_predict = predict(fit, instances_df)
          noise_predict
          
          # Confusion matrix
          print("Confusion matrix")
          if(noise == 0) {
            conf_matrix = caret::confusionMatrix(predict_unseen, predict_unseen)
            #print(conf_matrix)
          }else {
            conf_matrix = caret::confusionMatrix(predict_unseen, noise_predict)
            #print(conf_matrix)
          }
          
          # Store df with noise in list
          instances_list[[instances_counter]] <- instances_df
          
          # Store Accuracy and Kappa results for % instances from the confusion matrix
          if(conf_matrix$overall["Kappa"] == "NaN"){ conf_matrix$overall["Kappa"] = 1 }
          #instances_AK[[instances_counter]] <- list(Accuracy = conf_matrix$overall["Accuracy"], Kappa = conf_matrix$overall["Kappa"])
          confMatrix_list[[instances_counter]] <- conf_matrix
          
          # Advance instances counter
          instances_counter = instances_counter + 1
          
          print("-")
        }
        
        # Alter names to make it easily readable
        names(instances_list) <- instances_names
        names(confMatrix_list) <- instances_names
        
        # Store % list with noise in list
        noise_list[[noise_counter]] <- instances_list
        noise_CM[[noise_counter]] <- confMatrix_list
        
        # Advance noise counter
        noise_counter = noise_counter + 1
      }
      
      # Alter names to make it easily readable
      names(noise_list) <- noise_names
      names(noise_CM) <- noise_names
      
      # Store noise list in method
      method_list[[method_counter]] <- noise_list
      method_CM[[method_counter]] <- noise_CM
    
    }
    
    # End of techniques
    # Alter names to make it easily readable
    #names(method_list) <- method_names
    #names(method_CM) <- method_names
    
    names(method_list) <- c("knn")
    names(method_CM) <- c("knn")
    # Store noise list in method
    folds_list[[folds_counter]] <- method_list
    folds_CM[[folds_counter]] <- method_CM
    
    # Advance fold counter
    folds_counter = folds_counter + 1
  }
  
  # Alter names to make it easily readable
  names(folds_list) <- fold_names
  names(folds_CM) <- fold_names
  
  # Store noise list in method
  dataset_list[[dataset_counter]] <- folds_list
  dataset_CM[[dataset_counter]] <- folds_CM
  
  # Advance dataset counter
  dataset_counter = dataset_counter + 1
  
  # Safeguard store by dataset
  filename1 = paste0("results/instances/by_dataset/", dataset, "_instances_KNN.rds")
  filename2 = paste0("results/instances/by_dataset/", dataset, "_instancesCM_KNN.rds")
  saveRDS(folds_list, file = filename1)
  saveRDS(folds_CM, file = filename2)
  
  print("Altered instances with noise recorded")
  print("----------------")
}

# Alter names to make it easily readable
names(dataset_list) <- datasets
names(dataset_CM) <- datasets

# Record full list
#saveRDS(dataset_list, file = "results/instances/instances_list.rds")
#saveRDS(dataset_CM, file = "results/instances/instancesCM_list.rds")
