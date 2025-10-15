# /usr/bin/env/Rscript

# Packages that need to be loaded
pacman::p_load(citation, dplyr)

# Set the seed to make the experiment reproducible
set.seed(1)

# Load data
args <- commandArgs(trailingOnly = TRUE)
datasets <- args
noise_level <- readRDS("files/noise.rds")
noise_names <- readRDS("files/noise_names.rds")
mia_df <- readRDS("results/most_important_attr/miaUnique_df.rds")

# Create a list to store the attribute lists per dataframe
dataset_list <- list()
dataset_counter = 1

# Per dataset
for(dataset in datasets) {
  
  # Load dataset
  filename = paste0("datasets/", dataset, ".rds")
  df <- readRDS(filename)

  # Filter from the MIA list for only this dataset
  dataset_mia <- mia_df %>% 
    distinct(dataset_name, most_important_attribute) %>% 
    filter(dataset_name == dataset)
  
  # Create a list of the most important attributes
  mia_list <- as.list(dataset_mia$most_important_attribute)
  
  # Create a list to store the noise lists per attribute
  attr_list <- list()
  attr_counter = 1
  
  # For each important attribute, process noise
  for(mia in mia_list) {
    
    # Create a list to store the dataframe with noise per level of noise
    noise_list <- list()
    noise_counter = 1
    
    # Add noise to most important attribute in the dataset and create a result file to pull data from later
    for(noise in noise_level){
      
      # Print information on current iteration
      print(paste("Dataset:", dataset))
      print(paste("Most important attribute:", mia))
      print(paste("Noise level:", noise))
      
      # Check if the class of the most important attribute is not a character
      print(paste("MIA's class:", class(df[[mia]])))
      if(class(df[[mia]]) == "factor") {
        df[[mia]] <- as.character(df[[mia]])
        transformed = TRUE
      }
      isNominal = class(df[[mia]]) == "character"
      print(paste("Is the MIA a nominal attribute?", isNominal))
      if(!isNominal){
        # Make the most important attribute the mean of the most important attribute
        mia_mean =  mean(df[[mia]])
        
        # Obtain the standard deviation
        std_dev = sd(df[[mia]])
        print(paste0("MIA's mean: ", mia_mean, ", and standard deviation: ", std_dev))
      }
      
      # Create an auxiliary dataframe to add 100% of noise to
      noise_df <- df
      
      # Injection of noise
      if(noise != 0){
        print("NOISE INJECTION")
        if(isNominal){
          
          # If the most important attribute is nominal, apply formula
          print("MIA is a nominal attribute")
          
          # Obtain frequency of each possible attribute as a probability in an array
          mia_new = df %>%
            group_by(df[[mia]]) %>%
            dplyr::summarise(n = n()) %>%
            mutate(Freq = n/sum(n))
          p = mia_new$Freq
          # Obtain the alpha
          alpha = 1 - exp(-noise)
          # Obtain vector t, used as one hot encoding to turn categorical data into integers
          ## Create a one-hot encoding
          t <- model.matrix(~ factor(df[[mia]]) - 1)
          ## Convert to a matrix and then to a data frame
          t <- as.data.frame(t)
          
          colnames(t) <- sort(unique(df[[mia]]))
          
          # Calculate the new probability in an array
          ## For each row, change the probability of the most important attributes for the new ones obtained through the formula
          ## Obtain a sampling to get a new  value for that column corresponding to the new probabilities
          test_df = data.frame(matrix(ncol = 3, nrow = 0))
          for(row in 1:nrow(t)){
            p1 = alpha*p+(1-alpha)*t[row, ]
            test_df[nrow(test_df) + 1,] <- p1
            sort(unique(df[[mia]]))
            sample(sort(unique(df[[mia]])),1,prob = p1, replace = FALSE)
            noise_df[[mia]][row] = sample(sort(unique(df[[mia]])),1,prob = p1, replace = FALSE)
          }
          
          # After injecting noise, coerce MIA's class to the original, in case it changed
          if(transformed) {
            noise_df[[mia]] <- as.factor(noise_df[[mia]])
          }
        }
        else {
          
          # If the most important attribute is not nominal, apply corresponding formula
          print("MIA is NOT a nominal attribute")
          
          ## Standard deviation * noise value
          dvar = std_dev * noise
          
          # For each value of the most important attribute, add normalized noise and round to two decimals
          noise_df[[mia]] <- sapply(noise_df[[mia]], function(x) rnorm(1, x, dvar))
          noise_df[[mia]] <- round(noise_df[[mia]], digits = 2)
        }
      }
      else {
        print("NO NOISE INJECTED")
        if(isNominal) {
          if(transformed) {
            noise_df[[mia]] <- as.factor(noise_df[[mia]])
          }
        }
      }
      
      # Store df with noise in list
      noise_list[[noise_counter]] <- noise_df
      
      # Advance noise counter
      noise_counter = noise_counter + 1
      
      print("________________")
    }
    
    # Alter names to make it easily readable
    names(noise_list) <- noise_names
    
    # Store df with noise in list
    attr_list[[attr_counter]] <- noise_list
    
    # Advance attribute counter
    attr_counter = attr_counter + 1
  }
  
  # Alter names to make it easily readable
  names(attr_list) <- mia_list
  
  # Store df with noise in list
  dataset_list[[dataset_counter]] <- attr_list
  
  # Safeguard store by dataset
  filename = paste0("results/noise/by_dataset/", dataset, "_noiseMIA.rds")
  saveRDS(attr_list, file = filename)
    
  # Advance attribute counter
  dataset_counter = dataset_counter + 1
  print(paste0("Finished with adding noise to dataset: ", dataset))
  
  print("----------------------")
} 

# Alter names to make it easily readable
names(dataset_list) <- datasets

# Record full list
#saveRDS(dataset_list, file = "results/noise/noise_list.rds")

print("****************")
print("RESULTS RECORDED")
print("****************")
