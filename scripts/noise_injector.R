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
pacman::p_load(citation, dplyr)

# Set the seed to make the experiment reproducible
set.seed(1)

# Load parameters
load_parameters <- function(params_file = "data/files/parameters.csv") {
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

  # Load noise levels from parameters file
  noise_levels <- unlist(strsplit(subset(params, parameter == "noise_levels")$values, "\\|"))
  noise_levels <- as.numeric(noise_levels)
  
  # Load most important attributes data
  mia_df <- read.csv("results/most_important_attr/miaUnique.csv", stringsAsFactors = FALSE)
  
  list(
    datasets = datasets,
    noise_levels = noise_levels,
    mia_df = mia_df
  )
}

# Inject noise into a single attribute at a specific noise level
inject_noise_at_level <- function(df, mia, noise, std_dev = NULL) {
  # Check if the class of the most important attribute is not a character
  cat(paste("Attribute's class:", class(df[[mia]]), "\n"))
  transformed <- FALSE
  if(class(df[[mia]]) == "factor") {
    df[[mia]] <- as.character(df[[mia]])
    transformed <- TRUE
  }
  
  isNominal <- class(df[[mia]]) == "character"
  
  if(!isNominal){
    # Make the most important attribute the mean of the most important attribute
    mia_mean <- mean(df[[mia]])
    
    # Obtain the standard deviation
    std_dev <- sd(df[[mia]])
    cat(paste0("Attribute's mean: ", mia_mean, ", and standard deviation: ", std_dev, "\n"))
  }
  
  # Create an auxiliary dataframe to add noise to
  noise_df <- df
  
  # Injection of noise
  if(noise != 0){
    cat("Noise injection into selected attribute\n")

    if(isNominal){
      # If the most important attribute is nominal, apply formula
      cat("Attribute is nominal\n")
      
      # Obtain frequency of each possible attribute as a probability in an array
      mia_new <- df %>%
        group_by(df[[mia]]) %>%
        dplyr::summarise(n = n()) %>%
        mutate(Freq = n/sum(n))
      p <- mia_new$Freq
      
      # Obtain the alpha
      alpha <- 1 - exp(-noise)
      
      # Obtain vector t, used as one hot encoding to turn categorical data into integers
      ## Create a one-hot encoding
      t <- model.matrix(~ factor(df[[mia]]) - 1)
      ## Convert to a matrix and then to a data frame
      t <- as.data.frame(t)
      
      colnames(t) <- sort(unique(df[[mia]]))
      
      # Calculate the new probability in an array
      ## For each row, change the probability of the most important attributes for the new ones obtained through the formula
      ## Obtain a sampling to get a new value for that column corresponding to the new probabilities
      for(row in 1:nrow(t)){
        p1 <- alpha*p+(1-alpha)*t[row, ]
        noise_df[[mia]][row] <- sample(sort(unique(df[[mia]])), 1, prob = p1, replace = FALSE)
      }
      
      # After injecting noise, coerce Attribute's class to the original, in case it changed
      if(transformed) {
        noise_df[[mia]] <- as.factor(noise_df[[mia]])
      }
    }
    else {
      # If the most important attribute is not nominal, apply corresponding formula
      cat("Attribute is numerical\n")
      
      ## Standard deviation * noise value
      dvar <- std_dev * noise
      
      # For each value of the most important attribute, add normalized noise and round to two decimals
      noise_df[[mia]] <- sapply(noise_df[[mia]], function(x) rnorm(1, x, dvar))
      noise_df[[mia]] <- round(noise_df[[mia]], digits = 2)
    }
  }
  else {
    cat("No noise injected (noise level is 0)\n")
  }
  noise_df
}

# Process all noise levels for a single attribute
process_attribute <- function(dataset, df, mia, noise_levels) {
  cat("Dataset:", dataset, "\n")
  cat("Most important attribute:", mia, "\n")
  
  # Process each noise level and save as CSV
  for(noise in noise_levels) {
    cat("Noise level:", noise, "\n")
    noise_df <- inject_noise_at_level(df, mia, noise)
    
    # Add metadata columns for easy unification later
    noise_df$dataset_name <- dataset
    noise_df$altered_attribute <- mia
    noise_df$noise_level <- as.numeric(noise)
    
    # Create filename with dataset, attribute, and noise level
    filename <- paste0("results/noise_injection/by_dataset/", dataset, "_", mia, "_noise", noise, ".csv")
    write.csv(noise_df, file = filename, row.names = FALSE)
    cat("Saved:", filename, "\n")
    cat("________________\n")
  }
  
  invisible(NULL)
}

# Process all attributes for a single dataset
process_dataset <- function(dataset, noise_levels, mia_df) {
  # Load dataset
  filename <- paste0("data/datasets/", dataset, ".csv")
  df <- read.csv(filename, stringsAsFactors = FALSE)
  
  # Filter from the Attribute list for only this dataset
  dataset_mia <- mia_df %>% 
    distinct(dataset_name, most_important_attribute) %>% 
    filter(dataset_name == dataset)
  
  # Create a list of the most important attributes
  mia_list <- as.list(dataset_mia$most_important_attribute)
  
  # Process all attributes for this dataset (saves CSVs internally)
  for(mia in mia_list) {
    process_attribute(dataset, df, mia, noise_levels)
  }
  
  cat(paste0("Finished with adding noise to dataset: ", dataset, "\n"))
  cat("----------------------\n")
  
  invisible(NULL)
}

# Load and extract parameters
parameters <- load_parameters()
datasets <- parameters$datasets
noise_levels <- parameters$noise_levels
mia_df <- parameters$mia_df

# Process each dataset (saves CSVs internally)
for(dataset in datasets) {
  process_dataset(dataset, noise_levels, mia_df)
}

cat("**********************\n")
cat("Noise injected in data\n")
cat("**********************\n")
