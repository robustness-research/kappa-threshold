#!/usr/bin/env Rscript

# Load necessary libraries
pacman::p_load(caret, citation, data.table, dplyr, earth, farff, ggpubr, ggplot2, 
               iml, knitr, rpart, tidyverse, tidyr, xtable, factoextra, proxy, 
               dominanceanalysis, clustertend, MASS, smacof, vegan, cluster, flexclust)

# Define datasets
datasets <- c("analcatdata_authorship", "badges2", "banknote", 
              "blood-transfusion-service-center", "breast-w", "cardiotocography", 
              "climate-model-simulation-crashes", "cmc", "credit-g", "diabetes", 
              "eucalyptus", "iris", "kc1", "liver-disorders", "mfeat-karhunen", 
              "mfeat-zernike", "ozone-level-8hr", "pc4", "phoneme", "qsar-biodeg", 
              "tic-tac-toe", "vowel", "waveform-5000", "wdbc", "wilt")

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


# Create a dataframe to store the summary
characteristics_df <- data.frame(
  dataset_name = character(0),
  instances_n = numeric(0),
  attributes_num = numeric(0),
  attributes_nom = numeric(0),
  dataset_type = character(0),
  stringsAsFactors = FALSE
)

# Iterate through datasets
for (dataset_name in datasets) {
  # Load dataset
  filename = paste0("datasets/", dataset_name, ".rds")
  dataset <- readRDS(filename)
  # Calculate summary metrics
  num_instances <- get_num_instances(dataset)
  attribute_types <- get_attribute_type(dataset)
  dataset_type <- is_binary(dataset)
  # Add to summary dataframe
  characteristics_df <- bind_rows(characteristics_df, data.frame(
    dataset_name = dataset_name,
    instances_n = num_instances,
    attributes_num = attribute_types$numerical,
    attributes_nom = attribute_types$nominal,
    dataset_type = dataset_type
  ))
}

characteristics_df$dataset_type = ifelse(characteristics_df$dataset_type == "binary", 0, 1)

# Convert all data to numeric
characteristics_df$instances_n <- as.numeric(characteristics_df$instances_n)
characteristics_df$attributes_num <- as.numeric(characteristics_df$attributes_num)
characteristics_df$attributes_nom <- as.numeric(characteristics_df$attributes_nom)
characteristics_df$dataset_type <- as.numeric(characteristics_df$dataset_type)

# Print the summary dataframe
print(characteristics_df)

# Save as CSV
write.csv(characteristics_df, "data/results/clustering/characteristics.csv", row.names = FALSE)

# Define test datasets
test_data <- c("analcatdata_authorship", "breast-w", "cardiotocography", "liver-disorders")

# Save test_data as CSV (as a single-column file)
test_data_df <- data.frame(dataset_name = test_data)
write.csv(test_data_df, "data/results/clustering/test_data.csv", row.names = FALSE)

# Scale the data
clusters_scaled <- scale(characteristics_df[,-1])

# Add dataset names back
clusters_scaled <- cbind(dataset_name = characteristics_df$dataset_name, clusters_scaled)

# Turn into dataframe
clusters_scaled <- as.data.frame(clusters_scaled)

# Print the scaled dataframe
print(clusters_scaled)

# Save as CSV
write.csv(clusters_scaled, "data/results/clustering/cl_scaled.csv", row.names = FALSE)

# Ensure all necessary data is numeric
clusters_scaled$instances_n <- as.numeric(clusters_scaled$instances_n)
clusters_scaled$attributes_num <- as.numeric(clusters_scaled$attributes_num)
clusters_scaled$attributes_nom <- as.numeric(clusters_scaled$attributes_nom)
clusters_scaled$dataset_type <- as.numeric(clusters_scaled$dataset_type)

# Create distance matrix for all data
set.seed(1)
dmatrix <- dist(clusters_scaled[,-1])

# Save distance matrix as CSV (as a matrix)
dmatrix_csv <- as.matrix(dmatrix)
write.csv(dmatrix_csv, "data/results/clustering/dmatrix.csv")

# Create new dataframe without the test set
train_set <- clusters_scaled[!clusters_scaled$dataset_name %in% test_data, ]

# Create new dataframe with only the test set
test_set <- clusters_scaled[clusters_scaled$dataset_name %in% test_data, ]

# Save as CSV
write.csv(train_set, "data/results/clustering/train_set.csv", row.names = FALSE)
write.csv(test_set, "data/results/clustering/test_set.csv", row.names = FALSE)

# Create distance matrix for training set
train_dmatrix <- dist(train_set[,-1])

# Save training distance matrix as CSV
train_dmatrix_csv <- as.matrix(train_dmatrix)
write.csv(train_dmatrix_csv, "data/results/clustering/train_dmatrix.csv")

# Create a new table with dataset names and rownames
groups_df <- data.frame(
  ID = as.numeric(rownames(train_set)),
  dataset_name = train_set$dataset_name
)

# K = 4 Hierarchical Clustering
hclusters <- hclust(train_dmatrix, method = "ward.D")
groups_k4 <- cutree(hclusters, k = 4)
groups_df$hclust_4 <- groups_k4

# K = 5 Hierarchical Clustering
groups_k5 <- cutree(hclusters, k = 5)
groups_df$hclust_5 <- groups_k5

# K = 6 Hierarchical Clustering
groups_k6 <- cutree(hclusters, k = 6)
groups_df$hclust_6 <- groups_k6

# K = 4 K-Means
set.seed(1)
kmeans_4 <- kmeans(train_set[,-1], centers = 4, nstart = 25)
groups_df$kmeans_4 <- kmeans_4$cluster
centroids_4 <- as.data.frame(kmeans_4$centers)
centroids_4$K <- 4

# K = 5 K-Means
set.seed(1)
kmeans_5 <- kmeans(train_set[,-1], centers = 5, nstart = 25)
groups_df$kmeans_5 <- kmeans_5$cluster
centroids_5 <- as.data.frame(kmeans_5$centers)
centroids_5$K <- 5

# K = 6 K-Means
set.seed(1)
kmeans_6 <- kmeans(train_set[,-1], centers = 6, nstart = 25)
groups_df$kmeans_6 <- kmeans_6$cluster
centroids_6 <- as.data.frame(kmeans_6$centers)
centroids_6$K <- 6

# Combine centroids
centroids <- rbind(centroids_4, centroids_5, centroids_6)

# Save centroids as CSV
write.csv(centroids, "data/results/clustering/centroids.csv", row.names = FALSE)

# Save KCCA information (K=4, as used in the original)
kcca_4 <- as.kcca(kmeans_4, train_set[,-1])
kcca_info <- data.frame(
  K = 4,
  num_centers = nrow(kmeans_4$centers),
  tot_withinss = kmeans_4$tot.withinss,
  betweenss = kmeans_4$betweenss,
  size_cluster_1 = kmeans_4$size[1],
  size_cluster_2 = kmeans_4$size[2],
  size_cluster_3 = kmeans_4$size[3],
  size_cluster_4 = kmeans_4$size[4]
)
write.csv(kcca_info, "data/results/clustering/kcca_info.csv", row.names = FALSE)

# Compute MDS for training set
fit_mds <- isoMDS(train_dmatrix, k = 2)

# Create a table with the results
coord_df <- data.frame(
  dataset_name = train_set$dataset_name,
  coordinate_1 = fit_mds$points[, 1],
  coordinate_2 = fit_mds$points[, 2]
)

# Print the results table
print(coord_df)

# Add coordinates to groups_df
groups_df <- left_join(groups_df, coord_df, by = "dataset_name")

# Save the complete groups dataframe as CSV
write.csv(groups_df, "data/results/clustering/groups.csv", row.names = FALSE)

cat("\n=== CLUSTERING ANALYSIS COMPLETE ===\n")
cat("data saved:\n")
cat("  - data/results/clustering/characteristics.csv\n")
cat("  - data/results/clustering/test_data.csv\n")
cat("  - data/results/clustering/cl_scaled.csv\n")
cat("  - data/results/clustering/dmatrix.csv\n")
cat("  - data/results/clustering/train_set.csv\n")
cat("  - data/results/clustering/test_set.csv\n")
cat("  - data/results/clustering/train_dmatrix.csv\n")
cat("  - data/results/clustering/centroids.csv\n")
cat("  - data/results/clustering/kcca_info.csv\n")
cat("  - data/results/clustering/groups.csv\n")
