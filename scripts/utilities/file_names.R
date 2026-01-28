#!/usr/bin/env Rscript

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, tidyr)

# Create a list of all parameters
parameters <- list(
  dataset_name = c("analcatdata_authorship", "badges2", "banknote", 
                   "blood-transfusion-service-center", "breast-w", "cardiotocography",
                   "climate-model-simulation-crashes", "cmc", "credit-g", "diabetes",
                   "eucalyptus", "iris", "kc1", "liver-disorders", "mfeat-factors",
                   "mfeat-karhunen", "mfeat-zernike", "ozone-level-8hr", "pc4",
                   "phoneme", "qsar-biodeg", "tic-tac-toe", "vowel", "waveform-5000",
                   "wdbc", "wilt"),
  fold_name = paste0("Fold_", 1:5),
  technique_name = c("C5.0", "ctree", "fda", "gbm", "gcvEarth", "JRip", "lvq",
                    "mlpML", "multinom", "naive_bayes", "PART", "rbfDDA", "rda",
                    "rf", "rpart", "simpls", "svmLinear", "svmRadial", "rfRules",
                    "knn", "bayesglm"),
  noise_level = seq(0, 1, by = 0.1),
  instance_level = seq(0, 1, by = 0.1)
)

# Convert to a more structured format
parameters_df <- data.frame(
  parameter = names(parameters),
  values = sapply(parameters, paste, collapse = "|")
)

# Save as CSV
write.csv(parameters_df, 
          file = here("data", "files", "parameters.csv"), 
          row.names = FALSE)

# If you still need the individual RDS files
for(param_name in names(parameters)) {
  saveRDS(parameters[[param_name]], 
          file = here("data", "files", paste0(param_name, ".rds")))
}
