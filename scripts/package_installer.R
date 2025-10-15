# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages that need to be installed (only once)
install.packages("pacman")
install.packages("caret")
install.packages("citation")
install.packages("ggpubr")
install.packages("iml")
install.packages("tidyverse")
install.packages("xtable")
install.packages("farff")
install.packages("dplyr")

# Packages for caret training
install.packages("klaR")
install.packages("randomForest")
install.packages("C50")
install.packages("naivebayes")
install.packages("kernlab")
install.packages("RSNNS")
install.packages("pls")
install.packages("earth")
install.packages("party")
install.packages("mda")
install.packages("gbm")
install.packages("RWeka")
install.packages("inTrees")
install.packages("arm")
install.packages("adabag")
install.packages("xgboost")

# Packages for ML training
install.packages("lime")

