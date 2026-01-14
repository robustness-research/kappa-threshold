#!/bin/bash

# Get the absolute path to the project root and scripts directory
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPTS_DIR="$PROJECT_ROOT/scripts"

# Create logs directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/scripts/logs/instances/original"

# Create results directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/results/instances/original/by_dataset"

# Create vectors directory for storing instance indices
mkdir -p "$PROJECT_ROOT/results/instances/original/vectors"

# Remaining models for diabetes
diabetes_models=("rpart" "simpls" "svmLinear" "svmRadial" "rfRules" "knn" "bayesglm")

# Remaining datasets to process (after diabetes)
remaining_datasets=("eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors" "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme" "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt")

# All models
all_models=("C5.0" "ctree" "fda" "gbm" "gcvEarth" "JRip" "lvq" "mlpML" "multinom" "naive_bayes" "PART" "rbfDDA" "rda" "rf" "rpart" "simpls" "svmLinear" "svmRadial" "rfRules" "knn" "bayesglm")

# Change to project root directory
cd "$PROJECT_ROOT"

# Export variables so parallel can use them
export PROJECT_ROOT SCRIPTS_DIR

echo "Continuing instances alteration from where it stopped..."
echo "First completing diabetes with ${#diabetes_models[@]} remaining models..."
echo "Then processing ${#remaining_datasets[@]} remaining datasets with ${#all_models[@]} models each..."
echo "Running max 16 jobs in parallel..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/instances/original/"
echo "Results will be saved to: $PROJECT_ROOT/results/instances/original/by_dataset/"
echo ""

# First, complete the remaining diabetes models
echo "Processing remaining diabetes models..."
parallel -j 16 --line-buffer --progress \
  Rscript "$SCRIPTS_DIR/instances_noiser.R" diabetes {1} '>' "$PROJECT_ROOT/scripts/logs/instances/original/diabetes_{1}.log" 2'>&1' \
  ::: "${diabetes_models[@]}"

echo ""
echo "Diabetes completed! Now processing remaining datasets..."
echo ""

# Then run the remaining datasets with all models
parallel -j 16 --line-buffer --progress \
  Rscript "$SCRIPTS_DIR/instances_noiser.R" {1} {2} '>' "$PROJECT_ROOT/scripts/logs/instances/original/{1}_{2}.log" 2'>&1' \
  ::: "${remaining_datasets[@]}" ::: "${all_models[@]}"

echo ""
echo "All processes completed!"
