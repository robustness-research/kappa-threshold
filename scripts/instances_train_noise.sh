#!/bin/bash

# Get the absolute path to the project root and scripts directory
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPTS_DIR="$PROJECT_ROOT/scripts"

# Create logs directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/scripts/logs/instances/train_noise"

# Create results directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/results/instances/train_noise/by_dataset"

# Note: Using existing vectors from results/instances/original/vectors/
# No need to create new vectors directory

# Define datasets to process
arguments=("ozone-level-8hr" "pc4" "phoneme" "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt") # datasets
arguments2=("C5.0" "ctree" "fda" "gbm" "gcvEarth" "JRip" "lvq" "mlpML" "multinom" "naive_bayes" "PART" "rbfDDA" "rda" "rf" "rfRules"  "rpart" "simpls" "svmLinear" "svmRadial" "knn" "bayesglm")

# "analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w"
# "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes"
# "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors"
# "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme"
# "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt"

# 

# Change to project root directory
cd "$PROJECT_ROOT"

# Export variables so parallel can use them
export PROJECT_ROOT SCRIPTS_DIR

echo "Starting instances alteration for ${#arguments[@]} datasets with ${#arguments2[@]} models..."
echo "Running max 4 jobs in parallel..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/instances/train_noise/"
echo "Results will be saved to: $PROJECT_ROOT/results/instances/train_noise/by_dataset/"
echo ""

# Run with max 4 concurrent jobs using GNU Parallel
# Note: --progress removed to avoid TTY errors when running with nohup
parallel -j 4 --line-buffer \
  Rscript "$SCRIPTS_DIR/instances_noiser_train_noise.R" {1} {2} \> "$PROJECT_ROOT/scripts/logs/instances/train_noise/{1}_{2}.log" 2\>\&1 \
  ::: "${arguments[@]}" ::: "${arguments2[@]}"

echo ""
echo "All processes completed!"