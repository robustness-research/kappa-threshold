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

# Define datasets to process
arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w" "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes" "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors" "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme" "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt") # datasets
arguments2=("C5.0" "ctree" "fda" "gbm" "gcvEarth" "JRip" "lvq" "mlpML" "multinom" "naive_bayes" "PART" "rbfDDA" "rda" "rf" "rpart" "simpls" "svmLinear" "svmRadial" "rfRules" "knn" "bayesglm")

# "analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w"
# "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes"
# "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors"
# "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme"
# "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt"

# Change to project root directory
cd "$PROJECT_ROOT"

# Export variables so parallel can use them
export PROJECT_ROOT SCRIPTS_DIR

echo "Starting instances alteration for ${#arguments[@]} datasets with ${#arguments2[@]} models..."
echo "Running max 16 jobs in parallel..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/instances/original/"
echo "Results will be saved to: $PROJECT_ROOT/results/instances/original/by_dataset/"
echo ""

# Run with max 16 concurrent jobs using GNU Parallel
parallel -j 16 --line-buffer --progress \
  Rscript "$SCRIPTS_DIR/instances_noiser.R" {1} {2} '>' "$PROJECT_ROOT/scripts/logs/instances/original/{1}_{2}.log" 2'>&1' \
  ::: "${arguments[@]}" ::: "${arguments2[@]}"

echo ""
echo "All processes completed!"