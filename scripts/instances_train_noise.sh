#!/bin/bash

# Get the absolute path to the project root and scripts directory
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPTS_DIR="$PROJECT_ROOT/scripts"

# Note: Using existing vectors from results/instances/original/vectors/
# No need to create new vectors directory

# Define datasets to process
arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w" "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes" "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors" "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme" "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt") # datasets
arguments2=("C5.0" "ctree" "fda" "gbm" "gcvEarth" "JRip" "lvq" "mlpML" "multinom" "naive_bayes" "PART" "rbfDDA" "rda" "rf" "rfRules"  "rpart" "simpls" "svmLinear" "svmRadial" "knn" "bayesglm")

# "analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w"
# "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes"
# "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors"
# "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme"
# "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt"

# Change to project root directory
cd "$PROJECT_ROOT"

# Export variables so they can be used in subshells
export PROJECT_ROOT SCRIPTS_DIR

echo "Starting instances alteration for ${#arguments[@]} datasets with ${#arguments2[@]} models..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/instances/train_noise/"
echo "Results will be saved to: $PROJECT_ROOT/data/results/instances/train_noise/by_dataset/"
echo ""

# Create combinations of datasets and models
counter=0
max_jobs=4
running_jobs=0

for dataset in "${arguments[@]}"; do
  for model in "${arguments2[@]}"; do
    counter=$((counter + 1))
    
    # Wait if max concurrent jobs reached
    while [ $(jobs -r | wc -l) -ge $max_jobs ]; do
      sleep 1
    done
    
    # Run the Rscript in background
    Rscript "$SCRIPTS_DIR/instances_noiser_train_noise.R" "$dataset" "$model" > "$SCRIPTS_DIR/logs/instances/train_noise/${dataset}_${model}.log" 2>&1 &
    
    echo "Submitted job $counter: $dataset - $model"
  done
done

# Wait for all remaining jobs to complete
wait
echo ""
echo "All processes completed!"