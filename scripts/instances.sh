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
arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w") # datasets
arguments2=("C5.0" "ctree" "fda" "gbm" "gcvEarth" "JRip" "lvq" "mlpML" "multinom" "naive_bayes" "PART" "rbfDDA" "rda" "rf" "rpart" "simpls" "svmLinear" "svmRadial" "rfRules" "knn" "bayesglm")

# "analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w"
# "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes"
# "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors"
# "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme"
# "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt"

# Change to project root directory
cd "$PROJECT_ROOT"

echo "Starting instances alteration for ${#arguments[@]} datasets..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/instances/original/"
echo "Results will be saved to: $PROJECT_ROOT/results/instances/original/by_dataset/"
echo ""

for arg in "${arguments[@]}"; do
	echo "Starting instance alteration for dataset: $arg"
	for arg2 in "${arguments2[@]}"; do
		echo "  Using model: $arg2"
		# Start the R script in the background, redirecting output to log file
		nohup Rscript "$SCRIPTS_DIR/instances_noiser.R" "$arg" "$arg2" > "$PROJECT_ROOT/scripts/logs/instances/original/${arg}_${arg2}.log" 2>&1 &
	done
done

echo ""
echo "All processes started. Check log files for progress."
echo "To monitor progress: tail -f logs/instances/original/*.log"