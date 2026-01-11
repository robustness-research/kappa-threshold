#!/bin/bash

# Get the absolute path to the project root and scripts directory
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPTS_DIR="$PROJECT_ROOT/scripts"

# Create logs directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/scripts/logs/noise_injection"

# Create results directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/results/noise_injection/by_dataset"

# Define datasets to process
# arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w")

arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w"
           "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes"
           "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors"
           "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme"
           "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt")

# Change to project root directory
cd "$PROJECT_ROOT"

echo "Starting noise injection for ${#arguments[@]} datasets..."
echo "Logs will be saved to: $PROJECT_ROOT/scripts/logs/noise_injection/"
echo "Results will be saved to: $PROJECT_ROOT/results/noise_injection/by_dataset/"
echo ""

# Process each dataset in parallel
for arg in "${arguments[@]}"; do
	echo "Starting noise injection for dataset: $arg"
	nohup Rscript "$SCRIPTS_DIR/noise_injector.R" "$arg" > "$PROJECT_ROOT/scripts/logs/noise_injection/$arg.log" 2>&1 &
done

echo ""
echo "All processes started. Check log files for progress."
echo "To monitor progress: tail -f scripts/logs/noise_injection/*.log"
