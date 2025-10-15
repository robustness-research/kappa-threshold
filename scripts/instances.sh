#!/bin/bash

# Create logs directory if it doesn't exist
mkdir -p logs/instances

#arguments=("analcatdata_authorship" "badges2" "banknote" "blood-transfusion-service-center" "breast-w" "cardiotocography" "climate-model-simulation-crashes" "cmc" "credit-g" "diabetes" "eucalyptus" "iris" "kc1" "liver-disorders" "mfeat-factors" "mfeat-karhunen" "mfeat-zernike" "ozone-level-8hr" "pc4" "phoneme" "qsar-biodeg" "tic-tac-toe" "vowel" "waveform-5000" "wdbc" "wilt")

arguments=("iris")


for arg in "${arguments[@]}"; do

	nohup Rscript Instances.R "$arg" > logs/instances/"$arg.log" 2>&1 &

done
