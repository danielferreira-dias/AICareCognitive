#!/bin/bash

# Define the project root directory
project_root="."

# Create directories
mkdir -p "$project_root/data/raw" \
         "$project_root/data/processed" \
         "$project_root/data/external" \
         "$project_root/notebooks" \
         "$project_root/src/preprocessing" \
         "$project_root/src/models" \
         "$project_root/src/optimization" \
         "$project_root/src/utils" \
         "$project_root/tests" \
         "$project_root/config" \
         "$project_root/results/figures" \
         "$project_root/results/models" \
         "$project_root/results/reports" \
         "$project_root/results/logs"

# Create placeholder files
touch "$project_root/data/README.md" \
      "$project_root/notebooks/README.md" \
      "$project_root/src/preprocessing/__init__.py" \
      "$project_root/src/models/__init__.py" \
      "$project_root/src/optimization/__init__.py" \
      "$project_root/src/utils/__init__.py" \
      "$project_root/src/main.py" \
      "$project_root/tests/__init__.py" \
      "$project_root/config/settings.yaml" \
      "$project_root/config/model_config.yaml" \
      "$project_root/config/optimization_config.yaml" \
      "$project_root/requirements.txt" \
      "$project_root/environment.yml" \
      "$project_root/README.md" \

# Create .tmp files in empty directories
touch "$project_root/data/raw/.tmp" \
      "$project_root/data/processed/.tmp" \
      "$project_root/data/external/.tmp" \
      "$project_root/results/figures/.tmp" \
      "$project_root/results/models/.tmp" \
      "$project_root/results/reports/.tmp" \
      "$project_root/results/logs/.tmp"

echo "Project structure created successfully at $project_root"
