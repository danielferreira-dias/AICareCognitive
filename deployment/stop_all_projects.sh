#!/bin/bash

echo "Stopping all running projects..."
BASE_PROJECT_DIR="$HOME/code"  # Replace with your base project directory

# Define paths to PID files
JAVA_PID_FILE="$BASE_PROJECT_DIR/AICare/java_project.pid"
PROLOG_PID_FILE="$BASE_PROJECT_DIR/prolog/prolog_server.pid"

# Stop each project by invoking the stop_project.sh script
./stop_project.sh "$JAVA_PID_FILE"
./stop_project.sh "$PROLOG_PID_FILE"

echo "All projects have been stopped."
