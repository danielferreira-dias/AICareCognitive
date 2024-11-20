#!/bin/bash

set -e

# Install SWI-Prolog if not already installed
echo "Checking for SWI-Prolog..."
if ! command -v swipl &> /dev/null; then
  echo "Installing SWI-Prolog..."
  sudo apt update
  sudo apt install -y swi-prolog
fi

echo "Navigating to Prolog project directory..."
PROLOG_PROJECT_DIR="$HOME/code/prolog"  # Replace with your project directory
cd "$PROLOG_PROJECT_DIR"

echo "Starting Prolog server in the background..."
nohup swipl -s server.pl -g "server(8081), thread_get_message(quit)" > prolog_server.log 2>&1 & echo $! > prolog_server.pid

echo "Prolog server started with PID $(cat prolog_server.pid)."
