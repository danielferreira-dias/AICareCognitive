#!/bin/bash

set -e

# Install Node.js v20.18.0 if not already installed
echo "Checking for Node.js v20.18.0..."
if ! node -v | grep -q "v20.18.0"; then
  echo "Installing Node.js v20.18.0..."
  curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
  sudo apt install -y nodejs
fi

echo "Navigating to Node.js project directory..."
NODE_PROJECT_DIR="$HOME/code/frontend"  # Replace with your project directory
cd "$NODE_PROJECT_DIR"

echo "Installing dependencies..."
npm install

echo "Starting Node.js project in the background..."
nohup npm run dev > node_project.log 2>&1 & echo $! > node_project.pid

echo "Node.js project started with PID $(cat node_project.pid)."
