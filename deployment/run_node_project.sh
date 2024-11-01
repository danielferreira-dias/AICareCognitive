#!/bin/bash

set -e

# Install Node.js v20.18.0 if not already installed
echo "Checking for Node.js v20.18.0..."
if ! node -v | grep -q "v20.18.0"; then
  echo "Installing Node.js v20.18.0..."
  curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
  sudo apt install -y nodejs
fi

# Define project directories
NODE_PROJECT_DIR="$HOME/code/frontend"  # Replace with your project directory
DEPLOY_DIR="/var/www/aicare"            # Destination directory for the build

# Ensure destination directory exists and set correct permissions
echo "Setting up the deployment directory at $DEPLOY_DIR..."
sudo mkdir -p "$DEPLOY_DIR"
sudo chown -R $USER:$USER "$DEPLOY_DIR"

# Navigate to Node.js project directory
echo "Navigating to Node.js project directory..."
cd "$NODE_PROJECT_DIR"

# Install dependencies
echo "Installing dependencies..."
npm install

# Build the project
echo "Building the project..."
npm run build

# Move built files to the deployment directory
echo "Deploying build to $DEPLOY_DIR..."
sudo rm -rf "$DEPLOY_DIR"/*  # Clear old build files
sudo cp -r "$NODE_PROJECT_DIR/dist/"* "$DEPLOY_DIR"

# Set permissions for the deployed files
echo "Setting permissions for deployed files..."
sudo chown -R www-data:www-data "$DEPLOY_DIR"
sudo chmod -R 755 "$DEPLOY_DIR"

# Optionally, start the server (assuming Nginx is handling the static files, no server start is required)
echo "Deployment completed. The static files are ready to be served by Nginx."

echo "Done."
