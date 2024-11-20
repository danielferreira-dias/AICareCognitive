#!/bin/bash

set -e

echo "Updating package list..."
sudo apt update

echo "Installing required packages (e.g., git, build-essential)..."
sudo apt install -y git build-essential curl

echo "Environment preparation complete."