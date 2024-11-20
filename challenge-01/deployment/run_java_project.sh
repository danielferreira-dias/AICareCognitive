#!/bin/bash

set -e

# Install Java 8 if not already installed
echo "Checking for Java 8..."
if ! java -version 2>&1 | grep -q "1.8"; then
  echo "Installing Java 8..."
  sudo apt update
  sudo apt install -y openjdk-8-jdk
fi

echo "Navigating to Java project directory..."
JAVA_PROJECT_DIR="$HOME/code/AICare"  # Replace with your project directory
cd "$JAVA_PROJECT_DIR"

echo "Building the Java project..."
./mvnw clean install

echo "Starting Java project in the background..."
nohup java -jar -Xmx512m -Xms256m target/AICare-0.0.1-SNAPSHOT.jar > java_project.log 2>&1 & echo $! > java_project.pid

echo "Java project started with PID $(cat java_project.pid)."
