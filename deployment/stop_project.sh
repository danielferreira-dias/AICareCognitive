#!/bin/bash

# Usage: ./stop_project.sh /path/to/project.pid

PID_FILE="$1"  # Path to the PID file provided as an argument

echo "Stopping the project..."

# Check if the PID file exists
if [ -f "$PID_FILE" ]; then
    PID=$(cat "$PID_FILE")
    echo "Found PID: $PID"
    
    # Check if the process is running
    if ps -p "$PID" > /dev/null; then
        kill "$PID"
        echo "Process $PID has been stopped."
        rm "$PID_FILE"  # Remove the PID file
    else
        echo "Process with PID $PID is not running."
        rm "$PID_FILE"  # Clean up the PID file even if process not running
    fi
else
    echo "PID file not found: $PID_FILE"
fi
