#!/bin/bash

# Kill any existing lock screen processes
pkill -9 hyprlock 2>/dev/null

# Small delay to ensure processes are killed
sleep 0.5

# Launch hyprlock
hyprlock
