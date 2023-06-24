#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# MONITORS=$(xrandr --query | grep " connected" | cut -d" " -f1)
MONITOR=$MONITORS polybar bottom
# Launch bar1 and bar2
# MONITORS=$(xrandr --query | grep " connected" | cut -d" " -f1)

# MONITORS=$MONITORS polybar top &
# MONITOR=$MONITORS polybar bottom;

# echo "Bars launched..."
