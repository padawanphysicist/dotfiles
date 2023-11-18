#!/bin/bash

# Terminate already running bar instances
killall -q polybar
killall -q stalonetray

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
while pgrep -u $UID -x stalonetray >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar panel-stumpwm &
sleep 10
stalonetray &

## echo "Polybar launched..."