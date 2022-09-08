#!/usr/bin/env bash
source monitor_detection_script.sh

# Terminate already running bar instances
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config.ini
if [ $monitors==2 ]; then
  MONITOR=eDP-1 polybar example &
  MONITOR=HDMI-1 polybar example &
else
  polybar &
fi


