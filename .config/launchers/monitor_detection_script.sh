#!/usr/bin/env bash

# if there is a monitor plugged in configure it to be on the left of the laptop
monitors=xrandr | grep -sw 'connected' | wc -l

if [ monitors==2 ]; then
  xrandr --output HDMI-1 --left-of eDP-1
fi
