#!/usr/bin/env bash
xrdb -override dawesome.xres
#xrandr --output eDP-1-1 --off --output HDMI-1-2 --mode 3840x1600 --primary
xrandr --output eDP-1-1 --off --output eDP-1-3 --mode 3840x1600 --primary
exec awesome --config $HOME/.config/dawesome/rc.lua
