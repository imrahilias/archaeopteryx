#!/bin/sh
# ~/.xinitrc > imrahil@archaeopteryx

# Source files
xrdb -merge ~/.xresources

# Start deamons
/usr/bin/urxvtd -q -f -o
sudo razerd -B

# start composit manager
#xcompmgr -c -o 0 &

# Set wallpaper
/usr/bin/feh --bg-scale ~/.backgrounds/marokko.png

# Fire up Conky | dzen
~/scripts/conkyzen
conky -c ~/.conky/background.conky &

# remove dbus session, maybe it helps?!
#rm -rf ~/.dbus/session-bus/*

# Launch window manager
exec xmonad
