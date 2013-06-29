#!/bin/sh
# ~/.xinitrc > imrahil@triceratops

# Source files
xrdb -merge ~/.xresources

# Start deamons
/usr/bin/urxvtd -q -f -o

# Set wallpaper
/usr/bin/feh --bg-scale ~/.background.jpg

# Fire up Conky | dzen
~/scripts/conkyzen
conky -c ~/.conky/conkybg &
#conky -c ~/.conky/conkybr & # > many conkys > own windows! (c notes)
conky -c ~/.conky/conkyrings # drawing the lua rings (descrition of them as well, if wanted)

# remove dbus session, maybe it helps?!
rm -rf ~/.dbus/session-bus/*

# Launch window manager
exec xmonad
