#!/bin/sh
#
# ~/.xinitrc

# Source files
#xrdb -merge .Xdefaults #@jinn
#/usr/bin/xrdb -load ~/.config/xorg/Xdefaults
#source ~/.config/shell/exports
#xrdb -merge ~/.Xressources #@m
xrdb -merge ~/.xresources

# Start deamons
#/usr/bin/mpd ~/.mpd/mpd.conf
/usr/bin/urxvtd -q -f -o
#/usr/bin/emacs --daemon # alias amc='emacsclient -c -a ""' is a better option (open server an firt access, reconnects afterwards) 

# Set typematic
#/usr/bin/xset r rate 180 20 #autorepeat (google:xset)

# Set wallpaper
/usr/bin/feh --bg-scale ~/.background.jpg

# Fire up Conky | dzen
~/scripts/conkyzen.sh

# Launch window manager
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session /usr/bin/xmonad # dbus-launch (...) actually causes problems it should prevail!
exec xmonad
