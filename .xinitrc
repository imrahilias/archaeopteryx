#!/bin/sh
## ~/.xinitrc > m@archaeopteryx

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

## merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

## start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

## start deamons
urxvtd -q -f -o
udiskie & # dont forget the &
# thunar --daemon # bugs around!

## fire up mouse button to key stroke binder
xbindkeys
## fire up conky
conky -c ~/.config/conky/left.lua
#conky -c ~/.config/conky/right.lua
conky -c ~/.config/conky/middle.lua

## switch to custom keyboard layout
setxkbmap -layout "us(m),de" # enables multiple layouts, switch via awesome, us(m) is custom (see notes)

## disable screen blanking and powersaving mode
xset s off -dpms

## composite manager to avoid screen tearing
picom -b

## start network-manager
#nm-applet

## start awesome
exec awesome
