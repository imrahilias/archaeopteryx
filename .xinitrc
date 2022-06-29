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

## start keyring
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

## start nextcloud client
#eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
#export SSH_AUTH_SOCK=/run/user/1000/keyring/ssh
nextcloud &

## start owncloud client
export QT_QPA_PLATFORMTHEME=Adwaita-Dark # has to be called before owncloud, via zshrc is not enough!?
export QT_STYLE_OVERRIDE=Adwaita-Dark
owncloud &

## fire up mouse button to key stroke binder
xbindkeys
## fire up conky
conky -c ~/.config/conky/left.lua
conky -c ~/.config/conky/middle.lua
conky -c ~/.config/conky/right.lua

## switch to custom keyboard layout
setxkbmap -layout "us(m),de" # enables multiple layouts, switch via awesome, us(m) is custom (see notes)

## disable screen blanking and powersaving mode
xset s off -dpms

## no beeping
xset -b

## sound on, pulseaudio mutes front out of no reason
amixer -c 1 set Front 100% on

## composite manager to avoid screen tearing
picom -b

## power tray icon
#cbatticon -r 10 -c 'notify-send Power on 10%' & # dont forget the &

## start awesome
exec awesome
