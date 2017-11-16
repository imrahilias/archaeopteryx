#!/bin/sh
# ~/.xinitrc > m@archaeopteryx

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

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

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi


# start deamons
/usr/bin/urxvtd -q -f -o

# fire up conky
conky -c ~/.config/conky/left.lua
conky -c ~/.config/conky/right.lua
conky -c ~/.config/conky/middle.lua

# switch to custom keyboard layout
#setxkbmap us -variant umlaut

# enable compose keycodes *e.g. ralt+"+a = adiaeresis, Kills the variant (see above)!
#setxkbmap -option compose:ralt

# disable screen blanking and powersaving mode
xset s off -dpms

# start awesome
exec awesome

# composite manager to avoid screen tearing
compton # --config /dev/null --backend glx --paint-on-overlay --glx-no-stencil --glx-no-rebind-pixmap --glx-swap-method buffer-age --xrender-sync-fence --xrender-sync
