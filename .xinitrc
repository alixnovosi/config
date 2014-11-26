#!/usr/bin/env bash

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

if [ -d /etc/X11/xinit/xinitrc.d ]; then
        for f in /etc/X11/xinit/xinitrc.d/*; do
               [ -x "$f" ] && . "$f"
        done
        unset f
fi

source $HOME/.profile

 # update x resources dib
[[ -f $XDG_CONFIG_HOME/xresources/.Xresources ]] && xrdb -merge $XDG_CONFIG_HOME/xresources/.Xresources

# cursor
xsetroot -cursor_name left_ptr
unclutter &
xscreensaver -no-splash &

# Resources and screensaver
$HOME/bin/setWallpaper &

xfce4-power-manager &



# PROGRAMS
pidgin --config=$XDG_CONFIG_HOME/pidgin &

# fix screens
$HOME/bin/fixscreens &

# MPD daemon start (if no other user instance exists)
[ ! -s $XDG_CONFIG_HOME/mpd/pid ] && mpd

# mpdscribble start if not already started
[ ! $(pgrep -x mpdscribble) ] && mpdscribble --conf $XDG_CONFIG_HOME/mpdscribble/config --log $XDG_CACHE_HOME/mpdscribble/mpdscribble.log &

export SSH_ASKPASS=ssh-askpass

# launch xmonad
exec /usr/bin/ssh-agent ck-launch-session dbus-launch xmonad
