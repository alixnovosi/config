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

xrdb -merge $HOME/.Xresources       # update x resources dib

# cursor
xsetroot -cursor_name left_ptr
unclutter &
xscreensaver -no-splash &

# Resources and screensaver
$HOME/bin/setWallpaper &

# PROGRAMS
pidgin --config=$XDG_CONFIG_HOME/pidgin &

# keychain
keychain $HOME/.ssh/id_rsa
[ -f $HOME/.keychain/$HOSTNAME-sh ] && . $HOME/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && . $HOME/.keychain/$HOSTNAME-sh-gpg 2>/dev/null
export SSH_ASKPASS=ssh-askpass

# MPD daemon start (if no other user instance exists)
[ ! -s $XDG_CONFIG_HOME/mpd/pid ] && mpd

# mpdscribble start if not already started
mpdscribble --conf $XDG_CONFIG_HOME/mpdscribble/config --log $XDG_CACHE_HOME/mpdscribble/mpdscribble.log &

# launch xmonad
exec ck-launch-session dbus-launch xmonad
