#!/bin/sh

# update x resources dib
[ -f .Xresources ] && xrdb -merge .Xresources

# cursor
xsetroot -cursor_name left_ptr
unclutter &
xscreensaver -no-splash &

# Resources and screensaver
[ -f $HOME/bin/setWallpaper ] && $HOME/bin/setWallpaper &

# PROGRAMS
pidgin --config=$HOME/.config/pidgin &

# fix screens
[ -f $HOME/bin/fixscreens ] && $HOME/bin/fixscreens &

# MPD daemon start (if no other user instance exists)
[ ! -s $HOME/.config/mpd/pid ] && mpd

# mpdscribble start if not already started
[ ! $(pgrep -x mpdscribble) ] && mpdscribble --conf $HOME/.config/mpdscribble/config --log $HOME/.config/mpdscribble/mpdscribble.log &

export SSH_ASKPASS=ssh-askpass

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)

eval `dbus-launch --auto-syntax`

taffybar &

exec /usr/bin/ssh-agent ck-launch-session xmonad
