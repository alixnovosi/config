# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# XDG base spec.  OS-independent, theoretically.
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/

export EDITOR="/usr/bin/vim -u $XDG_CONFIG_HOME/vim/.vimrc"
export PAGER="/bin/less"
export VISUAL="$EDITOR"

export HISTFILE="$XDG_CACHE_HOME/bash/history"
export LESSHISTFILE="$XDG_CACHE_HOME/less/hist"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	source "$HOME/.bashrc"
    fi
fi

