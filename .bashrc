#!/bin/bash

# Set up aliases and stuff.
if [ -z ${XDG_CONFIG_HOME+x} ]; then
    XDG_CONFIG_HOME="$HOME/.config"
    . $XDG_CONFIG_HOME/shell/env
fi

. $XDG_CONFIG_HOME/shell/aliases
. $XDG_CONFIG_HOME/shell/host
. $XDG_CONFIG_HOME/shell/func
. $XDG_CONFIG_HOME/shell/env

################################################################################
#########################    OS DETECTION       ################################
################################################################################
if [ "$(uname -s)" == "Darwin" ]; then
    . $XDG_CONFIG_HOME/shell/osx
elif [ "$(uname -s)" == "Linux" ]; then
    . $XDG_CONFIG_HOME/shell/linux
fi

if [[ $(tput colors) -ge 256 ]]; then
    BASE03=$(tput setaf 234)
    BASE02=$(tput setaf 235)
    BASE01=$(tput setaf 240)
    BASE00=$(tput setaf 241)
    BASE0=$(tput setaf 244)
    BASE1=$(tput setaf 245)
    BASE2=$(tput setaf 254)
    BASE3=$(tput setaf 230)
    YELLOW=$(tput setaf 136)
    ORANGE=$(tput setaf 166)
    RED=$(tput setaf 160)
    MAGENTA=$(tput setaf 125)
    VIOLET=$(tput setaf 61)
    BLUE=$(tput setaf 33)
    CYAN=$(tput setaf 37)
    GREEN=$(tput setaf 64)
else
    BASE03=$(tput setaf 8)
    BASE02=$(tput setaf 0)
    BASE01=$(tput setaf 10)
    BASE00=$(tput setaf 11)
    BASE0=$(tput setaf 12)
    BASE1=$(tput setaf 14)
    BASE2=$(tput setaf 7)
    BASE3=$(tput setaf 15)
    YELLOW=$(tput setaf 3)
    ORANGE=$(tput setaf 9)
    RED=$(tput setaf 1)
    MAGENTA=$(tput setaf 5)
    VIOLET=$(tput setaf 13)
    BLUE=$(tput setaf 4)
    CYAN=$(tput setaf 6)
    GREEN=$(tput setaf 2)
fi

PS1="\[$BOLD$BASE3\]\u\[$BASE0\]@\[$CYAN\]\h\[$BASE0\]:\[$VIOLET\]\W\[$BASE0\]\$ \[$RESET\]"

# transparent xterm?
[ -n "$XTERM_VERSION" ] && transset -a 45 > /dev/null

# append to the history file, don't overwrite it
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s histappend checkwinsize

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# System-specific stuff.
if [ -f $XDG_CACHE_HOME/shell/host ]; then
    . $XDG_CONFIG_HOME/shell/host
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

