#!/bin/bash

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

## OS-INDEPENDENT - PRE OS CHECK ##
if [[ -z "$EDITOR" ]]; then
    export EDITOR=/usr/bin/vim
fi

if [[ -z "$PAGER" ]]; then
    export PAGER=/bin/less
fi

if [[ -z "$VISUAL" ]]; then
    export VISUAL=/usr/bin/vim
fi

# XDG base spec.  OS-independent, theoretically.
if [ -z "${XDG_DATA_HOME}" ]; then
    mkdir -p "${HOME}/.local/share"
    XDG_DATA_HOME="${HOME}/.local/share"
    export XDG_DATA_HOME
fi

if [ -z "${XDG_CONFIG_HOME}" ]; then
    mkdir -p "${HOME}/.config"
    XDG_CONFIG_HOME="${HOME}/.config"
    export XDG_CONFIG_HOME
fi

if [ -z "${XDG_CACHE_HOME}" ]; then
    mkdir -p "${HOME}/.cache"
    XDG_CACHE_HOME="${HOME}/.cache"
    export XDG_CACHE_HOME
fi

if [[ -z "$XDG_DATA_DIRS" ]]; then
    export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
fi

# Set path.
mkdir -p "${HOME}/bin"
PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$HOME/bin

# Gopath.
mkdir -p "${HOME}/src/go"
GOPATH="${HOME}/src/go"
export GOPATH
PATH=$PATH:${GOPATH}/bin

################################################################################
#########################    OS DETECTION       ################################
################################################################################
if [ "$(uname -s)" == "Darwin" ]; then

    # We're on a mac.
    alias ls="ls -FG"

    # woo colors
    PS1="\[\033[01;32m\]\u\[\033[00m\]@\[\033[0;36m\]\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "


elif [ "$(uname -s)" == "Linux" ]; then

    # We're on linux.
    # enable color support of ls and also add handy aliases
    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls='ls --color=auto'
        alias dir='dir --color=auto'
        alias vdir='vdir --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi

    # This only needs to happen on linux.
    if [[ -z "$CABAL_CONFIG" ]]; then
        export CABAL_CONFIG=$XDG_CONFIG_HOME/cabal/config
    fi

    if [[ -z "$MPV_HOME" ]]; then
        export MPV_HOME=$XDG_CONFIG_HOME/mpv
    fi

    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

    # set variable identifying the chroot you work in (used in the prompt below)
    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        debian_chroot=$(cat /etc/debian_chroot)
    fi

    # set a fancy prompt (non-color, unless we know we "want" color)
    case "$TERM" in
        xterm-color) color_prompt=yes;;
    esac

    # uncomment for a colored prompt, if the terminal has the capability; turned
    # off by default to not distract the user: the focus in a terminal window
    # should be on the output of commands, not on the prompt
    force_color_prompt=yes

    if [ -n "$force_color_prompt" ]; then
        if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
        else
        color_prompt=
        fi
    fi

    if [ "$color_prompt" = yes ]; then
        PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[00m\]@\[\033[0;36m\]\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
    unset color_prompt force_color_prompt

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
    esac

    # Password-store completion
    source /etc/bash_completion.d/password-store

    if [[ -z "$DISPLAY" ]]; then
        export BROWSER=chromium
    fi

fi

# append to the history file, don't overwrite it
shopt -s histappend

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# System-specific stuff.
for f in ${XDG_CONFIG_HOME}/bash/extra/*; do
    if [ -f $f ]; then
        . $f
    fi
done

if [ -f ${XDG_CONFIG_HOME}/bash/bash_aliases ]; then
    . ${XDG_CONFIG_HOME}/bash/bash_aliases ]
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

