#!/bin/zsh
#-------------------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud                                                                         #
#          https://andrewmichaud.com                                                              #
# FILE:    .zshrc                                                                                 #
# PURPOSE: zsh config file.                                                                       #
# UPDATED: 2015-08-27                                                                             #
# LICENSE: MIT/BSD                                                                                #
#-------------------------------------------------------------------------------------------------#

# This is the only thing I can think of that needs to be done every session instead of once,
# which is why my zshenv is so full and this so empty.
which fortune &> /dev/null && fortune -s

# Load any os-specific stuff.
[[ "$(uname -s)" == "Darwin" ]] && source "$HOME/Library/Preferences/shell/osx"
[[ "$(uname -s)" == "Linux" ]] && source "$HOME/.config/shell/linux"

# I could check if these exist, but I want zsh to let me know if I haven't created them.
files=('env' 'aliases' 'func' 'host')
for file in $files; do source "$XDG_CONFIG_HOME/shell/$file"; done
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Enable 256 color capabilities for appropriate terminals.
if [ -n "$SEND_256_COLORS_TO_REMOTE" ]; then

    case "$TERM" in
        'xterm') export TERM=xterm-256color;;
        'screen') export TERM=screen-256color;;
    esac

    if [ -n "$TERMCAP" ] && [ "$TERM" = "screen-256color" ]; then
        export TERMCAP="${TERMCAP//Co#8/Co#256/}"
    fi
fi

# Better completion.
# Tab completion from both ends, case-insensitive completion, better killall completion.
fpath=($XDG_CONFIG_HOME/zsh/completion $fpath)
autoload -U compinit && compinit
setopt completeinword
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:killall:*' command "ps -u $USER -o cmd"

# Move and expand history.
HISTFILE="$XDG_CACHE_HOME/$(basename "$SHELL")/history"
HISTSIZE=SAVEHIST=10000
setopt sharehistory extendedhistory

# Superglob, and cd into directory without cd.
setopt extendedglob nocaseglob auto_cd

bindkey '^R' history-incremental-search-backward
