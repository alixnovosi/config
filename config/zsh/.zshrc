#!/bin/zsh
#-------------------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                             #
# FILE:    .zshrc                                                                                 #
# PURPOSE: zsh config file.                                                                       #
# UPDATED: 2015-08-02                                                                             #
# LICENSE: ISC                                                                                    #
#-------------------------------------------------------------------------------------------------#
# Sketchy way of detecting multiuser machine (where users is interesting).
if [[ "$(who | grep -c "$USER")" -ne "$(who | wc -l)" ]]; then

    # Display who else is logged in.
    users | tr ' ' '\n' | uniq | tr '\n' ' ' | awk '{print $0} END {print ""}'
fi
which fortune &> /dev/null && fortune -s

# Load any os-specific stuff.
if [[ "$(uname -s)" == "Darwin" ]]; then
    source "$HOME/Library/Preferences/shell/osx"
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

[[ "$(uname -s)" == "Linux" ]] && source "$HOME/.config/shell/linux"

# I could check if these exist, but I want zsh to let me know if I haven't created them.
files=('env' 'aliases' 'host')
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

# Act as if we used 'time' for any command that lasted longer than five seconds.
REPORTTIME=5

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
