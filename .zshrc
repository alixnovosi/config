#!/bin/zsh

# Only show fortune for the first time I log in.
if [[ "$(who | grep -c "$USER")" -le 1 ]]; then

    # Only show users if I'm not the only one logged in.
    if [[ "$(who | grep -c "$USER")" -ne "$(who | wc -l)" ]]; then

        # display who else is logged in
        users|tr ' ' '\n'|uniq|tr '\n' ' '|awk '{print $0} END {print ""}'
    fi

    # fortune of the day - short message to brighten up your login
    which fortune &> /dev/null && fortune -s
fi

# Load stuff.
. "$HOME/.config/shell/env"
. "$XDG_CONFIG_HOME/shell/aliases"
. "$XDG_CONFIG_HOME/shell/func"

# Load any os-specific stuff.
if [[ "$(uname -s)" == "Darwin" ]]; then
    . "$XDG_CONFIG_HOME/shell/osx"
elif [[ "$(uname -s)" == "Linux" ]]; then
    . "$XDG_CONFIG_HOME/shell/linux"
fi

# Enable 256 color capabilities for appropriate terminals

# Terminals with any of the following set, support 256 colors (and are local)
local256="$COLORTERM$XTERM_VERSION"

if [ -n "$local256" ] || [ -n "$SEND_256_COLORS_TO_REMOTE" ]; then

  case "$TERM" in
    'xterm') TERM=xterm-256color;;
    'screen') TERM=screen-256color;;
  esac
  export TERM

  if [ -n "$TERMCAP" ] && [ "$TERM" = "screen-256color" ]; then
    export TERMCAP="${TERMCAP//Co#8/Co#256/}"
  fi
fi

unset local256

export GOPATH=$HOME/src/go
export PATH=$PATH:$GOPATH/bin

# Options.
# Better completion
autoload -U compinit
compinit

# Tab completion from both ends.
setopt completeinword

# Case-insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Better killall completion.
zstyle ':completion:*:killall:*' command "ps -u $USER -o cmd"

# Move and expand history.
HISTFILE="$XDG_CACHE_HOME/$(basename "$SHELL")/history"
HISTSIZE=SAVEHIST=10000
setopt sharehistory
setopt extendedhistory

# Superglob
setopt extendedglob
unsetopt caseglob

# Type directory without needing cd
setopt auto_cd
bindkey '^R' history-incremental-search-backward

# Prompt.
PROMPT="%{$P_CYAN%}%n%{$P_RESET%}@%{$P_GREEN%}%m%{$P_RESET%}:%{$P_BLUE%}%~%{$P_RESET%}# "

# Load any host-specific stuff.
if [ -f "$XDG_CONFIG_HOME/shell/host" ]; then
    . "$XDG_CONFIG_HOME/shell/host"
fi
