#!/bin/zsh

# Show fortune on first login.
if [ -z ${FIRSTLOGIN+x} ]; then
    export FIRSTLOGIN="foo"
    # Sketchy way of detecting multiuser machine (where users is interesting).
    if [[ "$(who | grep -c "$USER")" -ne "$(who | wc -l)" ]]; then

        # display who else is logged in
        users|tr ' ' '\n'|uniq|tr '\n' ' '|awk '{print $0} END {print ""}'
    fi

    which fortune &> /dev/null && fortune -s
fi

# Load any os-specific stuff.
if [[ "$(uname -s)" == "Darwin" ]]; then
    . "$HOME/Library/Preferences/shell/osx"
elif [[ "$(uname -s)" == "Linux" ]]; then
    . "$HOME/.config/shell/linux"
fi

. "$XDG_CONFIG_HOME/shell/env"
. "$XDG_CONFIG_HOME/shell/aliases"
. "$XDG_CONFIG_HOME/shell/func"

# Enable 256 color capabilities for appropriate terminals
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

# Better completion
# Tab completion from both ends.
# Case-insensitive
# Better killall completion.
fpath=($XDG_CONFIG_HOME/zsh/completion $fpath)
autoload -U compinit
compinit
setopt completeinword
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
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

eval "$(thefuck-alias)"

# Prompt.
export PROMPT="%{$P_CYAN%}%n%{$P_RESET%}@%{$P_GREEN%}%m%{$P_RESET%}:%{$P_BLUE%}%~%{$P_RESET%}# "

# Load any host-specific stuff.
if [ -f "$XDG_CONFIG_HOME/shell/host" ]; then
    . "$XDG_CONFIG_HOME/shell/host"
fi
