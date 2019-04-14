#!/usr/bin/env bash
#-------------------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud                                                                         #
# FILE:    colors.sh                                                                              #
# PURPOSE: color-related environment variable generator                                           #
# UPDATED: 2019-04-13                                                                             #
# LICENSE: ISC                                                                                    #
#-------------------------------------------------------------------------------------------------#
# TODO I don't know if this should live here.
# Base16 Shell
# shellcheck source=/dev/null
source "$XDG_CONFIG_HOME/base16-shell/shell-current.sh"

OS=$(uname -s)
if [ "$OS" = "FreeBSD" ]; then
    TPUT_ARGS="AF"
else
    TPUT_ARGS="setaf"
fi

# tput color nonsense. Use 256 colors, if we have them.
# Technically the assignment could error out or something, so we export then assign.
if [ "$(tput colors)" = "256" ] || [ "$OS" = "FreeBSD" ]; then
    export P_BASE03 && P_BASE03="$(tput $TPUT_ARGS 234)"
    export P_BASE02 && P_BASE02="$(tput $TPUT_ARGS 235)"
    export P_BASE01 && P_BASE01="$(tput $TPUT_ARGS 240)"
    export P_BASE00 && P_BASE00="$(tput $TPUT_ARGS 241)"
    export P_BASE0 && P_BASE0="$(tput $TPUT_ARGS 244)"
    export P_BASE1 && P_BASE1="$(tput $TPUT_ARGS 245)"
    export P_BASE2 && P_BASE2="$(tput $TPUT_ARGS 254)"
    export P_BASE3 && P_BASE3="$(tput $TPUT_ARGS 230)"
    export P_YELLOW && P_YELLOW="$(tput $TPUT_ARGS 136)"
    export P_ORANGE && P_ORANGE="$(tput $TPUT_ARGS 166)"
    export P_RED && P_RED="$(tput $TPUT_ARGS 160)"
    export P_MAGENTA && P_MAGENTA="$(tput $TPUT_ARGS 125)"
    export P_VIOLET && P_VIOLET="$(tput $TPUT_ARGS 61)"
    export P_BLUE && P_BLUE="$(tput $TPUT_ARGS 33)"
    export P_CYAN && P_CYAN="$(tput $TPUT_ARGS 37)"
    export P_GREEN && P_GREEN="$(tput $TPUT_ARGS 64)"

# Fine, only 16 colors.
else
    export P_BASE03 && P_BASE03="$(tput $TPUT_ARGS 8)"
    export P_BASE02 && P_BASE02="$(tput $TPUT_ARGS 0)"
    export P_BASE01 && P_BASE01="$(tput $TPUT_ARGS 10)"
    export P_BASE00 && P_BASE00="$(tput $TPUT_ARGS 11)"
    export P_BASE0 && P_BASE0="$(tput $TPUT_ARGS 12)"
    export P_BASE1 && P_BASE1="$(tput $TPUT_ARGS 14)"
    export P_BASE2 && P_BASE2="$(tput $TPUT_ARGS 7)"
    export P_BASE3 && P_BASE3="$(tput $TPUT_ARGS 15)"
    export P_YELLOW && P_YELLOW="$(tput $TPUT_ARGS 3)"
    export P_ORANGE && P_ORANGE="$(tput $TPUT_ARGS 9)"
    export P_RED && P_RED="$(tput $TPUT_ARGS 1)"
    export P_MAGENTA && P_MAGENTA="$(tput $TPUT_ARGS 5)"
    export P_VIOLET && P_VIOLET="$(tput $TPUT_ARGS 13)"
    export P_BLUE && P_BLUE="$(tput $TPUT_ARGS 4)"
    export P_CYAN && P_CYAN="$(tput $TPUT_ARGS 6)"
    export P_GREEN && P_GREEN="$(tput $TPUT_ARGS 2)"
fi

# Do this in any case.
# ugh
if [ "$OS" = "FreeBSD" ]; then
    export P_RESET && P_RESET="$(tput me)"
else
    export P_RESET && P_RESET="$(tput sgr0)"
fi
