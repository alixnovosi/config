#!/bin/bash #-------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud                                                                         #
# FILE:    colors.sh                                                                              #
# PURPOSE: color-related environment variable generator                                           #
# UPDATED: 2016-03-09                                                                             #
# LICENSE: ISC                                                                                    #
#-------------------------------------------------------------------------------------------------#
BASE16_SHELL="$XDG_DATA_HOME/bin/base16-shell/base16-ocean.dark.sh"
[[ -s "$BASE16_SHELL" ]] && source "$BASE16_SHELL"

# tput color nonsense. Use 256 colors, if we have them.
# Technically the assignment could error out or something, so we export then assign.
if [ "$(tput colors)" = "256" ]; then
    export P_BASE03 && P_BASE03="$(tput setaf 234)"
    export P_BASE02 && P_BASE02="$(tput setaf 235)"
    export P_BASE01 && P_BASE01="$(tput setaf 240)"
    export P_BASE00 && P_BASE00="$(tput setaf 241)"
    export P_BASE0 && P_BASE0="$(tput setaf 244)"
    export P_BASE1 && P_BASE1="$(tput setaf 245)"
    export P_BASE2 && P_BASE2="$(tput setaf 254)"
    export P_BASE3 && P_BASE3="$(tput setaf 230)"
    export P_YELLOW && P_YELLOW="$(tput setaf 136)"
    export P_ORANGE && P_ORANGE="$(tput setaf 166)"
    export P_RED && P_RED="$(tput setaf 160)"
    export P_MAGENTA && P_MAGENTA="$(tput setaf 125)"
    export P_VIOLET && P_VIOLET="$(tput setaf 61)"
    export P_BLUE && P_BLUE="$(tput setaf 33)"
    export P_CYAN && P_CYAN="$(tput setaf 37)"
    export P_GREEN && P_GREEN="$(tput setaf 64)"

# Fine, only 16 colors.
else
    export P_BASE03 && P_BASE03="$(tput setaf 8)"
    export P_BASE02 && P_BASE02="$(tput setaf 0)"
    export P_BASE01 && P_BASE01="$(tput setaf 10)"
    export P_BASE00 && P_BASE00="$(tput setaf 11)"
    export P_BASE0 && P_BASE0="$(tput setaf 12)"
    export P_BASE1 && P_BASE1="$(tput setaf 14)"
    export P_BASE2 && P_BASE2="$(tput setaf 7)"
    export P_BASE3 && P_BASE3="$(tput setaf 15)"
    export P_YELLOW && P_YELLOW="$(tput setaf 3)"
    export P_ORANGE && P_ORANGE="$(tput setaf 9)"
    export P_RED && P_RED="$(tput setaf 1)"
    export P_MAGENTA && P_MAGENTA="$(tput setaf 5)"
    export P_VIOLET && P_VIOLET="$(tput setaf 13)"
    export P_BLUE && P_BLUE="$(tput setaf 4)"
    export P_CYAN && P_CYAN="$(tput setaf 6)"
    export P_GREEN && P_GREEN="$(tput setaf 2)"
fi

# Do this in any case.
export P_RESET && P_RESET="$(tput sgr0)"
