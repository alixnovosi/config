"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                             "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim configuration file.                                                           "
" UPDATED: 2016-10-18                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
source $XDG_CONFIG_HOME/nvim/plugins.vim
source $XDG_CONFIG_HOME/nvim/settings.vim
source $XDG_CONFIG_HOME/nvim/mappings.vim
if filereadable(expand('$XDG_CONFIG_HOME/nvim/host.vim'))
    source $XDG_CONFIG_HOME/nvim/host.vim
endif
