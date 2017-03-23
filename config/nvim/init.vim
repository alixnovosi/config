"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - andrewmichaud.com                                                     "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim base config file.                                                             "
" UPDATED: 2017-03-23                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
source $XDG_CONFIG_HOME/nvim/plugins.vim
source $XDG_CONFIG_HOME/nvim/settings.vim
source $XDG_CONFIG_HOME/nvim/mappings.vim
if filereadable(expand('$XDG_CONFIG_HOME/nvim/host.vim'))
    source $XDG_CONFIG_HOME/nvim/host.vim
endif
