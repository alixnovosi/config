"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - drew.life                                                             "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim base config file.                                                             "
" UPDATED: 2019-04-08                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
source $XDG_CONFIG_HOME/nvim/plugins.vim
source $XDG_CONFIG_HOME/nvim/settings.vim
source $XDG_CONFIG_HOME/nvim/mappings.vim
if filereadable(expand('$XDG_CONFIG_HOME/nvim/host.vim'))
    source $XDG_CONFIG_HOME/nvim/host.vim
endif

augroup FiletypeGroup
    autocmd!
    au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
augroup END
