"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - drew.life                                                             "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim base config file.                                                             "
" UPDATED: 2019-04-13                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
if empty(expand('$XDG_DATA_HOME/nvim/site/autoload/plug.vim'))
  silent !curl -fLo expand('$XDG_DATA_HOME/nvim/site/autoload/plug.vim') --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source expand('$XDG_CONFIG_HOME/nvim/init.vim')
endif

source $XDG_CONFIG_HOME/nvim/plugins.vim
source $XDG_CONFIG_HOME/nvim/settings.vim
source $XDG_CONFIG_HOME/nvim/mappings.vim
source $XDG_CONFIG_HOME/nvim/theme.vim
if filereadable(expand('$XDG_CONFIG_HOME/nvim/host.vim'))
    source $XDG_CONFIG_HOME/nvim/host.vim
endif

augroup FiletypeGroup
    autocmd!
    au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
augroup END
