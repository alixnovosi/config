"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - andrewmichaud.com                                                     "
" FILE:    init.vim                                                                               "
" PURPOSE: vim configuration file.                                                                "
" UPDATED: 2017-03-23                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Load defaults.vim file (requires vim > 8.0).
source $VIMRUNTIME/defaults.vim

""" Load file changed under us, force UTF-8.
set autoread encoding=utf-8

""" Format text automatically in a nice way, use largest history, use smarttab.
""" Use 2-high status line (for airline).
set formatoptions+=tcqj history=10000 smarttab laststatus=2

""" Set vim-specific sequences for RGB colors, to prevent truecolor breaking in vim.
""" https://github.com/vim/vim/issues/993#issuecomment-255651605
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

""" Make opening files not suck, make search better, be faster, indent.
set wildmode=list:longest,full ttyfast autoindent

""" Use XDG dirs. Neovim does this by default. dir==swap dir.
set viminfo+=n$XDG_DATA_HOME/vim/viminfo dir=$XDG_DATA_HOME/vim/swap//
set runtimepath+=$XDG_CONFIG_HOME/vim,$XDG_DATA_HOME/vim/site
set undodir=$XDG_DATA_HOME/vim/undo

""" I don't know if neovim sets this by default, assuming.
let g:netrw_home=$XDG_DATA_HOME.'/vim'

""" Share config with neovim. Will eventually make it easy to get rid of plain vim forever.
source $XDG_CONFIG_HOME/nvim/init.vim
