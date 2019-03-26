"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - andrewmichaud.com                                                     "
" FILE:    plugins.vim                                                                            "
" PURPOSE: Plugins used by (neo)vim (all).                                                        "
" UPDATED: 2019-02-26                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
call plug#begin('$XDG_DATA_HOME/nvim/site/plugins')

""" Language assistance.
Plug 'vim-scripts/a.vim',                {'for': ['c', 'cpp']}
Plug 'Valloric/MatchTagAlways',          {'for': 'html'}
Plug 'ehamberg/vim-cute-python',         {'for': 'python'}
Plug 'nvie/vim-flake8',                  {'for': 'python'}
Plug 'gcorne/vim-sass-lint',             {'for': 'sass'}
Plug 'ejholmes/vim-forcedotcom'
Plug 'sheerun/vim-polyglot'
Plug 'justinj/vim-pico8-syntax'
Plug 'evanrelf/vim-pico8-color'
Plug 'editorconfig/editorconfig-vim'

""" General programming support.
Plug 'bronson/vim-trailing-whitespace'
Plug 'embear/vim-localvimrc'
Plug 'ervandew/supertab'
Plug 'junegunn/vim-easy-align'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'mileszs/ack.vim'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'w0rp/ale'

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'morhetz/gruvbox'

""" File stuff/ things outside vim.
Plug 'scrooloose/nerdtree',         {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-vinegar'
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}

call plug#end()
