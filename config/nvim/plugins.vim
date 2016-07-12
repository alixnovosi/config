"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                             "
" FILE:    plugins.vim                                                                            "
" PURPOSE: Plugins used by (neo)vim.                                                              "
" UPDATED: 2016-06-23                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
call plug#begin("$XDG_DATA_HOME/nvim/site/plugins")

""" Language assistance.
Plug 'vim-scripts/a.vim',                {'for': ['c', 'cpp']}
Plug 'burnettk/vim-angular',             {'for': 'html'}
Plug 'matthewsimo/angular-vim-snippets', {'for': 'html'}
Plug 'Valloric/MatchTagAlways',          {'for': 'html'}
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json',            {'for': 'json'}
Plug 'ehamberg/vim-cute-python', {'for': 'python'}
Plug 'nvie/vim-flake8',          {'for': 'python'}
Plug 'ejholmes/vim-forcedotcom'
Plug 'sheerun/vim-polyglot'

""" General programming support.
Plug 'bronson/vim-trailing-whitespace'
Plug 'embear/vim-localvimrc'
Plug 'ervandew/supertab'
Plug 'junegunn/vim-easy-align'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'mileszs/ack.vim'
Plug 'scrooloose/syntastic'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'chriskempson/base16-vim'

""" File stuff/ things outside vim.
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-vinegar'
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}

call plug#end()
