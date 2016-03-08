"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    plugins.vim                                                                            "
" PURPOSE: Plugins used by (neo)vim                                                               "
" UPDATED: 2016-03-08                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
call plug#begin("$XDG_DATA_HOME/nvim/site/plugins")

""" Language assistance.
Plug 'calebsmith/vim-lambdify'
Plug 'vim-scripts/a.vim',                      {'for': ['c', 'cpp']}
Plug 'OmniSharp/omnisharp-vim',                {'for': 'csharp'}
Plug 'OrangeT/vim-csharp',                     {'for': 'csharp'}
Plug 'alecthomas/gometalinter',                {'for': 'go'}
Plug 'fatih/vim-go',                           {'for': 'go'}
Plug 'enomsg/vim-haskellConcealPlus',          {'for': 'haskell'}
Plug 'burnettk/vim-angular',                   {'for': 'html'}
Plug 'matthewsimo/angular-vim-snippets',       {'for': 'html'}
Plug 'Valloric/MatchTagAlways',                {'for': 'html'}
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json',                          {'for': 'json'}
Plug 'rodjek/vim-puppet',                      {'for': 'puppet'}
Plug 'davidhalter/jedi-vim',                   {'for': 'python'}
Plug 'ehamberg/vim-cute-python',               {'for': 'python'}
Plug 'nvie/vim-flake8',                        {'for': 'python'}
Plug 'derekwyatt/vim-scala',                   {'for': 'scala'}
Plug 'keith/tmux.vim'

""" General programming support.
Plug 'bronson/vim-trailing-whitespace'
Plug 'embear/vim-localvimrc'
Plug 'ervandew/supertab'
Plug 'junegunn/vim-easy-align'
Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline-themes'

""" File stuff/ things outside vim.
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-vinegar'
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}

call plug#end()
