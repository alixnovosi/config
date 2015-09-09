"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    config.vim                                                                             "
" PURPOSE: Vim configuration file                                                                 "
" UPDATED: 2015-08-26                                                                             "
" LICENSE: MIT/BSD                                                                                "
"-------------------------------------------------------------------------------------------------"

"-------------------------------------------------------------------------------------------------"
" ----------------------------------  PLUGIN PREPARATION  --------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Prefer XDG_CONFIG_HOME/XDG_CACHE_HOME, and don't be vi.
set nocompatible runtimepath+=$XDG_CONFIG_HOME/vim,$XDG_CACHE_HOME/vim
filetype plugin indent on
syntax enable
set backup backupdir=$XDG_DATA_HOME/vim/backup dir=$XDG_DATA_HOME/vim/swap
set undodir=$XDG_DATA_HOME/vim/undo undofile undolevels=1000 undoreload=10000
set viminfo+=n$XDG_DATA_HOME/vim/viminfo

""" nvim-/Windows-specific stuff
if !has("nvim")
    set spell spelllang=en_us spellfile=$XDG_CACHE_HOME/vim/spell/en-utf-8.add
endif

if has("win32") || has("win16")
    """ Always prefer unix line endings.
    set fileformats=unix,dos
endif

"-------------------------------------------------------------------------------------------------"
" --------------------------------------  PLUGINS  ---------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
call plug#begin('$XDG_CACHE_HOME/vim/plugins')

""" Language assistance.
Plug 'OmniSharp/omnisharp-vim',  {'for': 'csharp'}
Plug 'OrangeT/vim-csharp',       {'for': 'csharp'}
Plug 'fatih/vim-go',             {'for': 'go'}
Plug 'nsf/gocode',               {'rtp': 'vim/', 'for': 'go'}
Plug 'dag/vim2hs',               {'for': 'haskell'}
Plug 'othree/html5.vim',         {'for': 'html'}
Plug 'pangloss/vim-javascript',  {'for': 'javascript'}
Plug 'elzr/vim-json',            {'for': 'json'}
Plug 'darfink/vim-plist'
Plug 'rodjek/vim-puppet',        {'for': 'puppet'}
Plug 'klen/python-mode',         {'for': 'python'}
Plug 'derekwyatt/vim-scala',     {'for': 'scala'}
Plug 'tejr/vim-tmux',            {'for': 'tmux'}
Plug 'dbakker/vim-lint',         {'for': 'vimscript'}
Plug 'tpope/vim-endwise'

""" Programming support.
Plug 'embear/vim-localvimrc'
Plug 'junegunn/vim-easy-align'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tComment'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe'

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'
Plug 'flazz/vim-colorschemes'

""" File stuff/ things outside vim.
Plug 'jmcantrell/vim-virtualenv'
Plug 'scrooloose/nerdtree',         {'on': 'NERDTreeToggle'}
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'vim-scripts/a.vim',           {'for': ['c', 'cpp']}
call plug#end()

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  SETTINGS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8

""" Personal preferences (space >> tabs, modelines scary, folds often annoying).
set et ts=4 softtabstop=4 shiftwidth=4 textwidth=99 nomodeline nofoldenable colorcolumn=100
colorscheme solarized

""" Make opening files not suck.
set wildmenu wildmode=list:longest,full

let g:EclimCompletionMethod = 'omnifunc'

""" Airline preferences.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#eclim#enabled = 1
let g:airline_extensions = ["hunks", "syntastic", "tagbar", "tabline"]
let g:airline_inactive_collapse=1

""" Enable mouse and enable nice cursor wrapping, use 2h status for airline, show commands.
""" Set nicer search settings, and reload if a file is changed on us.
set mouse=a whichwrap=[,],<,>,h,l,b,s laststatus=2 showcmd noshowmode
set incsearch ignorecase smartcase nohlsearch autoread ttimeoutlen=50

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  KEYBINDS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Fast escape.
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

""" Let C-s, C-q go to Vim instead of terminal, and define some commands using C-s.
silent !stty -ixon > /dev/null 2>/dev/null
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>r :set relativenumber!<CR>
nmap <C-s>h :set hlsearch!<CR>
nmap <C-s>t :TagbarToggle<CR>
nmap <C-s>u :UndotreeToggle<CR>

""" Become a better person.
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
