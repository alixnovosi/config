"--------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                          "
" FILE:    config.vim                                                                              "
" PURPOSE: Vim configuration file                                                                  "
" UPDATED: 2015-08-25                                                                              "
" Free for use! (MIT/BSD license, GPL is plague.)                                                  "
"--------------------------------------------------------------------------------------------------"

"--------------------------------------------------------------------------------------------------"
" ----------------------------------  PLUGIN PREPARATION  ---------------------------------------- "
"--------------------------------------------------------------------------------------------------"
""" Prefer XDG_CONFIG_HOME/XDG_CACHE_HOME, and don't be vi.
filetype plugin indent on
syntax enable
set nocompatible
set backup backupdir=$XDG_CACHE_HOME/vim/back,/tmp directory=$XDG_CACHE_HOME/vim/swp,/tmp
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo runtimepath+=$XDG_CONFIG_HOME/vim,$XDG_CACHE_HOME/vim
let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim-go")

"--------------------------------------------------------------------------------------------------"
" ----------------------------------------  PLUGINS  --------------------------------------------- "
"--------------------------------------------------------------------------------------------------"
call plug#begin('$XDG_CACHE_HOME/vim/plugins')

""" Language assistance.
Plug 'OmniSharp/omnisharp-vim',   { 'for': 'csharp' }
Plug 'OrangeT/vim-csharp',        { 'for': 'csharp' }
Plug 'fatih/vim-go',              { 'for': 'go' }
Plug 'nsf/gocode',                { 'rtp': 'vim/', 'for': 'go' }
Plug 'dag/vim2hs',                { 'for': 'haskell' }
Plug 'Twinside/vim-haskellFold',  { 'for': 'haskell' }
Plug 'othree/html5.vim',          { 'for': 'html' }
Plug 'pangloss/vim-javascript',   { 'for': 'javascript' }
Plug 'elzr/vim-json',             { 'for': 'json' }
Plug 'rodjek/vim-puppet',         { 'for': 'puppet' }
Plug 'tmhedberg/SimpylFold',      { 'for': 'python' }
Plug 'davidhalter/jedi',          { 'for': 'python' }
Plug 'derekwyatt/vim-scala',      { 'for': 'scala' }
Plug 'tejr/vim-tmux',             { 'for': 'tmux' }
Plug 'dbakker/vim-lint',          { 'for': 'vimscript' }

""" Programming support.
Plug 'embear/vim-localvimrc'
Plug 'junegunn/vim-easy-align'
Plug 'majutsushi/tagbar'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tComment'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe',    { 'do': './install.sh --clang-completer --gocode-completer' }

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'

""" File stuff/ things outside vim.
Plug 'jmcantrell/vim-virtualenv'
Plug 'scrooloose/nerdtree',      {'on': 'NERDTreeToggle' }
Plug 'tpope/vim-dispatch'
Plug 'vim-scripts/a.vim',        {'for': ['c', 'cpp'] }
call plug#end()

"--------------------------------------------------------------------------------------------------"
" --------------------------------  NON-VUNDLE SETTINGS  ----------------------------------------- "
"--------------------------------------------------------------------------------------------------"
""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8

""" Personal preferences.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4 nomodeline showcmd nofoldenable textwidth=100
set spell spelllang=en_us spellfile=$XDG_CACHE_HOME/vim/spell/en-utf-8.add
set wildmenu wildmode=list:longest,full background=dark
colorscheme solarized

""" Airline preferences.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_extensions = ["tagbar", "tabline", "syntastic", "hunks",]

""" Enable mouse and enable word wrapping, and make a 2-line status bar so stuff works.
""" Set nicer search settings, and reload if a file is changed on us.
set mouse=a whichwrap=[,],<,>,h,l,b,s laststatus=2 ruler showcmd
set incsearch ignorecase smartcase nohlsearch autoread

"--------------------------------------------------------------------------------------------------"
" -----------------------------------------  KEYBINDS  ------------------------------------------- "
"--------------------------------------------------------------------------------------------------"
""" Fast escape.
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

""" Let C-s, C-q go to Vim instead of terminal.
silent !stty -ixon > /dev/null 2>/dev/null
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>h :set hlsearch!<CR>
nmap <C-s>t :TagbarToggle<CR>
