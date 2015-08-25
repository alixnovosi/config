"------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                      "
" FILE:    .nvimrc                                                             "
" PURPOSE: Neovim configuration file                                           "
" UPDATED: 2015-08-20                                                          "
" Free for use!                                                                "
"------------------------------------------------------------------------------"
"------------------------------------------------------------------------------"
" -------------------------  PLUG PREPARATION  ------------------------------- "
"------------------------------------------------------------------------------"
filetype plugin indent on
""" Prefer XDG_CONFIG_HOME/XDG_CACHE_HOME.
set backup backupdir=$XDG_CACHE_HOME/nvim/backup,/tmp
set dir=$XDG_CACHE_HOME/nvim/swap,/tmp
set viminfo+=n$XDG_CACHE_HOME/nvim/nviminfo
set runtimepath+=$XDG_CONFIG_HOME/nvim
set spellfile=$XDG_CACHE_HOME/nvim/spell/en.utf-8.add

"------------------------------------------------------------------------------"
" ----------------------------  PLUG BUNDLES  -------------------------------- "
"------------------------------------------------------------------------------"
call plug#begin('$XDG_CONFIG_HOME/vim/vundle')

" Language assistance.
Plug 'OmniSharp/omnisharp-vim'
Plug 'OrangeT/vim-csharp'
Plug 'dag/vim2hs'
Plug 'Twinside/vim-haskellFold'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
Plug 'fatih/vim-go'
Plug 'nsf/gocode', {'rtp': 'vim/'}
Plug 'tmhedberg/SimpylFold'
Plug 'davidhalter/jedi'
Plug 'derekwyatt/vim-scala'
Plug 'tejr/vim-tmux'
Plug 'dbakker/vim-lint'

" Programming support.
Plug 'majutsushi/tagbar'
Plug 'scrooloose/syntastic'
Plug 'tComment'
Plug 'tpope/vim-surround'
Plug 'Valloric/YouCompleteMe'
Plug 'embear/vim-localvimrc'

" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Appearance
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'

" File stuff/ things outside vim.
Plug 'jmcantrell/vim-virtualenv'
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle' }
Plug 'tpope/vim-dispatch'
Plug 'vim-scripts/a.vim'

call plug#end()

"------------------------------------------------------------------------------"
" ----------------------  NON-VUNDLE SETTINGS  ------------------------------- "
"------------------------------------------------------------------------------"

""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8
set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd nofoldenable textwidth=80
set spell spelllang=en_us
set wildmenu
set wildmode=list:longest,full

let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tagbar#enabled = 1

let g:go_bin_path = expand("$XDG_CONFIG_HOME/nvim/vim-go")

syntax enable
set background=dark
colorscheme solarized

""" Enable mouse and enable word wrapping.
set mouse=a ww=[,],<,>,h,l,b,s

"------------------------------------------------------------------------------"
" -------------------------------  KEYBINDS  --------------------------------- "
"------------------------------------------------------------------------------"
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

""" Show commamore useful information.
set ruler showcmd

""" Let C-s, C-q go to Vim instead of terminal.
silent !stty -ixon > /dev/null 2>/dev/null
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>h :set hlsearch!<CR>
nmap <C-s>t :TagbarToggle<CR>

""" Set nicer search settings.
set incsearch ignorecase smartcase nohlsearch

""" Reload if a file is changed on us.
set autoread
