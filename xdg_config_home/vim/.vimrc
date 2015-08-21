"------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                      "
" FILE:    .vimrc                                                              "
" PURPOSE: Vim configuration file                                              "
" DATE:    2015-07-23                                                          "
" UPDATED: 2015-08-20                                                          "
" Free for use!                                                                "
"------------------------------------------------------------------------------"
"------------------------------------------------------------------------------"
" -------------------------  VUNDLE PREPARATION  ----------------------------- "
"------------------------------------------------------------------------------"
set nocompatible
filetype off
set runtimepath+=$XDG_CONFIG_HOME/vim,$XDG_CONFIG_HOME/vim/vundle/Vundle.vim

let g:vundle_lazy_load=0
let g:bundles_dir = expand('$XDG_CONFIG_HOME/vim/vundle')

"------------------------------------------------------------------------------"
" ---------------------------  VUNDLE BUNDLES  ------------------------------- "
"------------------------------------------------------------------------------"
call vundle#rc(bundles_dir)

Bundle 'gmarik/Vundle.vim'

" Language assistance.
Bundle 'OmniSharp/omnisharp-vim'
Bundle 'OrangeT/vim-csharp'
Bundle 'dag/vim2hs'
Bundle 'Twinside/vim-haskellFold'
Plugin 'othree/html5.vim'
Plugin 'pangloss/vim-javascript'
Bundle 'elzr/vim-json'
Plugin 'fatih/vim-go'
Plugin 'nsf/gocode', {'rtp': 'vim/'}
Plugin 'tmhedberg/SimpylFold'
Plugin 'davidhalter/jedi'
Bundle 'derekwyatt/vim-scala'
Plugin 'tejr/vim-tmux'
Bundle 'dbakker/vim-lint'

" Programming support.
Plugin 'majutsushi/tagbar'
Bundle 'scrooloose/syntastic'
Bundle 'tComment'
Bundle 'tpope/vim-surround'
Bundle 'Valloric/YouCompleteMe'
Bundle 'embear/vim-localvimrc'

" Version control nonsense.
Plugin 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'

" Appearance
Bundle 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Bundle 'bronson/vim-trailing-whitespace'

" File stuff/ things outside vim.
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-dispatch'
Bundle 'vim-scripts/a.vim'

call vundle#end()
filetype plugin indent on

"------------------------------------------------------------------------------"
" ----------------------  NON-VUNDLE SETTINGS  ------------------------------- "
"------------------------------------------------------------------------------"
""" Prefer XDG_CONFIG_HOME/XDG_CACHE_HOME
set runtimepath+=$XDG_CONFIG_HOME/vim/plugin,$XDG_CONFIG_HOME/vim/ftplugin
set backup backupdir=$XDG_CACHE_HOME/vim/backup,/tmp
set dir=$XDG_CACHE_HOME/vim/swap,/tmp
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo

""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8
set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd nofoldenable textwidth=80
set spell spelllang=en_us

let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tagbar#enabled = 1

let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim-go")

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
