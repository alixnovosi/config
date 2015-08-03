"------------------------------------------------------------------------------"
" AUTHOR: Andrew Michaud                                                       "
" FILE: .vimrc                                                                 "
" PURPOSE: Vim configuration file                                              "
" DATE: 2015-07-23                                                             "
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

call vundle#rc(bundles_dir)

"------------------------------------------------------------------------------"
" ---------------------------  VUNDLE BUNDLES  ------------------------------- "
"------------------------------------------------------------------------------"
Bundle 'gmarik/Vundle.vim'

""" Language assistance.
""" C#
Bundle 'OmniSharp/omnisharp-vim'
Bundle 'OrangeT/vim-csharp'

""" Haskell
Bundle 'dag/vim2hs'
Bundle 'Twinside/vim-haskellFold'

""" HTML5
Plugin 'othree/html5.vim'

""" Javascript
Plugin 'pangloss/vim-javascript'

""" JSON
Bundle 'elzr/vim-json'

""" Golang
Plugin 'fatih/vim-go'
Plugin 'nsf/gocode', {'rtp': 'vim/'}

""" Python
Plugin 'tmhedberg/SimpylFold'
Plugin 'davidhalter/jedi'

""" Scala
Bundle 'derekwyatt/vim-scala'

""" Tmux
Plugin 'tejr/vim-tmux'

""" Vimscript
Bundle 'dbakker/vim-lint'

""" Programming support.
Plugin 'ciaranm/securemodelines'
Bundle 'scrooloose/syntastic'
Bundle 'tComment'
Bundle 'tpope/vim-surround'
Bundle 'Valloric/YouCompleteMe'
Bundle 'embear/vim-localvimrc'

""" Version control nonsense.
Plugin 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'

""" Appearance.
Bundle 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Bundle 'bronson/vim-trailing-whitespace'

""" File stuff / things outside vim.
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'techlivezheng/vim-plugin-minibufexpl'
Bundle 'tpope/vim-dispatch'
Bundle 'vim-scripts/a.vim'

call vundle#end()
filetype plugin indent on

"------------------------------------------------------------------------------"
" ----------------------  NON-VUNDLE SETTINGS  ------------------------------- "
"------------------------------------------------------------------------------"
""" Load plugins.
set runtimepath+=$XDG_CONFIG_HOME/vim/plugin,$XDG_CONFIG_HOME/vim/ftplugin

""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8

""" Manually enable html5 checking.
let g:syntastic_html_tidy_exec = '/usr/bin/tidy5'

""" Airline config.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''

set spell spelllang=en_us
let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim-go")

syntax enable
set background=dark
colorscheme solarized

""" Enable mouse and word wrapping.
set mouse=a ww=[,],<,>,h,l,b,s

""" Set some personal coding preferences.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd
set nofoldenable
set textwidth=80

""" Prefer $XDG_CACHE_HOME/vim for swap and backup files.
set backup backupdir=$XDG_CACHE_HOME/vim/backup,/tmp
set dir=$XDG_CACHE_HOME/vim/swap,/tmp
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo

"------------------------------------------------------------------------------"
" -------------------------------  KEYBINDS  --------------------------------- "
"------------------------------------------------------------------------------"
""" Thanks cstanfill!
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

""" These cause the bottom of the screen to contain more useful information.
set laststatus=2 ruler showcmd

""" Let C-s, C-q go to Vim instead of terminal.
silent !stty -ixon > /dev/null 2>/dev/null

""" Also stolen from cstanfill.
""" Filesystem tree, numbering, highlighting search toggle.
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>h :set hlsearch!<CR>

""" Set nicer search settings.
set incsearch ignorecase smartcase nohlsearch

""" Automatically reload if someone changes a file on us.
set autoread
