"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    config.vim                                                                             "
" PURPOSE: Vim configuration file                                                                 "
" UPDATED: 2015-09-21                                                                             "
" LICENSE: MIT/BSD                                                                                "
"-------------------------------------------------------------------------------------------------"

"-------------------------------------------------------------------------------------------------"
" ----------------------------------  NON-PLUGIN SETTINGS  -------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" nvim-/Windows-specific stuff.  neovim sets some settings by default, so only set them for vim.
if !has("nvim")
    """ Be vim, load reloaded file, make mouse work, make backspace nice, utf-8 is great.
    set nocompatible autoread mouse=a backspace=indent,eol,start encoding=utf-8
    """ Setting this breaks neovim for some reason I haven't figured out.
    set spell spelllang=en_us spellfile=$XDG_CACHE_HOME/vim/spell/en-utf-8.add
    """ Make opening files not suck, make search better, be faster.
    set wildmenu wildmode=list:longest,full incsearch ttyfast
    set autoindent
else
    set background=dark nohlsearch
endif

if has("win32") || has("win16")
    """ Always always always prefer unix line endings.
    set fileformats=unix,dos
endif

""" Syntax!
filetype plugin indent on
syntax enable

""" Prefer XDG_CONFIG_HOME/XDG_CACHE_HOME.
set runtimepath+=$XDG_CONFIG_HOME/vim viminfo+=n$XDG_DATA_HOME/vim/viminfo
set backup backupdir=$XDG_DATA_HOME/vim/backup dir=$XDG_DATA_HOME/vim/swap
set undodir=$XDG_DATA_HOME/vim/undo undofile undolevels=1000 undoreload=10000
let g:netrw_home=$XDG_CACHE_HOME.'/vim'

"-------------------------------------------------------------------------------------------------"
" --------------------------------------  PLUGINS  ---------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
call plug#begin('$XDG_CACHE_HOME/vim/plugins')

""" Language assistance.
Plug 'OmniSharp/omnisharp-vim',  {'for': 'csharp'}
Plug 'OrangeT/vim-csharp',       {'for': 'csharp'}
Plug 'alecthomas/gometalinter',  {'for': 'go'}
Plug 'pangloss/vim-javascript',  {'for': 'javascript'}
Plug 'elzr/vim-json',            {'for': 'json'}
Plug 'rodjek/vim-puppet',        {'for': 'puppet'}
Plug 'klen/python-mode',         {'for': 'python'}
Plug 'derekwyatt/vim-scala',     {'for': 'scala'}
Plug 'tejr/vim-tmux',            {'for': 'tmux'}

""" Programming support.
Plug 'embear/vim-localvimrc'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tComment'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-scripts/a.vim',           {'for': ['c', 'cpp']}

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'
Plug 'flazz/vim-colorschemes'

""" File stuff/ things outside vim.
Plug 'scrooloose/nerdtree',         {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}
call plug#end()

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  SETTINGS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Make backspacing reasonable, and force utf-8 and 256 colors.
set t_Co=256
scriptencoding utf-8

""" Personal preferences (space >> tabs, modelines scary, folds often annoying).
set et tabstop=4 softtabstop=4 shiftwidth=4 nomodeline textwidth=99 colorcolumn=100
set foldenable foldlevelstart=10 foldnestmax=10 foldmethod=syntax
colorscheme solarized

set cursorline
set lazyredraw

let g:EclimCompletionMethod = 'omnifunc'

""" Airline preferences.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#eclim#enabled = 1
let g:airline_extensions = ["hunks", "syntastic", "tagbar", "tabline"]
let g:airlione_inactive_collapse=1

" this mapping Enter key to <C-y> to chose the current highlight item
" and close the selection list, same as other IDEs.
" CONFLICT with some plugins like tpope/Endwise
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

""" Enable mouse and enable nice cursor wrapping, use 2h status for airline, show commands.
set whichwrap=[,],<,>,h,l,b,s laststatus=2 showcmd noshowmode ignorecase smartcase ttimeoutlen=50

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
