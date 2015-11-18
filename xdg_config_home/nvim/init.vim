"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim configuration file                                                            "
" UPDATED: 2015-11-18                                                                             "
" LICENSE: MIT/BSD                                                                                "
"-------------------------------------------------------------------------------------------------"

"-------------------------------------------------------------------------------------------------"
" ----------------------------------  NON-PLUGIN SETTINGS  -------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" nvim-/vim-specfic options  neovim sets some settings by default, so only set them for vim.
if !has("nvim")
    """ Be vim, load reloaded file, make mouse work, make backspace nice, utf-8 is great.
    set nocompatible autoread mouse=a backspace=indent,eol,start encoding=utf-8
    syntax enable

    """ Setting this breaks neovim for some reason I haven't figured out.
    set spell spelllang=en_us

    """ Make opening files not suck, make search better, be faster, indent.
    set wildmenu wildmode=list:longest,full incsearch ttyfast autoindent

    """ Use XDG dirs. Neovim does this by default. dir==swap
    set viminfo+=n$XDG_DATA_HOME/vim/viminfo dir=$XDG_DATA_HOME/vim/swap//
    set runtimepath+=$XDG_DATA_HOME/vim/site,$XDG_CONFIG_HOME/vim
    set backupdir=$XDG_DATA_HOME/vim/backup// undodir=$XDG_DATA_HOME/vim/undo//

    """ Syntax, and 256 colors!
    filetype plugin indent on
    set t_Co=256
else
    set backupdir=$XDG_DATA_HOME/nvim/backup//
endif

""" Windows-specific options.
if has("win32") || has("win16")
    """ Always always always prefer unix line endings.
    set fileformats=unix,dos
endif

""" Save backup files just in case. Save undo files for undo-history even if we close files.
set backup undofile
let g:netrw_home=$XDG_DATA_HOME.'/vim'

"-------------------------------------------------------------------------------------------------"
" --------------------------------------  PLUGINS  ---------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
call plug#begin('$XDG_DATA_HOME/nvim/site/plugins')

""" Language assistance.
Plug 'OmniSharp/omnisharp-vim', {'for': 'csharp'}
Plug 'OrangeT/vim-csharp',      {'for': 'csharp'}
Plug 'alecthomas/gometalinter', {'for': 'go'}
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'elzr/vim-json',           {'for': 'json'}
Plug 'rodjek/vim-puppet',       {'for': 'puppet'}
Plug 'klen/python-mode',        {'for': 'python'}
Plug 'derekwyatt/vim-scala',    {'for': 'scala'}
Plug 'tejr/vim-tmux',           {'for': 'tmux'}

""" Programming support.
Plug 'embear/vim-localvimrc'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tomtom/tcomment_vim'
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
""" Force utf-8.
scriptencoding utf-8

""" Don't use tabs or modeline, use 4-width spaces. Use 99-width text with a column one past that.
""" Enable folding, within reason.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4 nomodeline textwidth=99 colorcolumn=100
set foldenable foldlevelstart=10 foldnestmax=10 foldmethod=syntax

""" Dark solarized is the way to go.
""" Show the line we're editing, and be lazy because eh.
set background=dark
colorscheme solarized
set cursorline lazyredraw

""" Airline preferences.
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#eclim#enabled = 1
let g:airline_extensions = ["hunks", "syntastic", "tagbar", "tabline"]
let g:airlione_inactive_collapse=1

""" Attempts to get Eclim and Eclipse and Vim and YCM to play nicely.
let g:EclimCompletionMethod = 'omnifunc'

""" this mapping Enter key to <C-y> to chose the current highlight item
""" and close the selection list, same as other IDEs.
""" CONFLICT with some plugins like tpope/Endwise
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
set ttimeoutlen=50

""" Enable nice cursor wrapping, use 2h status for airline, show commands.
""" Don't show redundant mode, case search usefully.
set whichwrap=[,],h,l,b,s laststatus=2 showcmd noshowmode ignorecase smartcase

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  KEYBINDS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Fast escape.
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bprevious<Cr>
noremap <C-l> <Esc>:bnext<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

""" Let C-s and C-q go to Vim instead of terminal, and then define some commands using C-s.
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
