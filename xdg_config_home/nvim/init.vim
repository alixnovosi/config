"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    init.vim                                                                               "
" PURPOSE: (neo)vim configuration file                                                            "
" UPDATED: 2016-01-29                                                                             "
" LICENSE: MIT/BSD                                                                                "
"-------------------------------------------------------------------------------------------------"

"-------------------------------------------------------------------------------------------------"
" --------------------------------------  PLUGINS  ---------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
call plug#begin("$XDG_DATA_HOME/nvim/site/plugins")

""" Language assistance.
Plug 'vim-scripts/a.vim',                      {'for': ['c', 'cpp']}
Plug 'ap/vim-css-color',                       {'for': 'css'}
Plug 'OmniSharp/omnisharp-vim',                {'for': 'csharp'}
Plug 'OrangeT/vim-csharp',                     {'for': 'csharp'}
Plug 'alecthomas/gometalinter',                {'for': 'go'}
Plug 'fatih/vim-go',                           {'for': 'go'}
Plug 'burnettk/vim-angular',                   {'for': 'html'}
Plug 'matthewsimo/angular-vim-snippets',       {'for': 'html'}
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'Valloric/MatchTagAlways',                {'for': 'html'}
Plug 'pangloss/vim-javascript',                {'for': 'javascript'}
Plug 'ternjs/tern_for_vim',                    {'for': 'javascript'}
Plug 'elzr/vim-json',                          {'for': 'json'}
Plug 'rodjek/vim-puppet',                      {'for': 'puppet'}
Plug 'nvie/vim-flake8',                        {'for': 'python'}
Plug 'derekwyatt/vim-scala',                   {'for': 'scala'}
Plug 'keith/tmux.vim'

""" General programming support.
Plug 'embear/vim-localvimrc'
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Valloric/YouCompleteMe'

""" Version control nonsense.
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""" Appearance.
Plug 'bronson/vim-trailing-whitespace'
Plug 'flazz/vim-colorschemes'
Plug 'vim-airline/vim-airline'

""" File stuff/ things outside vim.
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-dispatch'
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': 'NERDTreeToggle'}
call plug#end()

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  SETTINGS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
set backupdir=$XDG_DATA_HOME/nvim/backup//

""" Force utf-8.
scriptencoding utf-8

""" Always always always prefer unix line endings.
set fileformats=unix

""" Use 4-wide space indents instead of tab characters. Don't use modeline.
""" Color columns past textwidth.
""" (Credit http://blog.hanschen.org/2012/10/24/different-background-color-in-vim-past-80-columns)
""" Enable folding, with reasonable settings.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4 nomodeline textwidth=99
execute "set colorcolumn=" . join(map(range(1,259,2), '"+" . v:val'), ',')
set foldenable foldlevelstart=10 foldnestmax=10 foldmethod=syntax

""" Dark solarized is the way to go.
""" Show the line we're editing, and be lazy because eh.
""" Save backup files just in case. Save undo files for undo-history even if we close files.
""" Enable english spelling.
set background=dark
colorscheme solarized
set cursorline lazyredraw backup undofile spell spelllang=en

""" Enable nice cursor wrapping, use 2h status for airline, show commands.
""" Airline handles mode for me. Use better (for me) search settings.
set whichwrap=[,],h,l,b,s laststatus=2 showcmd noshowmode ignorecase smartcase

""" Airline preferences.
let g:airline_left_sep = ""
let g:airline_right_sep = ""
let g:airline#extensions#tabline#left_sep = " "
let g:airline#extensions#tabline#right_sep = " "
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline_extensions = ["eclim", "hunks", "syntastic", "tagbar", "tabline"]
let g:airline_inactive_collapse = 1

""" Attempts to get Eclim and Eclipse and Vim and YCM to play nicely.
let g:EclimCompletionMethod = "omnifunc"

""" Copied from somewhere.
""" this mapping Enter key to <C-y> to chose the current highlight item and close the selection
""" list, same as other IDEs. CONFLICTS with some plugins like tpope/Endwise
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
set ttimeoutlen=50

"-------------------------------------------------------------------------------------------------"
" ---------------------------------------  KEYBINDS  -------------------------------------------- "
"-------------------------------------------------------------------------------------------------"
""" Use sudo after accessing file where sudo is needed, without having to reload.
cmap w!! w !sudo tee % >/dev/null

""" Fast escape.
inoremap jk <Esc>

""" Faster commands.
nnoremap ; :

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
nmap <C-s>nn :set nonumber norelativenumber<CR>
nmap <C-s>h :set hlsearch!<CR>
nmap <C-s>t :TagbarToggle<CR>
nmap <C-s>u :UndotreeToggle<CR>

""" Become a better person.
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>
