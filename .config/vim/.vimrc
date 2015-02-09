set nocompatible

set runtimepath+=$XDG_CONFIG_HOME/vim/vim
filetype off

let g:vundle_lazy_load=0
set rtp+=$XDG_CONFIG_HOME/vim/vim/vundle/Vundle.vim
let bundles_dir = expand('$XDG_CONFIG_HOME/vim/vim/vundle')

call vundle#rc(bundles_dir)

""" Bundles.  Look them up on GitHub for more detail.
Bundle 'gmarik/Vundle.vim'

""" Language support/language-specific stuff.
Bundle 'elzr/vim-json'
Bundle 'derekwyatt/vim-scala'
Plugin 'fatih/vim-go'
Plugin 'smancill/conky-syntax.vim'
Bundle 'dag/vim2hs'
Bundle 'Twinside/vim-haskellFold'

""" Programming support.
Bundle 'scrooloose/syntastic'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'Valloric/YouCompleteMe'
Bundle 'tComment'
Bundle 'tpope/vim-surround'

""" Version control nonsense.
Bundle 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

""" Appearance.
Bundle 'altercation/vim-colors-solarized'
Bundle 'bronson/vim-trailing-whitespace'
Plugin 'bling/vim-airline'
Bundle 'edkolev/tmuxline.vim'

""" File stuff / things outside vim.
Bundle 'majutsushi/tagbar'
Bundle 'scrooloose/nerdtree'
Bundle 'vim-scripts/a.vim'

call vundle#end()
set rtp+=$XDG_CONFIG_HOME/vim/vim
filetype plugin indent on

""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8

""" Tmuxline and airline config.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ' '
let g:airline_right_sep = ' '
let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = {
    \'a'    : ['#S'],
    \'b'    : ['#H'],
    \'win'  : ['#I', '#W', '#F'],
    \'cwin' : ['#I', '#W', '#F'],
    \'z'    : ['%e %b %Y', '%A', '%H %M']}

let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim/vim-go")

""" Colorscheme
syntax enable
set background=dark
colorscheme solarized
hi clear SignColumn "fix vim-solarized breaking gitgutter.
" stop solarized bg from being non-transparent
hi Normal ctermbg=none


""" Enable mouse and word wrapping.
set mouse=a ww=[,],<,>,h,l,b,s

set textwidth=110 "TODO figure out a way to make this work across machines better.

""" Tabs work nicely, no modeline.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd

""" Code folding!
set nofoldenable foldmethod=indent foldnestmax=10 foldlevel=1

autocmd FileType tex noremap <F5> <Esc>:!pdflatex %<Cr><Cr>
autocmd FileType tex noremap <F6> <Esc>:!silent !evince %<.pdf >/dev/null 2>&1 &<Cr><Cr>

""" Throw everything in $XDG_CACHE_HOME or /tmp instead of the current directory..
set backup backupdir=$XDG_CACHE_HOME/vim/backup,.,/tmp
set dir=$XDG_CACHE_HOME/vim/swap,.,/tmp
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo

""" Thanks cstanfill
""" Faster escape if I ever remember to use the damn keybind.
inoremap jk <Esc>

""" Cycle buffers easier.
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

""" Move screen up and down without moving cursor.
nmap <C-j> <C-e>
nmap <C-k> <C-y>

"load plugins
set runtimepath+=$XDG_CONFIG_HOME/vim/vim/plugin

""" These cause the bottom of the screen to contain more useful information
set laststatus=2 ruler showcmd

"lets C-s and C-q be capture by vim instead of the terminal
silent !stty -ixon > /dev/null 2>/dev/null

" Also stolen from cstanfill.
nmap <C-s>v :TagbarToggle<CR>
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>h :set hlsearch!<CR>

""" Search settings.
set incsearch ignorecase smartcase nohlsearch

""" Automatically reload if someone changes a file on us.
set autoread

"""""""""""""""""""""""""""""
""" PER-FILETYPE NONSENSE """
"""""""""""""""""""""""""""""

""" Turn on spellcheck for some files automatically.
autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us

""" Autowrap text to 80 chars for certain filetypes
autocmd BufRead,BufNewFile *.md setlocal textwidth=80
