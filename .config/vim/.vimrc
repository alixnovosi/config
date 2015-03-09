set nocompatible

set runtimepath+=$XDG_CONFIG_HOME/vim
filetype off

let g:vundle_lazy_load=0
set rtp+=$XDG_CONFIG_HOME/vim/vundle/Vundle.vim
let g:bundles_dir = expand('$XDG_CONFIG_HOME/vim/vundle')

call vundle#rc(bundles_dir)

""" Bundles.  Look them up on GitHub for more detail.
Bundle 'gmarik/Vundle.vim'

""" Language assistance.
Bundle 'OmniSharp/omnisharp-vim'
Bundle 'OrangeT/vim-csharp'
Bundle 'elzr/vim-json'
Plugin 'fatih/vim-go'
Bundle 'dag/vim2hs'
Bundle 'Twinside/vim-haskellFold'
Bundle 'derekwyatt/vim-scala'
Plugin 'tejr/vim-tmux'

""" Programming support.
Bundle 'fholgado/minibufexpl.vim'
Bundle 'scrooloose/syntastic'
Bundle 'tComment'
Bundle 'tpope/vim-surround'
Bundle 'Valloric/YouCompleteMe'

""" Version control nonsense.
Plugin 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'

""" Appearance.
Bundle 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Bundle 'bronson/vim-trailing-whitespace'

""" File stuff / things outside vim.
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-dispatch'
Bundle 'vim-scripts/a.vim'

call vundle#end()
filetype plugin indent on

""" Spellcheck.
set spell spelllang=en_us

""" Make backspacing reasonable, and force utf-8 and 256 colors.
set backspace=indent,eol,start encoding=utf-8 t_Co=256
scriptencoding utf-8

""" Tmuxline and airline config.
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ' '
let g:airline_right_sep = ' '

let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim-go")

""" Colorscheme
syntax enable
set background=dark
colorscheme solarized
""" fix vim-solarized breaking gitgutter
hi clear SignColumn
" stop solarized bg from being non-transparent
hi Normal ctermbg=none


""" Enable mouse and word wrapping.
set mouse=a ww=[,],<,>,h,l,b,s

set textwidth=80

""" Tabs work nicely, no modeline.
set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd

""" Code folding!
set nofoldenable foldmethod=syntax foldnestmax=10 foldlevel=1

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
""" Autowrap text to 80 chars for certain filetypes
autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us textwidth=80
