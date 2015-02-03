" Set up vundle
set nocompatible

set runtimepath+=$XDG_CONFIG_HOME/vim/vim
filetype off

let g:vundle_lazy_load=0
set rtp+=$XDG_CONFIG_HOME/vim/vim/vundle/Vundle.vim
let bundles_dir = expand('$XDG_CONFIG_HOME/vim/vim/vundle')

call vundle#rc(bundles_dir)

Bundle 'gmarik/Vundle.vim'

""" Vundle bundles.
" Language support.
Bundle 'elzr/vim-json'
Bundle 'derekwyatt/vim-scala'
Plugin 'fatih/vim-go'
Plugin 'smancill/conky-syntax.vim'
Bundle 'dag/vim2hs'

" Style.
Bundle 'altercation/vim-colors-solarized'
Bundle 'bronson/vim-trailing-whitespace'

" File stuff / things outside vim.
Bundle 'majutsushi/tagbar'
Bundle 'scrooloose/nerdtree'
Bundle 'vim-scripts/a.vim'

" Vim style and code support.
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
" Make buffers easy and fun.
Bundle 'fholgado/minibufexpl.vim'
" auto-completion magic
Bundle 'Valloric/YouCompleteMe'
Bundle 'tComment'
Plugin 'itchyny/lightline.vim'

call vundle#end()
set rtp+=$XDG_CONFIG_HOME/vim/vim
filetype plugin indent on

""" Nice backspacing.
set backspace=indent,eol,start
set encoding=utf-8 t_Co=256
scriptencoding utf-8

" Tmux nonsense.
if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif

let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/vim/vim-go")

""" Colorscheme
syntax enable
set background=dark
colorscheme solarized
hi Normal ctermbg=none

""" Enable mouse.
set mouse=a
""" word wrapping.
set ww=[,],<,>,h,l,b,s

set textwidth=110

set expandtab tabstop=4 softtabstop=4 shiftwidth=4
set nomodeline showcmd

set nofoldenable foldmethod=indent foldnestmax=10 foldlevel=1

autocmd FileType tex noremap <F5> <Esc>:!pdflatex %<Cr><Cr>
autocmd FileType tex noremap <F6> <Esc>:!silent !evince %<.pdf >/dev/null 2>&1 &<Cr><Cr>

""" Store cache files elsewhere.
""" Store backup files
set backup backupdir=$XDG_CACHE_HOME/vim/backup,.,/tmp
set dir=$XDG_CACHE_HOME/vim/swap,.,/tmp
""" Also viminfo.
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo

" stolen from cstanfill, here because I'm '''cool'''
"remap jk to escape for 3xtr4 l33t h4xx|ng
inoremap jk <Esc>

""more hjkl!!!
noremap <C-h> <Esc>:bp<Cr>
noremap <C-l> <Esc>:bn<Cr>

" MORE HJKL!!!!!!!!!
nmap <C-j> <C-e>
nmap <C-k> <C-y>

"load plugins
set runtimepath+=$XDG_CONFIG_HOME/vim/vim/plugin

""" These cause the bottom of the screen to contain more useful information
set laststatus=2 ruler showcmd

""" Lightline
let g:lightline = {
    \ 'colorscheme': 'solarized_dark',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
    \ },
    \ 'component': {
    \   'readonly': '%{&filetype=="help"?"":&readonly?"RO":""}',
    \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
    \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
    \ },
    \ 'component_visible_condition': {
    \   'readonly': '(&filetype!="help"&& &readonly)',
    \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
    \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
    \ },
    \ }

" stolen
let g:miniBufExplForceSyntaxEnable = 1

"lets C-s and C-q be capture by vim instead of the terminal
silent !stty -ixon > /dev/null 2>/dev/null

" Also stolen from cstanfill.
nmap <C-s>v :TagbarToggle<CR>
nmap <C-s>n :NERDTreeToggle<CR>
nmap <C-s>s :set number!<CR>
nmap <C-s>h :set hlsearch!<CR>
nmap <C-s>k :bd<CR>

""" Search settings.
set incsearch
set ignorecase
set smartcase
set nohlsearch

""" Automatically reload if someone changes a file on us.
set autoread

"""""""""""""""""""""""""""""
""" PER-FILETYPE NONSENSE """
"""""""""""""""""""""""""""""

""" Turn on spellcheck for some files automatically.
autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us

""" Autowrap text to 80 chars for certain filetypes
autocmd BufRead,BufNewFile *.md setlocal textwidth=80
