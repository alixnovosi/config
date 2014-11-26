" Set up vundle
set nocompatible

set runtimepath+=$XDG_CONFIG_HOME/vim/vim
filetype off

let g:vundle_lazy_load=0

if has("unix")
    let s:uname = system("uname -s")
    set rtp+=$XDG_CONFIG_HOME/vim/vim/common_bundles/Vundle.vim
    if s:uname ==? "Darwin\n"
        let bundles_dir = expand('$XDG_CONFIG_HOME/vim/vim/osx_bundles')
    else
        let bundles_dir = expand('$XDG_CONFIG_HOME/vim/vim/linux_bundles')
    endif
else
    """ Windows-only stuff, currently nothing.
endif

call vundle#rc(bundles_dir)

Bundle 'gmarik/Vundle.vim'

""" Vundle bundles.
" Better haskell in vim
"Bundle 'dag/vim2hs'
Bundle 'majutsushi/tagbar'
"Bundle 'scrooloose/nerdtree'
" Syntax checking.
"Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
" Sensical date increment/decrement
"Bundle 'tpope/vim-speeddating'
Bundle 'vim-scripts/a.vim'
" Make buffers easy and fun.
Bundle 'fholgado/minibufexpl.vim'
" auto-completion magic
Bundle 'Valloric/YouCompleteMe'
Bundle 'tComment'
" beautiful colors
Bundle 'altercation/vim-colors-solarized'
" trailing whitespace
Bundle 'bronson/vim-trailing-whitespace'
" go
Plugin 'fatih/vim-go'
" json
"Bundle 'elzr/vim-json'
" Conky syntax highlighting.
"Plugin 'smancill/conky-syntax.vim'
" Organization lists.
"Bundle 'jceb/vim-orgmode'
" scala
Bundle 'derekwyatt/vim-scala'
" Lint vim
"Bundle 'dbakker/vim-lint'

call vundle#end()
set rtp+=$XDG_CONFIG_HOME/vim/vim
filetype plugin indent on

""" Nice backspacing.
set backspace=indent,eol,start

set t_Co=256

""" OS-Dependent stuff that has to happen after bundles load.
if has("unix")
    let s:uname = system("uname -s")
    if s:uname ==? "Darwin\n"
        """ Put vim-go stuff in a reasonable place.
        let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/mac-vim-go")
    else
        let g:go_bin_path = expand("$XDG_CONFIG_HOME/vim/linux-vim-go")
    endif
else
    """ Windows-only stuff, currently nothing.
endif

""" Colorscheme
syntax enable
set background=dark
colorscheme solarized
hi Normal ctermbg=none

""" These cause the bottom of the screen to contain more useful information
set laststatus=2 ruler showcmd

""" Enable mouse.
set mouse=a
""" word wrapping.
set ww=[,],<,>,h,l,b,s

set textwidth=130

""" tabs
set expandtab     "use spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set nomodeline      "don't show modeline
set showcmd       "show command as it is typed

""" Folding?
set nofoldenable "fold by default"
set foldmethod=indent "fold based on indentation"
set foldnestmax=10 "deepest fold is 10 levels."
set foldlevel=1

autocmd FileType tex noremap <F5> <Esc>:!pdflatex %<Cr><Cr>
autocmd FileType tex noremap <F6> <Esc>:!silent !evince %<.pdf >/dev/null 2>&1 &<Cr><Cr>

""" Store cache files elsewhere.
""" Store backup files
set backup
set dir=$XDG_CACHE_HOME/vim/swap,.,/tmp
set backupdir=$XDG_CACHE_HOME/vim/backup,.,/tmp
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

" Config status line.
" filename
set statusline =%#identifier#
set statusline+=[%t]

" line ending warning
set statusline+=%#warningmsg#
set statusline+=%{&ff!='unix'?'['.&ff.']':''}

" filetype
set statusline+=%#identifier#
set statusline+=%y

" modified flag
set statusline+=%m

" git branch
set statusline+=%{fugitive#statusline()}
"
set statusline+=%=
set statusline+=%c\
set statusline+=%l/%L
set statusline+=\ %P

hi StatusLine ctermfg=4

" stolen
let g:miniBufExplForceSyntaxEnable = 1
let g:EclimCompletionMethod = 'omnifunc'
let g:EclimIvyClasspathUpdate = 0

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
