
" Set up vundle
set nocompatible
filetype off

set rtp+=$XDG_CONFIG_HOME/vim/vim/bundle/Vundle.vim
call vundle#begin("/Users/amichaud/.config/vim/vim/bundle")

Bundle 'gmarik/Vundle.vim'

""" Vundle bundles.
Bundle 'majutsushi/tagbar'
Bundle 'mantiz/vim-plugin-dirsettings'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/a.vim'
Bundle 'argtextobj.vim'
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
" indentation marking
Bundle 'Yggdroot/indentLine'

call vundle#end()
filetype plugin indent on

let g:indentLine_char='│'

""" Nice backspacing.
set backspace=indent,eol,start

""" Setup term color support.
if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
    set t_Co=256
endif

""" Colorscheme
syntax enable
set background=dark
colorscheme solarized

""" Show trailing whitespace.
set nolist
"set listchars=tab:\|\ ,trail:·,extends:#,nbsp:.

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
set modeline      "show modeline
set showcmd       "show command as it is typed

""" Folding?
set nofoldenable "fold by default"
set foldmethod=indent "fold based on indentation"
set foldnestmax=10 "deepest fold is 10 levels."
set foldlevel=1

""" Store cache files elsewhere.
set directory=$XDG_CACHE_HOME/vim,.,/tmp
set backupdir=$XDG_CACHE_HOME/vim,.,/tmp

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

""" STATUSLINE CONFIGURATION
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

"let g:miniBufExplForceSyntaxEnable = 1

"lets C-s and C-q be capture by vim instead of the terminal
silent !stty -ixon > /dev/null 2>/dev/null

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

""""""""""""""""""""""""""""""""
""" PER-ENVIRONMENT NONSENSE """
""""""""""""""""""""""""""""""""
autocmd BufNewFile,BufRead /mnt/home/amichaud/courses/cs134/dai_gurren/* set noexpandtab shiftwidth=8 tabstop=8

""" auto remove whitespace on buffer save
"autocmd! BufWrite * mark ' | silent! %s/\s\+$// | norm ''

