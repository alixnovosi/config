"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                             "
" FILE:    mappings.vim                                                                           "
" PURPOSE: Mappings used in (neo)vim.                                                             "
" UPDATED: 2016-03-11                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Use sudo after accessing file where sudo is needed, without having to reload.
cnoremap w!! w !sudo tee % >/dev/null

""" Fast escape.
inoremap jk <Esc>

""" Buffer cycling.
noremap <C-h> <Esc>:bprevious<Cr>
noremap <C-l> <Esc>:bnext<Cr>

""" Stuff.
nnoremap <C-s>n :NERDTreeToggle<CR>
nnoremap <C-s>s :set number!<CR>
nnoremap <C-s>r :set relativenumber!<CR>
nnoremap <C-s>nn :set nonumber norelativenumber<CR>
nnoremap <C-s>h :set hlsearch!<CR>
nnoremap <C-s>t :TagbarToggle<CR>
nnoremap <C-s>u :UndotreeToggle<CR>

""" Move screen up and down without moving cursor.
nnoremap <C-j> <C-e>
nnoremap <C-k> <C-y>

""" Faster commands.
nnoremap ; :

""" Stop mouse clicks from being inserted as text in edit mode.
inoremap <LeftMouse> <Nop>
inoremap <RightMouse> <Nop>

""" 'Learn vimscript the hard way' testbed - http://learnvimscripthehardway.stevelosh.com.
""" Move line downward/upward with one keystroke.
noremap - ddp
noremap _ dd2kp

onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>

""" Quote word.
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel

""" 'Become a better person' section
inoremap <esc> <nop>
