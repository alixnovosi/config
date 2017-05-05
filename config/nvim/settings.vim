"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - andrewmichaud.com                                                     "
" FILE:    settings.vim                                                                           "
" PURPOSE: Settings used in (neo)vim.                                                             "
" UPDATED: 2017-05-05                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Unix file endings, no backups, no modelines, yes spelling, yes persistent undo history.
set fileformats=unix nobackup nomodeline spell spelllang=en_us undofile
scriptencoding utf-8

""" Use 4-wide spaces, no tabs.
set expandtab shiftwidth=4 softtabstop=4 tabstop=4

""" Allow reasonable folding. Wrap text at 99 columns, let movement spill over into next line.
set foldenable foldlevelstart=10 foldnestmax=5 foldmethod=syntax textwidth=99 whichwrap=[,],h,l,b,s

""" Color columns past textwidth.
""" Credit http://blog.hanschen.org/2012/10/24/different-background-color-in-vim-past-80-columns.
execute 'set colorcolumn=' . join(map(range(1,259,2), '"+" . v:val'), ',')

""" Highlight current line, no mode in status.
""" Ignore case in searching unless I use capital letters.
set cursorline noshowmode ignorecase smartcase

""" Use true color, set colorscheme.
set termguicolors background=dark
colorscheme gruvbox

set mouse=a

""" Use pipe cursor in insert mode.
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor

""" Aggressively force python 3 on everything, using a virtualenv for neovim.
let g:python = '/Users/amichaud/.virtualenvs/neovim/bin/python3'
let g:python3_host_prog = g:python
let g:ycm_python_binary_path = g:python
let g:ycm_server_python_interpreter = g:python

""" JSON conceal.
let g:vim_json_syntax_conceal = 1

""" Airline preferences.
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_extensions = ['branch', 'hunks', 'ale', 'tagbar', 'tabline', 'ycm']
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline_inactive_collapse = 1

""" Make YCM and UltiSnips work together via supertab.
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

""" Better key bindings for UltiSnipsExpandTrigger.
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

""" Make ack ag.
let g:ackprg = 'ag --vimgrep --smart-case'
cnoreabbrev ag Ack
cnoreabbrev aG Ack
cnoreabbrev Ag Ack
cnoreabbrev AG Ack

""" Let C-s and C-q go to Vim instead of terminal. We'll later set commands using that.
silent !stty -ixon > /dev/null 2>/dev/null

""" Fun with concealing.
set conceallevel=1
let g:hsoptions='+'

""" 'Learn vimscript the hard way' testbed - http://learnvimscripthehardway.stevelosh.com.
let g:maplocalleader = '_'
